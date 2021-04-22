defmodule GRPCPrometheus.ClientInterceptor do
  use Prometheus.Metric

  @behaviour GRPC.ClientInterceptor

  @labels [:grpc_service, :grpc_method, :grpc_type]
  @labels_with_code [:grpc_code | @labels]

  require Prometheus.Contrib.HTTP

  use Prometheus.Config,
    latency: false,
    histogram_buckets: [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10],
    registry: :default

  def setup do
    latency = Config.latency(__MODULE__)
    histogram_buckets = Config.histogram_buckets(__MODULE__)
    registry = Config.registry(__MODULE__)

    Counter.declare(
      name: :grpc_client_started_total,
      labels: @labels,
      help: "Total number of RPCs started on the client.",
      registry: registry
    )

    Counter.declare(
      name: :grpc_client_msg_received_total,
      labels: @labels,
      help: "Total number of RPC stream messages received on the client.",
      registry: registry
    )

    Counter.declare(
      name: :grpc_client_msg_sent_total,
      labels: @labels,
      help: "Total number of gRPC stream messages sent by the client.",
      registry: registry
    )

    Counter.declare(
      name: :grpc_client_handled_total,
      labels: @labels_with_code,
      help: "Total number of RPCs completed on the client, regardless of success or failure.",
      registry: registry
    )

    case latency do
      :histogram ->
        Histogram.declare(
          name: :grpc_client_handled_latency_seconds,
          help: "Histogram of response latency of rpcs handled by the client, in seconds.",
          labels: @labels_with_code,
          buckets: histogram_buckets,
          registry: registry
        )

      :summary ->
        Summary.declare(
          name: :grpc_client_handled_latency_seconds,
          help: "Summary of response latency of rpcs handled by the client, in seconds.",
          labels: @labels_with_code,
          registry: registry
        )

      _ ->
        :ok
    end
  end

  def init(_), do: []

  def call(%{grpc_type: grpc_type, __interface__: interface} = stream, req, next, _) do
    registry = Config.registry(__MODULE__)
    latency = Config.latency(__MODULE__)
    labels = [stream.service_name, stream.method_name, grpc_type]
    Counter.inc(registry: registry, name: :grpc_client_started_total, labels: labels)

    start = if latency, do: System.monotonic_time()

    send_request =
      if grpc_type == :client_stream || grpc_type == :bidi_stream do
        fn s, r, opts ->
          s = interface[:send_request].(s, r, opts)
          Counter.inc(registry: registry, name: :grpc_client_msg_sent_total, labels: labels)
          s
        end
      else
        interface[:send_request]
      end

    monitor = {registry, labels, latency, start}

    recv =
      if stream.server_stream do
        stream_recv(monitor, stream)
      else
        interface[:recv]
      end

    interface =
      interface
      |> Map.put(:send_request, send_request)
      |> Map.put(:recv, recv)

    result = next.(%{stream | __interface__: interface}, req)

    if grpc_type == :unary do
      code =
        if elem(result, 0) == :ok do
          GRPC.Status.code_name(0)
        else
          {:error, error} = result
          GRPC.Status.code_name(error.status)
        end

      handled_rpc(monitor, code)
    end

    result
  end

  defp stream_recv(monitor, %{__interface__: interface}) do
    fn s, opts ->
      result = interface[:recv].(s, opts)

      if elem(result, 0) == :ok do
        new_enum = new_res_enum(monitor, elem(result, 1))
        put_elem(result, 1, new_enum)
      else
        {:error, error} = result
        handled_rpc(monitor, GRPC.Status.code_name(error.status))
        result
      end
    end
  end

  defp new_res_enum(monitor, enum) do
    Stream.transform(
      enum,
      fn -> :ok end,
      fn elem, acc ->
        msg_received(monitor)
        {[elem], acc}
      end,
      fn _ ->
        handled_rpc(monitor, GRPC.Status.code_name(0))
      end
    )
  end

  def msg_received({registry, labels, _, _}) do
    Counter.inc(registry: registry, name: :grpc_client_msg_received_total, labels: labels)
  end

  def handled_rpc({registry, labels, latency, start}, code) do
    labels_with_code = [code | labels]
    Counter.inc(name: :grpc_client_handled_total, labels: labels_with_code)
    track_time(registry, latency, start, labels_with_code)
  end

  defp track_time(registry, latency, start, labels_with_code) do
    stop = if latency, do: System.monotonic_time()

    case latency do
      :histogram ->
        diff = System.convert_time_unit(stop - start, :native, :second)

        Histogram.observe(
          [
            registry: registry,
            name: :grpc_client_handled_latency_seconds,
            labels: labels_with_code
          ],
          diff
        )

      :summary ->
        diff = System.convert_time_unit(stop - start, :native, :second)

        Summary.observe(
          [
            registry: registry,
            name: :grpc_client_handled_latency_seconds,
            labels: labels_with_code
          ],
          diff
        )

      _ ->
        :ok
    end
  end
end
