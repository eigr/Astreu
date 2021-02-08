defmodule Astreu.Consumer.Service do
  use GRPC.Server, service: Astreu.Consumer.Subscriber.Service
  require Logger
  alias GRPC.Server
  alias Astreu.SubscriberManager

  @spec subscribe(Astreu.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Astreu.Protocol.Message.t()
  def subscribe(message_stream, stream) do
    Logger.debug("Received request #{inspect(message_stream)}")

    Enum.each(message_stream, fn message ->
      Logger.info("Decode request from #{inspect(message)}")

      case message.data do
        {:system, _} -> stream |> handle_system(message.data)
        {:exchange, _} -> stream |> handle_exchange(message.data)
        {:ack, _} -> stream |> handle_ack(message.data)
        _ -> stream |> handle_invalid(message.data)
      end
    end)
  end

  @spec unsubscribe(Astreu.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Google.Protobuf.Empty.t()
  def unsubscribe(message, _stream) do
    Logger.debug("Received request #{inspect(message)}")
  end

  defp handle_system(stream, data) do
    system = elem(data, 1)

    case system.data do
      {:connect, _} -> stream |> handle_connect(system.data)
      {:disconnect, _} -> stream |> handle_disconnect(system.data)
      {:info, _} -> stream |> handle_info(system.data)
      {:failure, _} -> stream |> handle_failure(system.data)
      _ -> stream |> handle_invalid(data)
    end
  end

  defp handle_connect(stream, data) do
    # TODO lookup before register new process
    Logger.debug("Connect subscriber #{inspect(data)}")
    connection = elem(data, 1)

    state = %{
      name: "#{connection.topic}:#{connection.subscription}",
      topic: connection.topic,
      subscriber: connection.subscription,
      stream: stream,
      uuid: connection.uuid,
      properties: connection.properties,
      timestamp: connection.timestamp
    }

    Horde.DynamicSupervisor.start_child(
      Astreu.SubscriberSupervisor,
      {Astreu.SubscriberManager, state}
    )

    SubscriberManager.subscribe(state[:name])
  end

  defp handle_disconnect(stream, data) do
    data
  end

  defp handle_info(stream, data) do
    data
  end

  defp handle_failure(stream, data) do
    data
  end

  defp handle_exchange(stream, data) do
    data
  end

  defp handle_ack(stream, data) do
    data
  end

  defp handle_invalid(stream, data) do
    Server.send_reply(
      stream,
      Astreu.Protocol.Message.new(data: {:exchange, Astreu.Protocol.Exchange.new()})
    )
  end
end
