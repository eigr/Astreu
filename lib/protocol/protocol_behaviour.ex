defmodule Astreu.ProtocolBehaviour do
  require Logger
  alias GRPC.Server
  alias Astreu.SubscriberManager

  @spec ensure_metadata(Astreu.Protocol.Message.t()) ::
          {:ok, Astreu.Protocol.Message.t()} | {:error, String.t()}
  def ensure_metadata(message) do
    case message.data do
      {:system, _} -> ensure_system(message)
      {:exchange, _} -> ensure_exchange(message)
      {:ack, _} -> ensure_ack(message)
      _ -> {:error, "Invalid message: #{inspect(message)}"}
    end
  end

  def handle_system(stream, data) do
    system = elem(data, 1)

    case system.data do
      {:connect, _} -> stream |> handle_connect(system.data)
      {:disconnect, _} -> stream |> handle_disconnect(system.data)
      {:info, _} -> stream |> handle_info(system.data)
      {:failure, _} -> stream |> handle_failure(system.data)
      _ -> stream |> handle_invalid(data)
    end
  end

  def handle_exchange(stream, data) do
    data
  end

  def handle_ack(stream, data) do
    data
  end

  def handle_invalid(stream, data) do
    Server.send_reply(
      stream,
      Astreu.Protocol.Message.new(data: {:exchange, Astreu.Protocol.Exchange.new()})
    )
  end

  def handle_invalid(stream, data, reason) do
    Server.send_reply(
      stream,
      Astreu.Protocol.Message.new(data: {:exchange, Astreu.Protocol.Exchange.new()})
    )
  end

  # Validations
  defp ensure_system(message) do
    {:ok, message}
  end

  defp ensure_exchange(message) do
    {:ok, message}
  end

  defp ensure_ack(message) do
    {:ok, message}
  end

  # handle messages
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
end
