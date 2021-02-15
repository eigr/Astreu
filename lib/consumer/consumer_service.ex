defmodule Astreu.Consumer.Service do
  use GRPC.Server, service: Astreu.Consumer.Subscriber.Service
  require Logger
  alias Astreu.ProtocolBehaviour, as: Protocol

  @spec subscribe(Astreu.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Astreu.Protocol.Message.t()
  def subscribe(message_stream, stream) do
    Logger.debug("Received request #{inspect(message_stream)}")

    Enum.each(message_stream, fn message ->
      Logger.info("Decode request from #{inspect(message)}")
      handle_message(stream, message)
    end)
  end

  @spec unsubscribe(Astreu.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Google.Protobuf.Empty.t()
  def unsubscribe(message, _stream) do
    Logger.debug("Received request #{inspect(message)}")
  end

  defp handle_message(stream, message) do
    params = %{message: message, consumer: true, producer: false}

    with {:ok, msg} <- Protocol.ensure_metadata(params) do
      case msg.data do
        {:system, _} -> Protocol.handle_system(stream, msg.data)
        {:exchange, _} -> Protocol.handle_exchange(stream, msg.data)
        {:ack, _} -> Protocol.handle_ack(stream, msg.data)
        _ -> Protocol.handle_invalid(stream, msg.data)
      end
    else
      {:error, reason} ->
        Protocol.handle_invalid(stream, message.data, reason)
    end
  end
end
