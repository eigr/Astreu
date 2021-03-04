defmodule Astreu.Core.Protocol.Consumer.Service do
  use GRPC.Server, service: Astreu.Core.Protocol.Consumer.Subscriber.Service
  require Logger
  alias Astreu.Core.Protocol.ProtocolBehaviour, as: Protocol

  @spec unsubscribe(Astreu.Core.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Google.Protobuf.Empty.t()
  def unsubscribe(message, _stream) do
    Logger.debug("Received request #{inspect(message)}")
  end

  @spec subscribe(Astreu.Core.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Astreu.Core.Protocol.Message.t()
  def subscribe(message_stream, stream) do
    Logger.debug("Received request #{inspect(message_stream)}")

    Enum.each(message_stream, fn message ->
      Logger.info("Decode request from #{inspect(message)}")
      handle_message(stream, message)
    end)
  end

  defp handle_message(stream, message) do
    params = %{stream: stream, message: message, consumer: true, producer: false}

    with {:ok, _} <- Protocol.ensure_metadata(params) do
      Protocol.handle(params)
    else
      {:error, reason} ->
        Protocol.handle_invalid(reason, params)
    end
  end
end
