defmodule Astreu.Consumer.Service do
  use GRPC.Server, service: Astreu.Consumer.Subscriber.Service
  require Logger
  alias GRPC.Server

  @spec subscribe(Astreu.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Astreu.Protocol.Message.t()
  def subscribe(message_stream, stream) do
    Logger.debug("Received request #{inspect(message_stream)}")

    Enum.each(message_stream, fn message ->
      Logger.info("Decode request from #{inspect(message)}")

      Server.send_reply(
        stream,
        Astreu.Protocol.Message.new(data: {:exchange, Astreu.Protocol.Exchange.new()})
      )
    end)
  end

  @spec unsubscribe(Astreu.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Google.Protobuf.Empty.t()
  def unsubscribe(message, _stream) do
    Logger.debug("Received request #{inspect(message)}")
  end
end
