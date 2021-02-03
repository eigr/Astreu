defmodule Astreu.Consumer.Service do
  use GRPC.Server, service: Astreu.Consumer.Subscriber.Service
  require Logger
  alias GRPC.Server

  @spec subscribe(Astreu.Protocol.Payload.t(), GRPC.Server.Stream.t()) ::
          Astreu.Protocol.Payload.t()
  def subscribe(payload_stream, stream) do
    Logger.debug("Received request #{inspect(payload_stream)}")

    Enum.each(payload_stream, fn payload ->
      Logger.info("Decode request from #{inspect(payload)}")
      Server.send_reply(stream, Astreu.Protocol.Payload.new())
    end)
  end

  @spec unsubscribe(Astreu.Protocol.Payload.t(), GRPC.Server.Stream.t()) ::
          Google.Protobuf.Empty.t()
  def unsubscribe(payload, _stream) do
    Logger.debug("Received request #{inspect(payload)}")
  end
end
