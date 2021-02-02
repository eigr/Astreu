defmodule Astreu.Consumer.Service do
  use GRPC.Server, service: Astreu.Consumer.Subscriber.Service
  require Logger
  alias GRPC.Server

  @spec subscribe(Astreu.Consumer.Info.t(), GRPC.Server.Stream.t()) ::
          Astreu.Protocol.Message.t()
  def subscribe(info_stream, stream) do
    Logger.debug("Received request #{inspect(info_stream)}")

    Enum.each(info_stream, fn info ->
      Logger.info("Decode request from #{inspect(info)}")
      Server.send_reply(stream, Astreu.Protocol.Message.new())
    end)
  end

  @spec unsubscribe(Astreu.Consumer.Info.t(), GRPC.Server.Stream.t()) ::
          Google.Protobuf.Empty.t()
  def unsubscribe(info, _stream) do
    Logger.debug("Received request #{inspect(info)}")
  end
end
