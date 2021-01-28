defmodule Astreu.Consumer.Service do
  use GRPC.Server, service: Astreu.Consumer.Subscriber.Service
  require Logger
  alias GRPC.Server

  @spec subscribe(Astreu.Consumer.Info.t(), GRPC.Server.Stream.t()) ::
          Astreu.Protocol.Message.t()
  def subscribe(info_stream, _stream) do
    Logger.debug("Received request #{inspect(info_stream)}")
  end

  @spec unsubscribe(Astreu.Consumer.Info.t(), GRPC.Server.Stream.t()) ::
          Google.Protobuf.Empty.t()
  def unsubscribe(info, _stream) do
    Logger.debug("Received request #{inspect(info)}")
  end
end
