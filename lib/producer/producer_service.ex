defmodule Astreu.Producer.Service do
  use GRPC.Server, service: Astreu.Producer.Publisher.Service
  require Logger
  alias GRPC.Server

  @spec publish(Astreu.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Astreu.Protocol.AckMessage.t()
  def publish(message_stream, _stream) do
    Logger.debug("Received request #{inspect(message_stream)}")
  end
end
