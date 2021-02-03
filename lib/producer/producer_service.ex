defmodule Astreu.Producer.Service do
  use GRPC.Server, service: Astreu.Producer.Publisher.Service
  require Logger
  alias GRPC.Server

  @spec publish(Astreu.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Astreu.Protocol.Message.t()
  def publish(message_stream, stream) do
    Logger.debug("Received publisher request #{inspect(message_stream)}")

    Enum.each(message_stream, fn message ->
      case Astreu.Producer.Dispatcher.dispatch(message) do
        # ACK with success
        :ok -> Server.send_reply(stream, Astreu.Protocol.Message.new())
        # ACK without success
        _ -> Server.send_reply(stream, Astreu.Protocol.Message.new())
      end
    end)
  end
end
