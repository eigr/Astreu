defmodule Astreu.Core.Protocol.Producer.Service do
  use GRPC.Server, service: Astreu.Core.Protocol.Producer.Publisher.Service
  require Logger
  alias GRPC.Server
  alias Astreu.Core.Protocol.ProtocolBehaviour, as: Protocol

  @spec publish(Astreu.Core.Protocol.Message.t(), GRPC.Server.Stream.t()) ::
          Astreu.Core.Protocol.Message.t()
  def publish(message_stream, stream) do
    Logger.debug("Received publisher request #{inspect(message_stream)}")

    Enum.each(message_stream, fn message ->
      handle_message(stream, message)
    end)
  end

  defp handle_message(stream, message) do
    params = %{stream: stream, message: message, consumer: false, producer: true}

    with {:ok, msg} <- Protocol.ensure_metadata(params) do
      Protocol.handle(params)
    else
      {:error, reason} ->
        Protocol.handle_invalid(reason, params)
    end
  end
end
