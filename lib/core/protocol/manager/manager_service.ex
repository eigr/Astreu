defmodule Astreu.Core.Protocol.Manager.Service do
  use GRPC.Server, service: Astreu.Core.Protocol.Manager.TopicService.Service
  require Logger
  alias GRPC.Server

  @spec info(Astreu.Core.Protocol.Manager.Topic.t(), GRPC.Server.Stream.t()) ::
          Astreu.Core.Protocol.Manager.TopicInfo.t()
  def info(topic, _stream) do
    Logger.debug("Received request #{inspect(topic)}")
  end

  @spec drop(Astreu.Core.Protocol.Manager.Topic.t(), GRPC.Server.Stream.t()) ::
          Google.Protobuf.Empty.t()
  def drop(topic, _stream) do
    Logger.debug("Received request #{inspect(topic)}")
  end

  @spec create(Astreu.Core.Protocol.Manager.Topic.t(), GRPC.Server.Stream.t()) ::
          Astreu.Core.Protocol.Manager.TopicInfo.t()
  def create(topic, _stream) do
    Logger.debug("Received request #{inspect(topic)}")
  end
end
