defmodule Astreu.Manager.Service do
  use GRPC.Server, service: Astreu.Manager.TopicService.Service
  require Logger
  alias GRPC.Server

  @spec info(Astreu.Manager.Topic.t(), GRPC.Server.Stream.t()) ::
          Astreu.Manager.TopicInfo.t()
  def info(topic, _stream) do
    Logger.debug("Received request #{inspect(topic)}")
  end

  @spec drop(Astreu.Manager.Topic.t(), GRPC.Server.Stream.t()) ::
          Google.Protobuf.Empty.t()
  def drop(topic, _stream) do
    Logger.debug("Received request #{inspect(topic)}")
  end

  @spec create(Astreu.Manager.Topic.t(), GRPC.Server.Stream.t()) ::
          Astreu.Manager.TopicInfo.t()
  def create(topic, _stream) do
    Logger.debug("Received request #{inspect(topic)}")
  end
end
