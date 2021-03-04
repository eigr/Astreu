defmodule Astreu.Producer.PubSubAdapter do
  @doc """
  Defines Publish/Subscriber behaviours
  """

  @type topic :: binary
  @type message :: Astreu.Protocol.Message.t()
  @type options :: Astreu.Consumer.Options.t()

  @doc """
  Initialize adapter.
  """
  @callback init(type :: any()) :: type :: any()

  @doc """
  Publish a message to the topic.
  """
  @callback publish(topic, message) :: :ok | {:error, term}
end
