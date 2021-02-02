defmodule Astreu.Producer.PubSubTransientAdapter do
  @behaviour Astreu.Producer.Adapter

  alias Phoenix.PubSub

  @imple true
  def init(_options) do
    {Phoenix.PubSub, name: Astreu.PubSub}
  end

  @imple true
  def publish(topic, message), do: PubSub.broadcast(Astreu.PubSub, topic, {:enqueue, message})
end
