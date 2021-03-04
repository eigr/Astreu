defmodule Astreu.Producer.PubSubTransientAdapter do
  @behaviour Astreu.Producer.PubSubAdapter

  alias Phoenix.PubSub

  @impl Astreu.Producer.PubSubAdapter
  def init(_options) do
    {Phoenix.PubSub, name: Astreu.PubSub}
  end

  @impl Astreu.Producer.PubSubAdapter
  def publish(topic, message), do: PubSub.broadcast(Astreu.PubSub, topic, {:enqueue, message})
end
