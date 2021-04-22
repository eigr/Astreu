defmodule Astreu.PubSub.Adapter.TransientAdapter do
  @behaviour Astreu.PubSub.Adapter

  alias Phoenix.PubSub

  @impl Astreu.PubSub.Adapter
  def init(_options) do
    {Phoenix.PubSub, name: Astreu.PubSub}
  end

  @impl Astreu.PubSub.Adapter
  def publish(topic, message), do: PubSub.broadcast(Astreu.PubSub, topic, {:enqueue, message})
end
