defmodule Astreu.PubSub.Adapter.DurableAdapter do
  @behaviour Astreu.PubSub.Adapter

  @impl Astreu.PubSub.Adapter
  def init(_options) do
    %{}
  end

  @impl Astreu.PubSub.Adapter
  def publish(_topic, _message) do
    :ok
  end
end
