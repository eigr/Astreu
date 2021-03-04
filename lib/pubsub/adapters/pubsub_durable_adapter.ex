defmodule Astreu.Producer.PubSubDurableAdapter do
  @behaviour Astreu.Producer.PubSubAdapter

  @impl Astreu.Producer.PubSubAdapter
  def init(_options) do
    %{}
  end

  @impl Astreu.Producer.PubSubAdapter
  def publish(_topic, _message) do
    :ok
  end
end
