defmodule Astreu.Producer.PubSubDurableAdapter do
  @behaviour Astreu.Producer.PubSubAdapter

  @imple true
  def init(_options) do
    %{}
  end

  @imple true
  def publish(_topic, _message) do
    :ok
  end
end
