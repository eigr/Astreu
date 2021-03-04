defmodule Astreu.Producer.Dispatcher do
  use GenServer
  require Logger

  @pubsub Application.get_env(:astreu, :producer_adapter)

  @impl true
  def init(state) do
    {:ok, state}
  end

  def child_spec(state) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [state]}
    }
  end

  def start_link(state \\ []) do
    Logger.info("Starting Dispatcher")
    GenServer.start_link(__MODULE__, state, name: __MODULE__)
  end

  @impl true
  def handle_cast({:publish, topic, message}, state) do
    @pubsub.publish(topic, message)
    {:noreply, state}
  end

  def dispatch(topic, message) do
    GenServer.cast(__MODULE__, {:publish, topic, message})
  end
end
