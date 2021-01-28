defmodule Astreu.TopicManager do
  use GenServer
  require Logger

  @doc """
  GenServer.init/1 callback
  """
  @impl true
  def init(state) do
    {:ok, state}
  end

  def child_spec() do
    %{
      id: Astreu.TopicManager,
      start: {Astreu.TopicManager, :start_link, []}
    }
  end

  def start_link(state \\ []) do
    Logger.info("Starting Topic Manager...")
    GenServer.start_link(__MODULE__, state, name: __MODULE__)
  end

  @impl true
  def handle_call(:info, _from, [value | state]) do
    {:reply, value, state}
  end

  @impl true
  def handle_call(:create, _from, [value | state]) do
    {:reply, value, state}
  end

  @impl true
  def handle_cast({:drop, _value}, state) do
    {:noreply, state}
  end

  # TODO Define client API
end
