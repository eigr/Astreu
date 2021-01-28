defmodule Astreu.SubscriberManager do
  use GenServer
  require Logger

  @doc """
  GenServer.init/1 callback
  """
  @impl true
  def init(state) do
    {:ok, state}
  end

  def child_spec(state) do
    %{
      id: state.subscriber,
      start: {__MODULE__, :start_link, [state]},
    }
  end

  def start_link(state \\ []) do
    Logger.info("Starting Topic Manager...")
    GenServer.start_link(__MODULE__, state, name: via_tuple(state.subscriber))
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
  def handle_call(:subscribe, _from, [value | state]) do
    {:reply, value, state}
  end

  @impl true
  def handle_cast({:unsubscribe, _value}, state) do
    {:noreply, state}
  end

  @impl true
  def handle_cast({:drop, _value}, state) do
    {:noreply, state}
  end

  @impl true
  def handle_cast({:publish, value}, state) do
    {:noreply, state ++ [value]}
  end

  @impl true
  def handle_cast({:ack, value}, state) do
    {:noreply, state ++ [value]}
  end

  @impl true
  def handle_cast({:reply_to, value}, state) do
    {:noreply, state ++ [value]}
  end

  # TODO Define client API

  defp via_tuple(subscriber_id) do
    {:via, Registry, {:topics, subscriber_id}}
  end

end
