defmodule Astreu.SubscriberManager do
  use GenServer
  require Logger
  alias GRPC.Server
  alias Phoenix.PubSub

  @registry Astreu.TopicsRegistry

  @doc """
  GenServer.init/1 callback
  """
  @impl true
  def init(state) do
    Process.flag(:trap_exit, true)
    {:ok, state}
  end

  def child_spec(state) do
    %{
      id: get_subscriber(state),
      start: {__MODULE__, :start_link, [state]}
    }
  end

  def start_link(state \\ []) do
    Logger.info("Starting Subscriber Manager with state #{inspect(state)}")
    GenServer.start_link(__MODULE__, state, name: via_tuple(get_subscriber(state)))
  end

  @impl true
  def handle_call(:subscribe, _from, state) do
    Logger.debug(
      "Subscriber #{inspect(state.subscriber)} subscribe to topic #{inspect(state.topic)}"
    )

    PubSub.subscribe(Astreu.PubSub, state.topic)
    {:reply, state, state}
  end

  @impl true
  def handle_info({:enqueue, message}, state) do
    Logger.debug(
      "Receive message #{inspect(message)}, Forward to subscriber #{inspect(state.subscriber)}"
    )

    # state.stream |> Server.send_reply(message)
    {:noreply, state}
  end

  @impl true
  def handle_cast({:unsubscribe, _value}, state) do
    Logger.debug(
      "Subscriber #{inspect(state.subscriber)} unsubscribe from topic #{inspect(state.topic)}"
    )

    PubSub.unsubscribe(Astreu.PubSub, state.topic)
    {:noreply, state, state}
  end

  @impl true
  def handle_cast({:ack, value}, state) do
    {:noreply, state ++ [value]}
  end

  @impl true
  def handle_cast({:reply_to, value}, state) do
    {:noreply, state ++ [value]}
  end

  def terminate(reason, state) do
    Logger.info("terminating")
    cleanup(reason, state)
    state
  end

  # TODO Define client API
  def subscribe(subscriber) do
    GenServer.call(via_tuple(subscriber), :subscribe)
  end

  # internals

  defp via_tuple(subscriber_id) do
    {:via, Horde.Registry, {@registry, subscriber_id}}
  end

  defp get_subscriber(state), do: "#{state.topic}:#{state.subscriber}"

  defp cleanup(reason, state) do
    # TODO implement Cleanup here
  end
end
