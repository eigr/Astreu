defmodule Astreu.Action do
  use GenServer
  require Logger

  alias Wasmex, as: Wasm

  @action_registry Astreu.ActionRegistry

  @impl true
  def init(state) do
    Process.flag(:trap_exit, true)
    {:ok, state}
  end

  def child_spec(state) do
    %{
      # TODO name
      id: __MODULE__,
      start: {__MODULE__, :start_link, [state]}
    }
  end

  def start_link(state \\ []) do
    # Fix Name
    GenServer.start_link(__MODULE__, state, name: via_tuple(__MODULE__))
  end

  @impl true
  def handle_call(:handle, _from, state) do
    # TODO: Move to init/1
    # TODO: Get bytes of wasm module
    {:ok, instance} = Wasm.start_link(state)

    # Call a function on it.
    # TODO: Pass string bytes
    {:ok, [_result]} = Wasm.call_function(instance, "handle", [1, 2])
    {:reply, state, state}
  end

  @impl true
  def terminate(_reason, state) do
    state
  end

  defp via_tuple(id) do
    {:via, Horde.Registry, {@action_registry, id}}
  end
end
