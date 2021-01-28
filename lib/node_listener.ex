defmodule Astreu.NodeListener do
  use GenServer
  require Logger

  @registry Astreu.TopicsRegistry
  @subscribers_supervisor Astreu.SubscriberSupervisor

  def start_link(_), do: GenServer.start_link(__MODULE__, [])

  def init(_) do
    :net_kernel.monitor_nodes(true, node_type: :visible)
    {:ok, nil}
  end

  def handle_info({:nodeup, _node, _node_type}, state) do
    Logger.debug("Node listener on event nodeup. Updating cluster members...")
    set_members(@registry)
    set_members(@subscribers_supervisor)
    {:noreply, state}
  end

  def handle_info({:nodedown, _node, _node_type}, state) do
    Logger.debug("Node listener on event nodedown. Updating cluster members...")
    set_members(@registry)
    set_members(@subscribers_supervisor)
    {:noreply, state}
  end

  defp set_members(name) do
    members =
      [Node.self() | Node.list()]
      |> Enum.map(fn node ->
        Logger.debug(
          "[Node listener on #{inspect(Node.self())}]: Connecting Horde to #{inspect(node)}"
        )

        {name, node}
      end)

    :ok = Horde.Cluster.set_members(name, members)
  end
end
