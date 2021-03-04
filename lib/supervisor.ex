defmodule Astreu.Supervisor do
  @moduledoc false
  use Supervisor
  require Logger

  @registry Astreu.TopicsRegistry
  @action_registry Astreu.ActionRegistry
  @subscribers_supervisor Astreu.SubscriberSupervisor
  @pubsub Application.get_env(:astreu, :producer_adapter)

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  def init(_args) do
    children =
      [
        cluster_supervisor(),
        Astreu.Producer.Dispatcher.child_spec([]),
        {Horde.Registry, [name: @registry, keys: :unique]},
        {Horde.Registry, [name: @action_registry, keys: :unique]},
        {Horde.DynamicSupervisor,
         [
           name: @subscribers_supervisor,
           strategy: :one_for_one,
           process_redistribution: :active
         ]},
        %{
          id: Astreu.HordeConnector,
          restart: :transient,
          start: {
            Task,
            :start_link,
            [
              fn ->
                set_members(@registry)
                set_members(@subscribers_supervisor)

                # TODO: Add logic to migrate state between process on nodes
                # Node.list()
                # |> Enum.each(fn node ->
                #  :ok = Astreu.StateHandoff.join(node)
                # end)
              end
            ]
          }
        },
        Astreu.NodeListener,
        @pubsub.init([]),
        Astreu.HTTP.PlugBootstrap.setup(),
        Astreu.HTTP.PlugBootstrap.drainer(),
        {GRPC.Server.Supervisor, {Astreu.Endpoint, 9980}}
      ]
      |> Enum.reject(&is_nil/1)

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp cluster_supervisor() do
    topologies = Application.get_env(:libcluster, :topologies)

    if topologies && Code.ensure_compiled(Cluster.Supervisor) do
      {Cluster.Supervisor, [topologies, [name: Astreu.ClusterSupervisor]]}
    end
  end

  defp set_members(name) do
    members =
      [Node.self() | Node.list()]
      |> Enum.map(fn node ->
        Logger.info(
          "[Astreu node on #{inspect(Node.self())}]: Connecting Horde to #{inspect(node)}"
        )

        {name, node}
      end)

    :ok = Horde.Cluster.set_members(name, members)
  end
end
