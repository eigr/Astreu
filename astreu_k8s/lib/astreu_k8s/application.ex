defmodule AstreuK8s.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Starts a worker by calling: AstreuK8s.Worker.start_link(arg)
      # {AstreuK8s.Worker, arg}
    ]

    opts = [strategy: :one_for_one, name: AstreuK8s.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
