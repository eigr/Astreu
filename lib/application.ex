defmodule Astreu.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # {Phoenix.PubSub.PG2, name: Astreu.PubSub},
      {Phoenix.PubSub, name: Astreu.PubSub},
      {GRPC.Server.Supervisor, {Astreu.Endpoint, 9980}}
    ]

    opts = [strategy: :one_for_one, name: Astreu.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
