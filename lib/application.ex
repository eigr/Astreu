defmodule Astreu.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # {GRPC.Server.Supervisor, {Image.Storage.Endpoint, @port}}
      # {Phoenix.PubSub.PG2, name: Astreu.PubSub},
      {Phoenix.PubSub, name: Astreu.PubSub}
    ]

    opts = [strategy: :one_for_one, name: Astreu.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
