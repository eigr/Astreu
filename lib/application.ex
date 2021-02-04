defmodule Astreu.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    Node.set_cookie(String.to_atom("astreu"))

    Astreu.Metrics.Setup.setup()
    GRPCPrometheus.ServerInterceptor.setup()
    Astreu.Supervisor.start_link([])
  end
end
