defmodule Astreu.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    Astreu.Supervisor.start_link([])
  end
end
