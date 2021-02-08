defmodule Astreu.HTTP.PlugBootstrap do
  require Logger

  @http_port 9100

  def setup() do
    Logger.info("Running Astreu Management API using http://0.0.0.0:#{@http_port}")

    Plug.Cowboy.child_spec(
      scheme: :http,
      plug: Astreu.Http.Endpoint,
      options: [port: @http_port]
    )
  end
end
