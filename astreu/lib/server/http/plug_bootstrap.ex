defmodule Astreu.Server.HTTP.PlugBootstrap do
  require Logger

  @sol_socket 1
  @so_reuseport 15

  @http_port 9100
  @schedulers System.schedulers_online()

  def setup() do
    Logger.info("Running Astreu Management API using http://0.0.0.0:#{@http_port}")

    Plug.Cowboy.child_spec(
      scheme: :http,
      plug: Astreu.Server.Http.Endpoint,
      options: [port: @http_port],
      transport_options: [
        num_acceptors: @schedulers,
        socket_opts: [{:raw, @sol_socket, @so_reuseport, <<1::size(32)>>}]
      ]
    )
  end

  def drainer, do: {Plug.Cowboy.Drainer, refs: [Astreu.Server.Http.Endpoint]}
end
