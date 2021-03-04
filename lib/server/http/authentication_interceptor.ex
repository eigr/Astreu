defmodule Astreu.Server.Http.AuthenticationInterceptor do
  require Logger

  @behaviour GRPC.ServerInterceptor

  @impl GRPC.ServerInterceptor
  def init(_), do: []

  @impl GRPC.ServerInterceptor
  @spec call(
          GRPC.Server.rpc_req(),
          GRPC.Server.Stream.t(),
          GRPC.ServerInterceptor.next(),
          GRPC.ServerInterceptor.options()
        ) :: GRPC.ServerInterceptor.rpc_return()
  def call(request, stream, next, _options) do
    try do
      headers = GRPC.Stream.get_headers(stream)
      Logger.debug("Inbound Request with Headers #{inspect(headers)}")

      with headers when is_map(headers) <- headers,
           token when not is_nil(token) <- Map.get(headers, "authorization") do
        # use token
        # if authenticated
        next.(request, stream)
      else
        # TODO implement when is not authenticated
        request ->
          next.(request, stream)
      end
    rescue
      exception in GRPC.RPCError ->
        # {:error, exception}
        next.(request, stream)

      exception ->
        next.(request, stream)
        # reraise exception, __STACKTRACE__
    end
  end
end
