defmodule Astreu.Server.Grpc.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Logger.Server)
  intercept(GRPCPrometheus.ServerInterceptor)
  intercept(Astreu.Server.Http.AuthenticationInterceptor)

  services = [
    Astreu.Core.Protocol.Manager.Service,
    Astreu.Core.Protocol.Consumer.Service,
    Astreu.Core.Protocol.Producer.Service
  ]

  run(services)
end
