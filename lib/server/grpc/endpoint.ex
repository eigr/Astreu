defmodule Astreu.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Logger.Server)
  intercept(GRPCPrometheus.ServerInterceptor)
  intercept(Astreu.Http.AuthenticationInterceptor)

  services = [
    Astreu.Manager.Service,
    Astreu.Consumer.Service,
    Astreu.Producer.Service
  ]

  run(services)
end
