defmodule Astreu.Endpoint do
  use GRPC.Endpoint

  intercept(GRPC.Logger.Server)
  intercept(GRPCPrometheus.ServerInterceptor)

  services = [
    Astreu.Manager.Service,
    Astreu.Consumer.Service,
    Astreu.Producer.Service
  ]

  run(services)
end
