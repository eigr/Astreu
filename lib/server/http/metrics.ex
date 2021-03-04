defmodule Astreu.Server.Http.Metrics.Setup do
  def setup do
    Astreu.Server.Http.MetricsExporter.setup()
    GRPCPrometheus.ServerInterceptor.setup()
  end
end
