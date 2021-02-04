defmodule Astreu.Metrics.Setup do
  def setup do
    Astreu.Http.MetricsExporter.setup()
  end
end
