defmodule GRPCPrometheus.MixProject do
  use Mix.Project

  def project do
    [
      app: :grpc_prometheus,
      version: "0.1.0",
      elixir: "~> 1.4",
      description: "Prometheus interceptor for gRPC",
      start_permanent: Mix.env() == :prod,
      package: package(),
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp package do
    %{
      maintainers: ["Bing Han"],
      licenses: ["MIT"],
      links: %{"GitHub" => "https://github.com/elixir-grpc/grpc-prometheus"},
      files: ~w(mix.exs README.md lib LICENSE)
    }
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:prometheus, "~> 4.0"},
      {:prometheus_ex, "~> 3.0"},
      {:grpc, ">= 0.0.0", optional: true},
      {:ex_doc, "~> 0.21", only: :dev, runtime: false}
    ]
  end
end
