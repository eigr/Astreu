defmodule Astreux.MixProject do
  use Mix.Project

  def project do
    [
      app: :astreux,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Astreux.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:google_protos, "~> 0.1.0"},
      {:grpc, github: "elixir-grpc/grpc", override: true},
      {:cowlib, "~> 2.9.0", override: true},
      {:uuid, "~> 1.1"},
    ]
  end
end
