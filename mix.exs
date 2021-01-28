defmodule Astreu.MixProject do
  use Mix.Project

  def project do
    [
      app: :astreu,
      version: "0.1.0",
      elixir: "~> 1.11-dev",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [
        :logger,
        :observer,
        :phoenix_pubsub
      ],
      mod: {Astreu.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:horde, "~> 0.8.3"},
      {:libcluster, "~> 3.2.1"},
      {:google_protos, "~> 0.1.0"},
      {:grpc, github: "elixir-grpc/grpc"},
      {:cowlib, "~> 2.9.0", override: true},
      {:phoenix_pubsub, "~> 2.0"}
    ]
  end
end
