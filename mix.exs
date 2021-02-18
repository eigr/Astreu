defmodule Astreu.MixProject do
  use Mix.Project

  def project do
    [
      app: :astreu,
      version: "0.1.1",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: releases(),
      aliases: [
        test: "test --no-start"
      ]
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
      {:grpc, github: "elixir-grpc/grpc", override: true},
      {:grpc_prometheus, "~> 0.1.0"},
      {:prometheus, "~> 4.6"},
      {:prometheus_plugs, "~> 1.1"},
      {:plug_cowboy, "~> 2.3"},
      {:cowlib, "~> 2.9.0", override: true},
      {:jason, "~> 1.2"},
      {:poison, "~> 4.0"},
      {:httpoison, "~> 1.6"},
      {:phoenix_pubsub, "~> 2.0"},
      {:uuid, "~> 1.1"},
      {:local_cluster, "~> 1.2", only: [:test]}
    ]
  end

  defp releases() do
    [
      astreu: [
        include_executables_for: [:unix],
        applications: [runtime_tools: :permanent]
      ]
    ]
  end
end
