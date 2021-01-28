import Config

# Our Logger general configuration
config :logger,
  backends: [:console],
  compile_time_purge_level: :debug

config :protobuf, extensions: :enabled

# Our Console Backend-specific configuration
config :logger, :console,
  format: "$date $time [$node]:[$metadata]:[$level]:$levelpad$message\n",
  metadata: [:pid]

# Cluster configurations
config :libcluster,
  topologies: [
    dev: [
      strategy: Cluster.Strategy.Epmd,
      config: [
        hosts: [
          :"a@127.0.0.1",
          :"b@127.0.0.1",
          :"c@127.0.0.1"
        ]
      ]
    ]
  ]
