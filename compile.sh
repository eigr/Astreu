#!/usr/bin/env bash

set -o nounset
set -o errexit
set -o pipefail

# Invent Protocol

protoc --include_imports \
  --descriptor_set_out=priv/protos/astreu/astreu.desc \
  --elixir_out=gen_descriptors=true,plugins=grpc:./lib \
  --proto_path=priv/protos/astreu \
  priv/protos/astreu/manager/manager.proto  \
  priv/protos/astreu/protocol/protocol.proto  \
  priv/protos/astreu/producer/publisher.proto \
  priv/protos/astreu/consumer/subscriber.proto
