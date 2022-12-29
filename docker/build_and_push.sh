#!/bin/bash

# Builds base images used to build extra flavours of Livebook images

set -ex
cd "$(dirname "$0")/.."

elixir=1.14.2
erlang=24.3.4.2

docker buildx build --push --platform linux/amd64,linux/arm64 \
  -t livebook/utils:elixir-cuda11.8 \
  --build-arg ELIXIR_VERSION=$elixir \
  --build-arg ERLANG_VERSION=$erlang \
  --build-arg CUDA_VERSION=11.8.0 \
  -f docker/base/elixir-cuda.dockerfile \
  docker/base
