#!/bin/bash

# Builds base images used to build extra flavours of Livebook images

set -ex
cd "$(dirname "$0")/.."

. versions

docker buildx build --push --platform linux/amd64,linux/arm64 \
  -t ghcr.io/livebook-dev/utils:elixir-$elixir-erlang-$otp-cuda11.8 \
  --build-arg ELIXIR_VERSION=$elixir \
  --build-arg ERLANG_VERSION=$otp \
  --build-arg UBUNTU_VERSION=$ubuntu \
  --build-arg CUDA_VERSION=11.8.0 \
  -f docker/base/elixir-cuda.dockerfile \
  docker/base
