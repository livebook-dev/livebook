#!/bin/bash

# Builds extra Livebook images.
#
# Images are pushed to GHCR, so you need to make sure that the docker
# client is authenticated.

set -ex
cd "$(dirname "$0")/.."

. versions

livebook_version="$(cat mix.exs | grep '@version "' | grep -v '\-dev' | grep -oE '[0-9]{1,}.[0-9]{1,}.[0-9]{1,}' || true)"


if [ -z "$livebook_version" ]; then
  echo "No stable Livebook version detected"
  exit 1
fi

cuda_tag_list=("cuda11.8" "cuda12.1")
cuda_version_list=("11.8.0" "12.1.0")

for idx in "${!cuda_tag_list[@]}"; do
  cuda_tag="${cuda_tag_list[idx]}"
  cuda_version="${cuda_version_list[idx]}"

  base_image="ghcr.io/livebook-dev/utils:elixir-$elixir-erlang-$otp-$cuda_tag"

  if docker manifest inspect $base_image > /dev/null; then
    echo "Using base image: $base_image"
  else
    echo "Building base image: $base_image"

    docker buildx build --push --platform linux/amd64,linux/arm64 \
      -t $base_image \
      --build-arg ELIXIR_VERSION=$elixir \
      --build-arg ERLANG_VERSION=$otp \
      --build-arg UBUNTU_VERSION=$ubuntu \
      --build-arg CUDA_VERSION=$cuda_version \
      -f docker/base/elixir-cuda.dockerfile \
      docker/base
  fi

  image="ghcr.io/livebook-dev/livebook:$livebook_version-$cuda_tag"

  if docker manifest inspect $image > /dev/null; then
    echo "Skipping image, since it already exists: $image"
  else
    echo "Building image: $image"

    docker buildx build --push --platform linux/amd64,linux/arm64 \
      --build-arg BASE_IMAGE="$base_image" \
      -t $image \
      -t ghcr.io/livebook-dev/livebook:latest-$cuda_tag \
      .
  fi
done


# Generate Ubi fips capable images
# https://catalog.redhat.com/software/containers/ubi8/ubi-minimal/5c359a62bed8bd75a2c3fba8?architecture=amd64&image=65cad19b3e4fe61cff409362&container-tabs=gti
image="ghcr.io/livebook-dev/livebook:$livebook_version-ubi8-fips"
base_image="registry.access.redhat.com/ubi8/ubi-minimal:8.9-1137"

if docker manifest inspect $image > /dev/null; then
  echo "Skipping image, since it already exists: $image"
else
  echo "Building image: $image"

  docker build \
    --build-arg ELIXIR_VERSION=$elixir \
    --build-arg ERLANG_VERSION=$otp \
    --build-arg BASE_IMAGE=$base_image \
    -f docker/base/elixir-ubi-fips.dockerfile \
    -t $image \
    -t ghcr.io/livebook-dev/livebook:latest-ubi8-fips \
    .
fi