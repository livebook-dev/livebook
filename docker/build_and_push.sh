#!/bin/bash

set -e
cd "$(dirname "$0")/.."

print_usage_and_exit() {
  echo "Usage: $0 <versions> <push>"
  echo ""
  echo "Builds extra variants of the regular Livebook Docker image."
  echo ""
  echo "<versions> is a comma-separated list of versions to use in the image tags (such as 0.1.0 or latest)."
  echo "Set <push> to 'true' to also pushe the images."
  echo "Set <arch> sets the architecture for the image being built."
  exit 1
}

if [ $# -ne 3 ]; then
  print_usage_and_exit
fi

versions=$1
push=$2
arch=$3

elixir=1.14.2
erlang=24.3.4.2

docker build -t elixir-cuda11.8 --build-arg ARCH=$arch --build-arg ELIXIR_VERSION=$elixir --build-arg ERLANG_VERSION=$erlang --build-arg CUDA_VERSION=11.8.0 -f docker/base/elixir-cuda.dockerfile docker/base
docker build -t livebook-cuda11.8 --build-arg BASE_IMAGE=elixir-cuda11.8 .


for version in ${versions//,/ }; do
  image=livebook/livebook:$version-cuda11.8
  docker tag livebook-cuda11.8 $image

  if [[ "$2" == "true" ]]; then
    docker push $image
  fi
done
