ARG BASE_IMAGE
ARG VARIANT

# Pre-stages for base image variants

FROM ${BASE_IMAGE} AS base-default

FROM ${BASE_IMAGE} AS base-cuda

ARG CUDA_VERSION

RUN distro="ubuntu$(. /etc/lsb-release; echo "$DISTRIB_RELEASE" | tr -d '.')" && \
  # Official Docker images use the sbsa packages when targetting arm64.
  # See https://gitlab.com/nvidia/container-images/cuda/-/blob/85f465ea3343a2d7f7753a0a838701999ed58a01/dist/12.5.1/ubuntu2204/base/Dockerfile#L12
  arch="$(if [ "$(uname -m)" = "aarch64" ]; then echo "sbsa"; else echo "x86_64"; fi)" && \
  apt-get update && apt-get install -y ca-certificates wget && \
  wget -qO /tmp/cuda-keyring.deb https://developer.download.nvidia.com/compute/cuda/repos/$distro/$arch/cuda-keyring_1.1-1_all.deb && \
  dpkg -i /tmp/cuda-keyring.deb && apt-get update && \
  # In order to minimize the image size, we install only a subset of
  # the CUDA toolkit that is required by Elixir numerical packages
  # (nvcc and runtime libraries). Note that we do not need to install
  # the driver, it is already provided by NVIDIA Container Toolkit.
  cuda_version="${CUDA_VERSION}" && cuda_major="${cuda_version%-*}" && \
  apt-get install -y git cuda-nvcc-${CUDA_VERSION} cuda-libraries-${CUDA_VERSION} libcudnn9-cuda-$cuda_major && \
  apt-get clean -y && rm -rf /var/lib/apt/lists/*

ENV PATH="/usr/local/nvidia/bin:/usr/local/cuda/bin:$PATH"

# Build stage: builds the release
FROM base-${VARIANT} AS build

RUN apt-get update && apt-get upgrade -y && \
  apt-get install --no-install-recommends -y \
    build-essential ca-certificates git && \
  apt-get clean -y && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# This flag disables JIT behaviour that causes a segfault under QEMU.
# Note that we set this runtime flag only during the build stage and
# it has no impact when running the final image. See [1] for more
# information.
#
# [1]: https://github.com/erlang/otp/pull/6340
ENV ERL_FLAGS="+JMsingle true"

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Build for production
ENV MIX_ENV=prod

# Install mix dependencies
COPY mix.exs mix.lock ./
COPY config config
RUN mix do deps.get, deps.compile

# Compile and build the release
COPY priv/.gitkeep priv/.gitkeep
COPY rel rel
COPY static static
COPY iframe/priv/static/iframe iframe/priv/static/iframe
COPY proto proto
COPY lib lib
# We need README.md during compilation (look for @external_resource "README.md")
COPY README.md README.md
RUN mix do compile, release livebook

# Final stage: prepares the runtime environment and copies over the release.
#
# As opposed to usual deployments with Elixir releases, we want Erlang,
# Elixir and Mix to be available in the runtime image, because those
# are required for the Livebook runtimes. Consequently, we build the
# release with `include_erts: false`.
FROM base-${VARIANT}

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get upgrade -y && \
  apt-get install --no-install-recommends -y \
    # Runtime dependencies
    build-essential ca-certificates libncurses5-dev \
    # In case someone uses `Mix.install/2` and point to a git repo
    git \
    # Additional standard tools
    wget \
    # In case someone uses Torchx for Nx
    cmake && \
  apt-get clean -y && rm -rf /var/lib/apt/lists/*

# Run in the /data directory by default, makes for a good place for
# the user to mount local volume
WORKDIR /data

ENV HOME=/home/livebook
# Make sure someone running the container with `--user` has permissions
# to the home dir (for `Mix.install/2` cache)
RUN mkdir $HOME && chmod 777 $HOME

# Install hex and rebar for `Mix.install/2` and Mix runtime
RUN mix local.hex --force && \
  mix local.rebar --force

# By default Livebook binds to loopback, but in order to make the app
# accessible outside of the container (by binding ports), we need to
# bind to any address. Also note that we specify IPv6 address, becuase
# we want to accept IPv6 connections. This is actually the default in
# usual Phoenix deployments.
ENV LIVEBOOK_IP="::"

ENV LIVEBOOK_HOME=/data

# Copy the release build from the previous stage
COPY --from=build /app/_build/prod/rel/livebook /app

# Make release files available to any user, in case someone runs the
# container with `--user`
RUN chmod -R go=u /app
# Make all home files available (specifically .mix/)
RUN chmod -R go=u $HOME

HEALTHCHECK CMD wget --no-verbose --tries=1 --spider http://localhost:${LIVEBOOK_PORT-8080}/public/health || exit 1

CMD [ "/app/bin/server" ]
