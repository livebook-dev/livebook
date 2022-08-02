# Stage 1
# Builds the Livebook release
FROM hexpm/elixir:1.13.4-erlang-25.0.2-debian-bullseye-20210902-slim AS build

RUN apt-get update && apt-get upgrade -y && \
    apt-get install --no-install-recommends -y \
        build-essential git && \
    apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false && \
    apt-get clean -y && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

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
COPY rel rel
COPY static static
COPY iframe/priv/static/iframe iframe/priv/static/iframe
COPY lib lib
# We need README.md during compilation
# (look for @external_resource "README.md")
COPY README.md README.md
RUN mix do compile, release livebook

# Stage 2
# Prepares the runtime environment and copies over the release.
# We use the same base image, because we need Erlang, Elixir and Mix
# during runtime to spawn the Livebook standalone runtimes.
# Consequently the release doesn't include ERTS as we have it anyway.
FROM hexpm/elixir:1.13.4-erlang-25.0.2-debian-bullseye-20210902-slim

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
    apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false && \
    apt-get clean -y && \
    rm -rf /var/lib/apt/lists/*

# Run in the /data directory by default, makes for
# a good place for the user to mount local volume
WORKDIR /data

ENV HOME=/home/livebook
# Make sure someone running the container with `--user`
# has permissions to the home dir (for `Mix.install/2` cache)
RUN mkdir $HOME && chmod 777 $HOME

# Install hex and rebar for `Mix.install/2` and Mix runtime
RUN mix local.hex --force && \
    mix local.rebar --force

# Override the default 127.0.0.1 address, so that the app
# can be accessed outside the container by binding ports
ENV LIVEBOOK_IP 0.0.0.0

ENV LIVEBOOK_HOME=/data

# Copy the release build from the previous stage
COPY --from=build /app/_build/prod/rel/livebook /app

# Make release files available to any user, in case someone
# runs the container with `--user`
RUN chmod -R go=u /app

CMD [ "/app/bin/livebook", "start" ]
