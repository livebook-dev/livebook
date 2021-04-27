# Stage 1
# Builds the Livebook release
FROM hexpm/elixir:1.12.0-rc.1-erlang-24.0-rc3-alpine-3.13.3 AS build

RUN apk add --no-cache build-base git

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
COPY priv priv
COPY lib lib
# We need README.md during compilation
# (look for @external_resource "README.md")
COPY README.md README.md
RUN mix do compile, release

# Stage 2
# Prepares the runtime environment and copies over the relase.
# We use the same base image, because we need Erlang, Elixir and Mix
# during runtime to spawn the Livebook standalone runtimes.
# Consequently the release doesn't include ERTS as we have it anyway.
FROM hexpm/elixir:1.12.0-rc.1-erlang-24.0-rc3-alpine-3.13.3

RUN apk add --no-cache \
    # Runtime dependencies
    openssl ncurses-libs \
    # In case someone uses `Mix.install/2` and point to a git repo
    git

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

# Copy the release build from the previous stage
COPY --from=build /app/_build/prod/rel/livebook /app

# Make release executables available to any user,
# in case someone runs the container with `--user`
RUN find /app -executable -type f -exec chmod +x {} +

CMD [ "/app/bin/livebook", "start" ]
