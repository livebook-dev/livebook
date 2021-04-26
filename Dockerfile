FROM hexpm/elixir:1.11.4-erlang-24.0-rc3-alpine-3.13.3 AS build

RUN apk add --no-cache build-base git python3

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

# Compile and build release
COPY priv priv
COPY lib lib
# We need README.md during compilation
# (look for @external_resource "README.md")
COPY README.md README.md
RUN mix do compile, release

FROM hexpm/elixir:1.11.4-erlang-24.0-rc3-alpine-3.13.3

# Install runtime dependencies
RUN apk add --no-cache openssl ncurses-libs

# Run in the /data directory by default,
# a good place for the user to mount local volume
WORKDIR /data

ENV HOME=/data

# Copy the release build from the previous stage
COPY --from=build /app/_build/prod/rel/livebook /app

COPY bin/docker-cmd.sh /app/docker-cmd.sh
CMD [ "/app/docker-cmd.sh" ]
