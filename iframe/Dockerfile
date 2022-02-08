# Stage 1: build
FROM hexpm/elixir:1.13.2-erlang-24.1.7-alpine-3.15.0 AS build

# Install build dependencies
RUN apk add --no-cache build-base git

WORKDIR /app

# Install hex and rebar
RUN mix local.hex --force && \
  mix local.rebar --force

# Set build ENV
ENV MIX_ENV=prod

# Install mix dependencies
COPY mix.exs mix.lock ./
RUN mix do deps.get, deps.compile

# Compile and build release
COPY priv priv
COPY lib lib
RUN mix do compile, release

# Stage 2: release image
FROM alpine:3.15.0

# Install runtime dependencies
RUN apk add --no-cache openssl ncurses-libs libstdc++

WORKDIR /app

# Copy the release build from the previous stage.
COPY --from=build /app/_build/prod/rel/livebook_space ./

ENV HOME=/app

CMD [ "/app/bin/livebook_space", "start" ]
