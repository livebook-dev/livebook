# This image version needs to match what is in the root Dockerfile.
FROM livebook/livebook:edge

# GitHub Codespaces put you in a `/workspaces` folder no matter what.
ENV HOME=/workspaces/livebook
WORKDIR $HOME

# Install the dependencies from the build image (which is discarded in the normal production images)
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

# Copy everything for development purposes
COPY .. .

# Install hex and rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Build for development
ENV MIX_ENV=dev

RUN mix dev.setup