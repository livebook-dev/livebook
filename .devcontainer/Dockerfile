# This image version needs to match what is in the root Dockerfile.
FROM hexpm/elixir:1.13.2-erlang-24.1.7-debian-bullseye-20210902-slim

ENV HOME=/home/livebook
ENV PATH ${PATH}:${HOME}/.mix/escripts

# GitHub Codespaces put you in a `/workspaces` folder no matter what.
ENV WORKSPACE_FOLDER=/workspaces/livebook

WORKDIR ${WORKSPACE_FOLDER}

# Install the dependencies from the build image (which is discarded in the normal production images)
RUN apt-get update && apt-get upgrade -y && \
    apt-get install --no-install-recommends -y \
        # Bash (for easier shell use)
        bash \
        # Runtime dependencies
        build-essential ca-certificates libncurses5-dev inotify-tools \
        # In case someone uses `Mix.install/2` and point to a git repo
        git \
        # Additional standard tools
        curl wget \
        # In case someone uses Torchx for Nx
        cmake && \
    apt-get purge -y --auto-remove -o APT::AutoRemove::RecommendsImportant=false && \
    apt-get clean -y && \
    rm -rf /var/lib/apt/lists/*

# Install hex and rebar
RUN mix local.hex --force && mix local.rebar --force

# Node/nvm setup
ARG NODE_SCRIPT_SOURCE="https://raw.githubusercontent.com/microsoft/vscode-dev-containers/main/script-library/node-debian.sh"
ARG NODE_SCRIPT_SHA="dev-mode"
ARG NODE_VERSION="14"
ENV NVM_DIR=/usr/local/share/nvm
ENV NVM_SYMLINK_CURRENT=true
ENV PATH=${NVM_DIR}/current/bin:${PATH}

RUN curl -sSL ${NODE_SCRIPT_SOURCE} -o /tmp/node-setup.sh && \
    /bin/bash /tmp/node-setup.sh "${NVM_DIR}" "${NODE_VERSION}" "${USERNAME}"

# On Codespaces the code is automatically copied and we run the setup as a hook,
# we still do this for the image, so the container can be tested on its own
COPY . .
RUN mix do dev.setup, deps.compile, compile
