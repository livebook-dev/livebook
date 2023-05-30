ARG ELIXIR_VERSION
ARG ERLANG_VERSION
ARG UBUNTU_VERSION

ARG CUDA_VERSION

FROM hexpm/elixir:${ELIXIR_VERSION}-erlang-${ERLANG_VERSION}-ubuntu-${UBUNTU_VERSION} AS elixir

FROM nvidia/cuda:${CUDA_VERSION}-cudnn8-devel-ubuntu20.04

ENV DEBIAN_FRONTEND nonintaeractive
ENV LANG=C.UTF-8

# Erlang runtime dependencies, see https://github.com/hexpm/bob/blob/3b5721dccdfe9d59766f374e7b4fb7fb8a7c720e/priv/scripts/docker/erlang-ubuntu-focal.dockerfile#L41-L45
RUN apt-get update && \
  apt-get -y --no-install-recommends install \
    libodbc1 \
    libssl1.1 \
    libsctp1

# We copy the top-level directory first to preserve symlinks in /usr/local/bin
COPY --from=elixir /usr/local /usr/ELIXIR_LOCAL

RUN cp -r /usr/ELIXIR_LOCAL/lib/* /usr/local/lib && \
  cp -r /usr/ELIXIR_LOCAL/bin/* /usr/local/bin && \
  rm -rf /usr/ELIXIR_LOCAL
