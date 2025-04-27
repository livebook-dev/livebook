# FIPS mode

For environments that require security hardening, you might need to turn on FIPS ([Federal Information Processing Standards](https://en.wikipedia.org/wiki/Federal_Information_Processing_Standards)) mode. Turning FIPS is a complex procedure, this just enables you to do it.

You will need to have an Erlang installation that has been compiled with [FIPS enabled](https://www.erlang.org/doc/apps/crypto/fips).

## Docker example

To do this in Docker, you will need to build it differently. Below is an example Dockerfile with FIPS-enabled Erlang/Elixir base image. You can use it as a base image for building Livebook. See the Livebook Dockerfile for further reference.

```docker
FROM registry.access.redhat.com/ubi8/ubi-minimal:8.9-1137
# Set environment variables for path and language
ENV PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# Install system dependencies and clean cache in one layer
RUN microdnf install -y unzip autoconf git ncurses-devel openssl-devel gcc gcc-c++ make automake perl clang wget tar cmake glibc-locale-source glibc-langpack-en && \
    microdnf clean all && \
    rm -rf /var/cache/yum

WORKDIR /install

# Download, configure, and install Erlang/OTP with FIPS enabled
ARG ERLANG_VERSION
RUN wget https://github.com/erlang/otp/archive/OTP-${ERLANG_VERSION}.tar.gz && \
    tar -xzvf OTP-${ERLANG_VERSION}.tar.gz && \
    cd otp-OTP-${ERLANG_VERSION} && \
    ./otp_build autoconf && \
    ./configure --enable-fips && \
    make && make install

# Clone, checkout, and install Elixir
ARG ELIXIR_VERSION
RUN git clone https://github.com/elixir-lang/elixir.git && \
    cd elixir && \
    git checkout v${ELIXIR_VERSION} && \
    make compile && \
    make install
```
