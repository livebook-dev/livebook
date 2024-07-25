ARG ELIXIR_VERSION
ARG ERLANG_VERSION
ARG UBUNTU_VERSION

FROM hexpm/elixir:${ELIXIR_VERSION}-erlang-${ERLANG_VERSION}-ubuntu-${UBUNTU_VERSION}

ARG CUDA_VERSION

RUN distro="ubuntu$(. /etc/lsb-release; echo "$DISTRIB_RELEASE" | tr -d '.')" && \
    # Official Docker images use the sbsa packages when targetting arm64.
    # See https://gitlab.com/nvidia/container-images/cuda/-/blob/85f465ea3343a2d7f7753a0a838701999ed58a01/dist/12.5.1/ubuntu2204/base/Dockerfile#L12
    arch="$(if [ "$(uname -m)" = "aarch64" ]; then echo "sbsa"; else echo "x86_64"; fi)" && \
    apt update -q && apt install -y ca-certificates wget && \
    wget -qO /tmp/cuda-keyring.deb https://developer.download.nvidia.com/compute/cuda/repos/$distro/$arch/cuda-keyring_1.1-1_all.deb && \
    dpkg -i /tmp/cuda-keyring.deb && apt update -q

# In order to minimize the image size, we install only a subset of
# the CUDA toolkit that is required by Elixir numerical packages
# (nvcc and runtime libraries). Note that we do not need to install
# the driver, it is already provided by NVIDIA Container Toolkit.
RUN apt install -y git cuda-nvcc-${CUDA_VERSION} cuda-libraries-${CUDA_VERSION} libcudnn8
