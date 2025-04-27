# Clustering

If you plan to run several Livebook instances behind a load balancer, you need to enable clustering via the `LIVEBOOK_CLUSTER` environment variable. This page describes how to configure the relevant environment variables.

If you are using [Livebook Teams](https://livebook.dev/teams/), you can deploy with the click of a button by running Livebook servers inside your infrastructure. To get started, open up Livebook and click "Add Organization" on the sidebar. Once completed, open up the Application pane on the sidebar (with a rocket icon), click "Deploy with Livebook Teams". We provide templates for clustering inside Fly.io and Kubernetes, without a need to follow the steps below.

## Setting `LIVEBOOK_CLUSTER`

You may set `LIVEBOOK_CLUSTER` to one of the following values.

### `auto`

> #### Attention {: .warning}
>
> "auto" is only a valid value if you're running Livebook's Docker image on Fly.io or Kubernetes.

Detects the hosting platform and automatically sets up a cluster using DNS configuration. Currently the supported platforms are Fly.io and Kubernetes.

### `dns:QUERY`

Sets up a cluster using DNS for queries for A/AAAA records to discover new nodes. Additionally, you must additionally set the following env vars:

  * `LIVEBOOK_NODE=livebook_server@MACHINE_IP`, where `MACHINE_IP` is the machine IP of each deployed node

  * If your cloud requires IPv6, also set `ERL_AFLAGS="-proto_dist inet6_tcp"`

## Setting other env vars

In addition you must set `LIVEBOOK_SECRET_KEY_BASE` and `LIVEBOOK_COOKIE` to different random values (use `openssl rand -base64 48` to generate said values).

You may need to set additional environment variables at runtime. When using the Livebook Docker image, you can create a file at `/app/user/env.sh` that exports the necessary environment variables. This file is invoked right before booting Livebook.
