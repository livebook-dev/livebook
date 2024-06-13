# Clustering

If you plan to run several Livebook instances behind a load balancer, you need to enable clustering via the `LIVEBOOK_CLUSTER` environment variable in your Docker image.

Depending on the clustering strategy of your choice, you must set additional environment variables, oftentimes, at runtime. When using the Livebook Docker image, you can create a file at `/app/user/env.sh` that exports the necessary environment variables. This file is invoked right before booting Livebook.

You may set `LIVEBOOK_CLUSTER` to one of the following values.

## `fly`

It automatically sets up a cluster to run on Fly using DNS configuration. Behind the scenes, it sets the relevant environment variables and enables IPv6 support.

## `dns:QUERY`

Sets up a cluster using DNS for queries for A/AAAA records to discover new nodes. Additionally, you must additionally set the following env vars:

  * `LIVEBOOK_NODE=livebook_server@MACHINE_IP`, where `MACHINE_IP` is the machine IP of each deployed node

  * You must set `LIVEBOOK_SECRET_KEY_BASE` and `LIVEBOOK_COOKIE` to different random values (use `openssl rand -base64 48` to generate said values)

  * If your cloud requires IPv6, also set `ERL_AFLAGS="-proto_dist inet6_tcp"`
