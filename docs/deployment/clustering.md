# Clustering

If you plan to run several Livebook instances behind a load balancer, you need to enable clustering via the `LIVEBOOK_CLUSTER` environment variable. This page describes how to configure the relevant environment variables.

If you are using [Livebook Teams](https://livebook.dev/teams/), you can deploy with the click of a button by running Livebook servers inside your infrastructure. To get started, open up Livebook and click "Add Organization" on the sidebar. Once completed, open up the Application pane on the sidebar (with a rocket icon), click "Deploy with Livebook Teams". We provide templates for clustering inside Fly.io and Kubernetes, without a need to follow the steps below.

## Setting `LIVEBOOK_CLUSTER`

You may set `LIVEBOOK_CLUSTER` to one of the following values.

### `auto`

> #### Attention {: .warning}
>
> "auto" is only a valid value if you're running Livebook's Docker image on AWS ECS, Kubernetes, and Fly.io.

Detects the hosting platform and automatically sets up a cluster using DNS configuration. See platform specific notes below.

#### AWS ECS & Fargate

If you're running Livebook in the **AWS ECS** environment, the `auto` configuration will automatically cluster based on the ECS Container Metadata HTTP API. The cluster's "deployment" name will be based on a SHA checksum of the ECS Container Image ID. Largely you don't need to care about this, but any Livebook deployment using the same image ID will be clustered together. If you want more containers (say in **AWS Fargate**), increase the `desiredCount` of the family.

While ECS and Fargate won't need any further configuration for clustering, you will need to do network level configuration to allow the containers to talk to other resources (databases, S3, etc), as well as be reached by the public internet, etc. That configuration is outside of the scope of this documentation. If you're having issues connecting, there's a good chance it's either you haven't setup the standard ports required, you haven't correctly setup or configured the security groups, or you haven't correctly configured the HTTP listeners/load balancers.

#### Fly.io

When deploying Livebook to Fly.io, the `auto` configuration automatically connects your cluster using Fly's private networks over IPv6.

#### Kubernetes

When using the Livebook application, you can also choose "auto" for Kubernetes deployments, but those values are automatically replaced by a DNS query, such as `dns:livebook-headless.$(POD_NAMESPACE).svc.cluster.local`, when generating the relevant resource definitions. See [the Kubernetes section in the Docker guides](docker.md#Kubernetes) for an example. 

### `dns:QUERY`

Sets up a cluster using DNS for queries for A/AAAA records to discover new nodes. Additionally, you must additionally set the following env vars:

- `LIVEBOOK_NODE=livebook_server@MACHINE_IP`, where `MACHINE_IP` is the machine IP of each deployed node

- If your cloud requires IPv6, also set `ERL_AFLAGS="-proto_dist inet6_tcp"`

## Setting other env vars

In addition you must set `LIVEBOOK_SECRET_KEY_BASE` and `LIVEBOOK_COOKIE` to different random values (use `openssl rand -base64 48` to generate said values).

You may need to set additional environment variables at runtime. When using the Livebook Docker image, you can create a file at `/app/user/env.sh` that exports the necessary environment variables. This file is invoked right before booting Livebook.
