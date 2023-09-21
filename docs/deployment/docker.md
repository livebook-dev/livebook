# Docker

## Dockerfile

You can deploy Livebook inside your infrastructure using Docker. The Dockerfile below provides a great starting point:

```dockerfile
FROM ghcr.io/livebook-dev/livebook:edge

# Configure your port accordingly
ENV LIVEBOOK_PORT 7860
EXPOSE 7860

# If you have a persistent volume, configure it here
ENV LIVEBOOK_DATA_PATH "/data"
USER root
RUN mkdir -p /data
RUN chmod 777 /data
```

You can consult our [README](../../README.md#environment-variables) for a complete list of environment variables and configuration.

If you plan to limit access to your Livebook via a proxy, we recommend leaving the "/public" route of your instances still public. This route is used for integration with the [Livebook Badge](https://livebook.dev/badge/) and other conveniences.

## Livebook Teams

[Livebook Teams](https://livebook.dev/teams/) users have access to airgapped notebook deployment via Docker, with pre-configured Zero Trust Authentication, shared team secrets and file storages. To get started, open up Livebook, click "Add Organization" on the sidebar, and visit the "Airgapped Deployment" section of your organization.
