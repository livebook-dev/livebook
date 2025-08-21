# Deploy Livebook Apps via CLI

Livebook provides a way to deploy Livebook apps via a CLI command to your deployment group's app servers.

This is particularly useful for automating deployments in CI/CD pipelines.

Deploying via CLI is as simple as calling `livebook deploy` with your configurations:

```shell
livebook deploy
  --org-token="lb_ot_your_org_token_here"
  --teams-key="lb_tk_your_teams_key_here"
  --deployment-group-id="13"
  path/to/notebook.livemd
```

> #### Livebook version requirement {: .info}
> Requires Livebook v0.17 or newer.

## Overview

CLI deployment enables you to:

- Deploy Livebook apps from the command line
- Automate app deployments in CI/CD pipelines
- Deploy single or multiple notebooks

## Prerequisites

Before using CLI deployment, ensure you have:

1. **A configured deployment group** in your Livebook Teams organization
2. **Deploy and Teams keys** from your Livebook Teams organization
3. **Elixir installed** on the machine where deployment will be performed
4. **Livebook CLI v0.17 or newer** installed via `mix escript.install hex livebook`

## Authentication

CLI deployment requires two authentication tokens:

### Org token

Org tokens are organization-level authentication tokens that allow CLI access for deployments.

To create a org token, follow these steps:

1. Log in to Livebook Teams
2. Navigate to your organization
3. Go to the **Tokens** page in the menu
4. Click the **Create org token** button
5. Provide a descriptive name (e.g., "CI/CD Pipeline" or "Local CLI") and copy the generated token

### Teams key

Each organization has a unique Teams key available in the local Livebook of each member of the organization.

To get the Teams key of your organization, follow these steps:

1. Open Livebook
2. Navigate to your Teams workspace in the sidebar
3. Click on "Display Teams key" at the top, and copy your Teams key

## Usage

### Deploying a single app

Deploy a single notebook:

```bash
livebook deploy \
  --org-token="lb_ot_..." \
  --teams-key="lb_tk_..." \
  --deployment-group-id="17" \
  path/to/notebook.livemd
```

### Deploying multiple apps at once

Deploy multiple notebooks:

```bash
livebook deploy \
  --org-token="lb_ot_..." \
  --teams-key="lb_tk_..." \
  --deployment-group-id="13" \
  app1.livemd app2.livemd app3.livemd
```

Use glob patterns for convenience:

```bash
livebook deploy \
  --org-token="lb_ot_..." \
  --teams-key="lb_tk_..." \
  --deployment-group-id="7" \
  notebooks/*.livemd
```

### Available options

- `--org-token`: A token from your Livebook Teams organization (required)
- `--teams-key`: Teams key from your Teams organization (required)
- `--deployment-group-id`: ID of the target deployment group (required)

## CI/CD integration

CLI deployment integrates seamlessly with automated workflows. Here's a GitHub Actions example:

```yaml
name: Deploy Livebook Apps
on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      # Install Elixir
      - name: Set up Elixir
        uses: erlef/setup-beam@v1
        with:
          elixir-version: '1.18.3'
          otp-version: '27.3.4.1'

      # Install Livebook CLI
      - name: Install Livebook
        run:
          mix escript.install hex livebook --force

      # Uses Livebook CLI to deploy your notebooks
      - name: Deploy notebooks
        run: |
          livebook deploy \
            --org-token="${{ secrets.LIVEBOOK_TEAMS_ORG_TOKEN }}" \
            --teams-key="${{ secrets.LIVEBOOK_TEAMS_KEY }}" \
            --deployment-group-id="3" \
            ./notebooks/*.livemd
```

Store your org token and teams key as repository secrets for secure access.

## FAQ

### Do I need a running app server to deploy?

No, you don't need a running app server at deployment time. If your app server isn't running, Livebook Teams will queue the deployment and automatically deploy when the server comes online.

### Why am I getting an "invalid notebook stamp" error when deploying?

This error occurs when the notebook you're trying to deploy hasn't been properly stamped. Stamping happens automatically whenever a notebook is saved via Livebook.

[Notebook stamping](../stamping.md) is a security mechanism to ensure only notebooks processed through your team's workspace can be deployed.

To fix this error, open the notebook inside Livebook, make sure it's using your Teams organization workspace, and save the notebook.
