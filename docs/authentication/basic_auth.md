# Basic Auth

Setting up Basic Authentication is a simple mechanism for protecting all routes of your Livebook instance with a single username-password combo. However, because this password is shared across all users, this authentication mechanism cannot be used to identity users and more robust authentication methods provided by Livebook should be preferred. Basic Authentication occurs in addition to [Livebook's authentication](../authentication.md) for deployed notebooks and admins.

## How to

To integrate Basic Authentication with Livebook, set the `LIVEBOOK_IDENTITY_PROVIDER` environment variable to `basic_auth:<username>:<password>`.

To do it, run:

```bash
LIVEBOOK_IDENTITY_PROVIDER=basic_auth:user:pass \
livebook server
```

## Livebook Teams

[Livebook Teams](https://livebook.dev/teams/) users can deploy notebooks with the click of a button with pre-configured Zero Trust Authentication, shared team secrets, and file storages. Both online and airgapped deployment mechanisms are supported.

Furthermore, if you are deploying multi-session apps via [Livebook Teams](https://livebook.dev/teams/), you can programmatically access data from the authenticated user by calling [`Kino.Workspace.app_info/0`](https://hexdocs.pm/kino/Kino.Workspace.html#app_info/0).

To get started, open up Livebook, click "Add Organization" on the sidebar. Then, inside the notebook of your choice, click "Deploy with Livebook Teams".
