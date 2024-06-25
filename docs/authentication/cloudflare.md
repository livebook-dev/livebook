# Cloudflare

Setting up Cloudflare authentication will protect all routes of your Livebook instance. It is particularly useful for adding authentication to Livebook instances with deployed notebooks. Cloudflare authentication occurs in addition to [Livebook's authentication](../authentication.md) for deployed notebooks and admins.

Once Cloudflare is enabled, we recommend leaving the "/public" route of your instances still public. This route is used for integration with the [Livebook Badge](https://livebook.dev/badge/) and other conveniences.

## How to

To integrate your Cloudflare Zero Trust authentication with Livebook, set the
`LIVEBOOK_IDENTITY_PROVIDER` environment variable to `cloudflare:<your-team-name>`.

For more details about how to find your `team-name`, see:
https://developers.cloudflare.com/cloudflare-one/glossary/#team-name.

For more information about Cloudflare Zero Trust, see:
https://developers.cloudflare.com/cloudflare-one/.

## Livebook Teams

[Livebook Teams](https://livebook.dev/teams/) users can deploy notebooks with the click of a button with pre-configured Zero Trust Authentication, shared team secrets, and file storages. Both online and airgapped deployment mechanisms are supported.

Furthermore, if you are deploying multi-session apps via [Livebook Teams](https://livebook.dev/teams/), you can programmatically access data from the authenticated user by calling [`Kino.Workspace.app_info/0`](https://hexdocs.pm/kino/Kino.Workspace.html#app_info/0).

To get started, open up Livebook, click "Add Organization" on the sidebar. Then, inside the notebook of your choice, click "Deploy with Livebook Teams".
