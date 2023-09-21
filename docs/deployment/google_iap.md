# Authentication with Google IAP

Setting up Google IAP authentication will protect all routes of your notebook. It is particularly useful for adding authentication to deployed notebooks. Google IAP authentication is provided in addition to [Livebook's authentication](../authentication.md) for authoring notebooks.

Once Google IAP is enabled, we recommend leaving the "/public" route of your instances still public. This route is used for integration with the [Livebook Badge](https://livebook.dev/badge/) and other conveniences.

## How to

To integrate your Google Identity-Aware Proxy (IAP) authentication with Livebook,
set the `LIVEBOOK_IDENTITY_PROVIDER` environment variable to `google_iap:<your-jwt-audience>`.

For more information about Google IAP, see https://cloud.google.com/iap/docs/concepts-overview.

Only access with Google accounts is supported. See https://cloud.google.com/iap/docs/authenticate-users-google-accounts.

For more details about how to find your JWT audience, see https://cloud.google.com/iap/docs/signed-headers-howto and look for "Signed Header JWT Audience."

## Livebook Teams

[Livebook Teams](https://livebook.dev/teams/) users have access to airgapped notebook deployment via Docker, with pre-configured Zero Trust Authentication, shared team secrets and file storages. To get started, open up Livebook, click "Add Organization" on the sidebar, and visit the "Airgapped Deployment" section of your organization.
