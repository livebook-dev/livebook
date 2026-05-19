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
