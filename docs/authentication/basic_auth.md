# Basic Auth

Setting up Basic Authentication is a simple mechanism for protecting all routes of your Livebook instance with a single username-password combo. However, because this password is shared across all users, this authentication mechanism cannot be used to identity users and more robust authentication methods provided by Livebook should be preferred. Basic Authentication occurs in addition to [Livebook's authentication](../authentication.md) for deployed notebooks and admins.

## How to

To integrate Basic Authentication with Livebook, set the `LIVEBOOK_IDENTITY_PROVIDER` environment variable to `basic_auth:<username>:<password>`, and then deploy or directly run your Livebook instance.
