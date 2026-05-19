# Authentication

Livebook has three levels of authentication:

  * **Instance authentication**: this authenticates the user on all routes of your Livebook instance, including deployed notebooks and the admin section. See the "Instance authentication" section for more information.

  * **Admin authentication**: this authenticates access to Livebook admin interface within an instance, where users can create, write, and manage notebooks. Both password and token authentication are available. See the ["Admin authentication"](#admin-authentication) section for more information.

  * **Deployed notebook passwords**: additionally, when deploying notebooks as applications, each application may be password protected with a unique password. Only users authenticated as admin or with the password will be able to access them.

## Instance authentication

You can configure Livebook with different instance authentication mechanisms by setting the `LIVEBOOK_IDENTITY_PROVIDER` provider environment variable. The supported values are:

  * `basic_auth:<username>:<password>`
  * `cloudflare:<your-team-name (domain)>`
  * `google_iap:<your-audience (aud)>`
  * `tailscale:<tailscale-cli-socket-path>`
  * `custom:YourElixirModule`

See the "Zero Trust Authentication" section in the sidebar.

## Admin authentication

Livebook's default admin authentication method is token authentication. A token is automatically generated at startup and printed to the logs.

You may optionally enable password-based authentication by setting the environment variable `LIVEBOOK_PASSWORD` on startup or deployment. It must be at least 12 characters.

To disable admin authentication, you may set the environment variable `LIVEBOOK_TOKEN_ENABLED` to `false`.
