# Authentication

Livebook has three levels of authentication:

  * **Instance authentication**: this authenticates the user on all routes of your Livebook instance, including deployed notebooks and the admin section. We provide a variety of authentication options here, including Single Sign On (SSO) and domain-based authentication via [Livebook Teams](https://livebook.dev/teams), as well as Zero Trust Authentication for airgapped environments. See the "Instance authentication" section for more information.

  * **Admin authentication**: this authenticates access to Livebook admin interface within an instance, where users can create, write, and manage notebooks. Both password and token authentication are available. See the "Admin authentication" section for more information.

  * **Deployed notebook passwords**: additionally, when deploying notebooks as applications, each application may be password protected with a unique password. Only users authenticated as admin or with the password will be able to access them.

## Instance authentication

When using [Livebook Teams](https://livebook.dev/teams), you can easily deploy instances of Livebook to run as application servers or as development servers. Those instances will, by default, use Livebook Teams to authenticate. The following authentication methods are supported by Livebook Teams:

* Using your Livebook Teams account: only users with a Livebook Teams account can access the instance

* Using your existing Identity Provider via [OpenID Single Sign-On](/oidc_sso.html)

* By validating users based on their domain, such as Google Workspaces

If your application servers must run in an airgapped environments and cannot reach out to Livebook Teams servers, we also provide a variety of options that can be configured directly in your Dockerfiles. See the "Airgapped Authentication" section in the sidebar.

## Admin authentication

Livebook's default admin authentication method is token authentication. A token is automatically generated at startup and printed to the logs.

You may optionally enable password-based authentication by setting the environment variable `LIVEBOOK_PASSWORD` on startup or deployment. It must be at least 12 characters.

To disable admin authentication, you may set the environment variable `LIVEBOOK_TOKEN_ENABLED` to `false`.
