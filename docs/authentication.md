# Authentication

Livebook has three levels of authentication:

  * Instance authentication: this authenticates the user on all routes of your Livebook instance, including deployed notebooks and the admin section. If you are deploying your Livebook instances with [Livebook Teams](https://livebook.dev/teams), all instances include Livebook Teams authentication by default. You can also manually configure your Livebook instance to have their own authentication, see the "Airgapped Authentication" section.

  * Admin authentication: this authenticates access to Livebook admin interface, where users can create, write, and manage notebooks. Both password and token authentication are available. See the next section for more information.

  * Deployed notebook passwords: additionally, when deploying notebooks as applications, each application may be password protected with a unique password. Only users authenticated as admin or with the password will be able to access them.

## Admin authentication

Livebook's default admin authentication method is token authentication. A token is automatically generated at startup and printed to the logs.

You may optionally enable password-based authentication by setting the environment variable `LIVEBOOK_PASSWORD` on startup or deployment. It must be at least 12 characters.

To disable authentication altogether, you may set the environment variable `LIVEBOOK_TOKEN_ENABLED` to `false`.
