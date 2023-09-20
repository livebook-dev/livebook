# Token authentication

Livebook's default authentication method is token authentication.

A token is automatically generated at startup and printed to the logs. The token can be customized
by setting the environment variable `LIVEBOOK_PASSWORD`, and must be at least 12 characters.

To disable token authentication, set the environment variable `LIVEBOOK_TOKEN_ENABLED` to `false`.