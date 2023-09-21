# Authentication

## Introduction

Livebook's authentication covers all pages for creating, writing, and managing notebooks.

Livebook's default authentication method is token authentication.  A token is automatically generated at startup and printed to the logs.

You may optionally enable password-based authentication by setting the environment variable `LIVEBOOK_PASSWORD` on startup or deployment. It must be at least 12 characters.

To disable authentication altogether, you may set the environment variable `LIVEBOOK_TOKEN_ENABLED` to `false`.

## Securing deployed notebooks

When you deploy a notebook as an application, the deployed application is not covered by Livebook's token/password authentication. In such cases, you have two options:

  * You can set a password when deploying your notebook

  * You can enable proxy authentication when deploying inside a cloud infrastructure.
    See the "Deployment" section on the sidebar for more information
