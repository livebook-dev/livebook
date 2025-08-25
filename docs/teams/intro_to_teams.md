# Intro to Livebook Teams

[Livebook Teams](https://livebook.dev/teams/?ref=docs) enables you to deploy notebooks as internal apps or turn Livebook into a controlled environment for runbooks and production operations.

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/lwLx5beXxsg?si=husANqYhTc3rXAZS" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

Livebook Teams integrates with Livebook, offering the following feautures on top of it:

- Deploy notebooks as internal apps to your infrastructure
  - [Deploy Livebook apps from Livebook](deploy_app.md)
  - [Deploy Livebook apps from the CLI](deploy_via_cli.md)
- Deployment authorization
  - [Authorize who can deploy Livebook apps to your servers](deploy_permissions.md)
- Authenticate who can can access your Livebook app servers and Livebook apps via multiple identity providers:
	- Livebook Teams
	- [Email-domain (Google, Microsoft, etc)](email_domain.md)
	- [OIDC (Okta, Microsoft Entra, etc)](oidc_sso.md)
- Authorization
	- [based on groups from an OIDC identity provider (Okta, Microsoft Entra, etc)](oidc_groups.md)
- Audit logs
  - Log to `STDOUT` the identity of the authenticaded person, what piece of code they executed in a notebook inside your Livebook app server, and when
- [Shared Livebook secrets](shared_secrets.md)
- [Shared Livebook file storages](shared_file_storages.md)

You can [start a free trial here](https://livebook.dev/teams/?ref=docs).
