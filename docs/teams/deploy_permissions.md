# Deploy permissions

Deploy permissions allow you to control who can deploy apps to your deployment groups.

## Configuration

### Enable deployment restrictions

To enable deploy permissions for a deployment group:

1. Log in to Livebook Teams
2. Navigate to the **Deployments** panel
3. Click **Edit** of the deployment group where you want to configure deploy permissions
4. Click **Deploy permissions** in the sidebar
5. Toggle **Restrict deployment access** to enable the feature

Once enabled, only explicitly authorized users and organization tokens will be able to deploy apps to this deployment group.

### Add user permissions

To authorize individual users for deployment:

1. In the **Users** section, click **Add user**
2. Select the user from your organization members
3. Click **Authorize users** to grant them deployment permissions

### Add organization token permissions

Organization tokens are commonly used for [CI/CD pipelines and automated deployments](deploy_via_cli.md). To authorize an organization token:

1. In the **Org tokens** section, click **Add org token**
2. Select the organization token you want to authorize
3. Click **Authorize org tokens** to grant them deployment permissions

## Use cases

### Allow deployment only via CI/CD pipeline

Set up automated deployments while restricting manual deployments:

1. Create an organization token for your CI/CD system
2. Enable **Restrict deployment access** for a deployment group
3. Add only the CI/CD organization token to **Org tokens**
4. Remove or limit user-level deployment permissions

### Deployment permissions per environment

Use different deployment permission policies across environments:

- **Staging deployment group**: Allow developers to directly deploy apps from their Livebooks for rapid iteration
- **Production deployment group**: Restrict deployments only to a CI/CD org token
