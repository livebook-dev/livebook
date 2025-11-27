# OIDC groups authorization

Livebook Teams allows you to control access to your Livebook [app servers](/docs/teams/teams_concepts.md#app-server) and Livebook apps based on group membership in your OIDC identity provider (IdP). This enables you to implement a Role-Based Access Control (RBAC) model for your Livebook deployment.

![](images/oidc_groups_auth.png)

> #### Livebook version requirement {: .info}
> Requires Livebook v0.18 or newer.

## Overview

OIDC groups authorization enables you to:

- Restrict access to your Livebook apps and app servers based on group membership
- Limit access to apps that belong to specific [app folders](app_folders.md) so groups of users only have access to what they're authorized to see
- Integrate seamlessly with your existing OIDC SSO provider (Okta, Microsoft Entra, Google etc.)

## Prerequisites

Before configuring OIDC groups authorization, ensure you have:

1. **Configured an OIDC provider** in your Livebook Teams organization. If you haven't done this yet, follow the instructions in our [OIDC SSO documentation](oidc_sso.md).

2. **Configured your OIDC identity provider** to include group information in the ID tokens:
   - **Okta**: [Customize tokens with a groups claim](https://developer.okta.com/docs/guides/customize-tokens-groups-claim/main/)
   - **Google Workspace**: Group-membersghip data is included by a mechanism that's specific to Google Workspace
   - **Others**: Search your identity provider's documentation for how to add a groups claim to the ID token

## Configuration

### 1. Enable authorization via OIDC SSO groups

To enable authorization via OIDC SSO groups for a deployment group:

1. Log in to Livebook Teams
2. Navigate to the **Deployments** panel
3. Click **Edit** on the deployment group where you want to enable authorization
4. Go to the **App server access** section (requires "admin" role in the organization)
5. Ensure that **Authenticate via Livebook Teams** is enabled
6. Enable **Authorize via OIDC SSO groups**

### 2. Add group authorization

Once authorization is enabled, you can add group access rules:

1. Click the **Add group** button
2. Select one of your configured OIDC identity providers
3. Enter the name of a group from your identity provider
4. Choose the Access type for this group:
   - Full access
   - App folder access
5. Click the **Add group** button to save the rule

You can create multiple group authorizations with different access types to set up your authorization strategy.

## Access types explained

### Full access
Users who belong to groups with full access can access:

- The app server admin interface*
- All Livebook apps deployed to this deployment group

**Note that [Admin authentication](authentication.md#admin-authentication) configuration will also apply if configured.*

### App folder access
Users who belong to groups with app folder access can only see and open apps from the selected app folders.

This allows you to implement role-based access control using groups from your identity provider.

## Testing the Configuration

To verify the configuration is working:

1. Sign out of any current Livebook Teams sessions
2. Navigate to an app server or Livebook app in the deployment group where you've enabled OIDC groups authorization
3. You will be prompted to authenticate through your OIDC provider
4. After authentication, the system will check your group membership
5. You will only be granted access if you belong to one of the configured groups with appropriate permissions

## Troubleshooting

If users are experiencing access issues:

1. Verify the group name matches exactly as defined in your IdP (group names are case-sensitive)
2. Check that the user is a member of the specified group in your IdP
3. Ensure your OIDC provider is properly set up to include group names inside the ID token
