# OIDC groups authorization

Livebook Teams allows you to control access to your app servers and Livebook apps based on group membership in your OIDC identity provider (IdP).

![](images/oidc_groups_auth.png)

> #### Livebook version requirement {: .info}
> Requires Livebook v0.16 or newer.

## Overview

OIDC groups authorization enables you to:

- Restrict access to your Livebook apps and app servers based on group membership
- Integrate seamlessly with your existing OIDC SSO provider (Okta, Microsoft Entra, etc.)

## Prerequisites

Before configuring OIDC groups authorization, ensure you have:

1. **Configured an OIDC provider** in your Livebook Teams organization. If you haven't done this yet, follow the instructions in our [OIDC SSO documentation](oidc_sso.md).

2. **Configured your OIDC identity provider** to include group information in the ID tokens:
   - **Okta**: [Customize tokens with a groups claim](https://developer.okta.com/docs/guides/customize-tokens-groups-claim/main/)

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
4. Click the **Add group** button to save the rule

You can add multiple groups with different access types to configure your authorization strategy.

## Access types explained

### Full access

Users who belong to groups with full access can access:
- The app server admin interface*
- All Livebook apps deployed to this deployment group

**Notice that [Admin authentication](authentication.md#admin-authentication) configs will also apply if configured.*

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
