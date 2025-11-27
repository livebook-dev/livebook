# OIDC SSO (OpenID Connect Single Sign-On)

Livebook Teams supports OpenID Connect (OIDC) Single Sign-On, allowing you to authenticate users through your existing Identity Provider (IdP) when they access your app servers and Livebook apps.

![](images/auth_via_teams_oidc.png)

> #### Livebook version requirement {: .info}
> Requires Livebook v0.18 or newer.

## Demo

Here's a quick demo of the feature working.

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/rG6OKethdJg?si=qofGf8M10F21IWd5" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

## 1. OIDC configurations

To integrate Livebook Teams with your OIDC SSO provider, follow these steps.

<!-- tabs-open -->

### OIDC IdPs

Follow these instructions if you're using Okta, Microsoft Entra, Keycloak or any other OIDC-compliant Identity Provider.

#### 1.1 Register with your OIDC provider

Go to your IdP admin, and register Livebook Teams as a Relying Party (client/app). This is the info you’ll need from Livebook Teams:

- **Redirect URI**: The Livebook Teams sign-in callback URL that must be registered with your IdP: ` https://teams.livebook.dev/identity/callbacks/oidc`
- **Post Logout Redirect URI**: The Livebook Teams URL that must be registered with your IdP to redirect users after logout: `https://teams.livebook.dev/identity/logout`
- **Required OIDC scopes**: The OIDC scopes Livebook Teams requires: `openid`, `profile`, `email`, `offline_access`

Once that’s done, you should retrieve the following information from the registration process:

- **Client ID**: A unique identifier assigned to Livebook Teams by your IdP
- **Client Secret**: A confidential key assigned to Livebook Teams by your IdP
- **Discovery URL**: This is the OIDC metadata URL provided by your IdP. Usual format: `https://YOUR_IDP/.well-known/openid-configuration`

#### 1.2 Configure OIDC in Livebook Teams

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/dNUcRD0A6kU?si=eNQ55-Aeg4I8PPgy" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

1. Log in to Livebook Teams
2. Go to the **Authentication** panel (requires "admin" role in the organization)
3. Click **Add OIDC SSO** and provide the following details:
	- **Name**: A display name for your OIDC provider (e.g., Okta, Microsoft Entra, Keycloak)
	- **Discovery URL**: Use the discovery URL retrieved from your IdP
	- **Client ID**: Use the Client ID retrieved from your IdP
	- **Client Secret**: Use the Client Secret retrieved from your IdP
	- **Enable this SSO provider**: Make sure this option is enabled
4. Save

### Google Workspace

To configure OIDC SSO with Google Workspace, follow the steps below. You'll need a Google Workspace account with the super admin role.

#### 1.1 Enable Cloud Identity API

1. Go to the [Google Cloud Console](https://console.cloud.google.com/)
2. Select your project or create a new one if needed
3. Navigate to **APIs & Services** > **Library**
4. Search for **Cloud Identity**
5. Click **Enable**

#### 1.2 Create service account

1. Navigate to **IAM & Admin** > **Service Accounts**
2. Click **Create Service Account**
3. Configure the service account:
    - **Service account name**: `livebook-teams` (or your preferred name)
    - **Service account description**: Service account for Livebook Teams to access Google Workspace groups
4. Click **Create and Continue**
5. Skip the optional steps by clicking **Continue** and then **Done**

#### 1.3 Get the unique ID of the service account

1. In the **Service Accounts** list, click on the service account you just created
2. In the **Details** tab, locate the **Unique ID** under the service account details
3. Copy this **Unique ID** (you'll need it in the next step)

#### 1.4 Create a JSON key for the service account

1. While still on your service account page, go to the **Keys** tab
2. Click **Add Key** > **Create new key**
3. Select **JSON** as the key type
4. Click **Create**
5. The JSON key file will be downloaded automatically (save it securely)

#### 1.5 Enable domain-wide delegation

1. Go to your [Google Workspace Admin Console](https://admin.google.com/) (requires super administrator access)
2. Navigate to **Main menu** > **Security** > **Access and data control** > **API controls**
3. In the **Domain wide delegation** section, click **Manage Domain Wide Delegation**
4. Click **Add new**
5. Configure the delegation:
    - **Client ID**: Paste the unique ID from step 1.3
    - **OAuth scopes**: Enter the following scope (this grants read-only access to group information): `https://www.googleapis.com/auth/cloud-identity.groups.readonly`
6. Click **Authorize**

#### 1.6 Configure Google Workspace OIDC in Livebook Teams

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/dNUcRD0A6kU?si=eNQ55-Aeg4I8PPgy" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

1. Log in to Livebook Teams
2. Go to the **Authentication** panel (requires "admin" role in the organization)
3. Click **Add OIDC SSO** and provide the following details:
    - **Name**: A display name for your OIDC provider (e.g., Google Workspace)
    - **Discovery URL**: `https://accounts.google.com/.well-known/openid-configuration`
    - **Admin E-mail**: Email address of a Super Admin user of your Google Workspace (e.g., `admin@yourcompany.com`). This user will be impersonated by the
  service account to read group memberships, but access is limited to the scopes authorized in step 1.5.
    - **Allowed domains**: Your Google Workspace domain(s), one per line if multiple (e.g., `yourcompany.com`)
    - **Service Account JSON Key**: Paste the entire contents of the JSON key file from step 1.4
    - **Enable this SSO provider**: Make sure this option is enabled
4. Save

<!-- tabs-close -->

### 2. Configure your deployment group to authenticate via Livebook Teams

To enable OIDC authentication, your deployment group must be configured to use authentication via Livebook Teams.

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/27GImleM3MQ?si=jlsa7cGvIgnM4xmN" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

To do so, follow these steps:

1. Log in to Livebook Teams
2. Navigate to the **Deployments** panel
3. Click **Edit** on the deployment group where you want to enable authentication
4. Ensure that **Authenticate via Livebook Teams** is enabled
5. Save your changes

### Test the integration with your OIDC IdP

To verify the integration is working, follow these steps:

1. Navigate to an application deployed in a deployment group configured for OIDC authentication.
2. The app server will redirect you to Livebook Teams for authentication
3. On the authentication page, you will see an option to sign in using your configured OIDC provider
4. Click the name of your OIDC SSO configuration and follow the authentication steps provided by your IdP
5. Once authentication is complete, the IdP will redirect you back to your app server, and you will be successfully signed in
