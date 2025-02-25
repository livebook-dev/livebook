# Authentication via email domain

Email domain authentication allows you to authenticate users based on their email domain when they access your app servers and Livebook apps. For example, your users can access your Livebook apps using their company email accounts.

![](images/email_domain_auth.png)

Currently, this feature supports only Google emails. If you need to use another email provider, [contact our support team](mailto:support@livebook.com?subject=Feature%20request%3A%20Additional%20email%20provider).

> #### Livebook version requirement {: .info}
> Requires Livebook v0.15 or newer.

## Demo
Here's a quick demo of the feature:

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/KOdFgN2MqNA?si=2j-yNhWE8IzvY9-A" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

## Configuration

### 1. Register your email domain
<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/l8SAxj62Pl8?si=2qZcLgMU7u9TH_Oy" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

To set up email domain authentication for your organization:

1. Log in to Livebook Teams.
2. Navigate to the **Authentication** panel.
3. Find the **Email domain** section.
4. Click **Configure**.
5. Enter your company's email domain(s):
   - Add one domain per line (e.g., `acme.com`)
   - You can add multiple domains if needed
6. Save your changes.

### 2. Configure your deployment group to authenticate via Livebook Teams
To enable email domain authentication, your deployment group must be configured to use authentication via Livebook Teams.

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/27GImleM3MQ?si=jlsa7cGvIgnM4xmN" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

In order to do so, follow these steps:

1. Log in to Livebook Teams.
2. Navigate to the **Deployments** panel.
3. Click **Edit** on the deployment group where you want to enable authentication.
4. Ensure that **Authenticate via Livebook Teams** is enabled.
5. Save your changes.

### Test the integration
To verify the integration is working, follow these steps:

1. Navigate to an application deployed in a deployment group configured with email domain authentication.
2. The app server will redirect you to Livebook Teams for authentication.
3. On the authentication page, you will see an option to sign in using your email provider.
4. Click the name of your email provider and follow the authentication steps.
5. Once authentication is complete, your email provider will redirect you back to your app server, and you will be successfully signed in.
