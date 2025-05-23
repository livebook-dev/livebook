# Understanding Livebook Teams concepts

This page explores the key concepts of Livebook Teams.

## Deployment groups

A deployment group serves as the organizational and configuration layer for your deployed Livebook apps. Think of it as defining a "deployment context" - a set of rules, configurations, and boundaries that govern where your apps are deployed and how they can be accessed.

The concept of deployment groups emerged from several needs:

1. **Logical separation**: A deployment group provides logical separation. One example is using different deployment groups for different environments, like staging and production. Or you can use different deployment groups for different departments of a company, like engineering and customer support.

2. **Grouping app servers**: A deployment group contains a one or more app servers.

3. **IAM Configuration**: A deployment group contains the Identity and Access Management (IAM) configuration that is applied to all app servers belonging to it.

## App server

A Livebook app server is a Livebook instance configured to act as an application server to where
you can deploy Livebook apps via Livebook Teams.

It's the same Livebook you can run on your machine, but with a few environment variables configs that makes that instance connect to an organization from Livebook Teams.

Since it's just a Livebook instance, it means you use it to create, open and run regular notebooks as well, not just as a target for deployng apps.

### The app server lifecycle

When you start an app server, it:

1. Connects to Livebook Teams servers, using its server key to connect to a specific organization
2. Waits to receive app deployments via Livebook Teams
3. Manages the lifecycle of deployed apps (starting, stopping, updating)

This architecture means app servers are essentially "deployment targets" - they don't store apps themselves but receive and run them as instructed by Livebook Teams.

### The relationship between app servers and deployment groups

A deployment group can have one or more app servers, allowing you to scale horizontally. An app server belongs to exactly one deployment group

Understanding deployment groups and app servers individually is important, but their real power comes from how they work together:

1. **The deployment group defines the "what"**: What configuration, what security rules, what environment
2. **The app server provides the "where"**: Where the apps actually run, where the compute happens
3. **Livebook Teams orchestrates the "how"**: How apps get from your Livebook to the app server

This separation of concerns makes the system both flexible and robust. You can change where apps run (different app servers) without changing what they are (deployment group configuration). You can update configuration (in the deployment group) without touching the infrastructure (app servers).
