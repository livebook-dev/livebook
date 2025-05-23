# Understanding Livebook Teams concepts

This page explores the key concepts of Livebook Teams.

## Deployment group

A deployment group serves as an organizational and configuration layer for your app servers and Livebook applications. Think of it as defining a "deployment context"—a set of rules, configurations, and boundaries that determine where your apps are deployed and how they can be accessed.

A deployment group serves multiple purposes:

1. **Logical separation**: A deployment group provides logical separation. One example is using different deployment groups for different environments, like staging and production. Or you can use different deployment groups for different departments of a company, like engineering and customer support.

2. **Grouping app servers**: A deployment group contains one or more app servers.

3. **IAM Configuration**: A deployment group contains the Identity and Access Management (IAM) configuration that is applied to all app servers belonging to it.

## App server

A Livebook app server is a Livebook instance configured to act as an application server where you can deploy Livebook apps via Livebook Teams.

It's the same Livebook you can run on your machine, but with a few environment variable configurations that make that instance connect to an organization from Livebook Teams.

Since it's a standard Livebook instance, you can also use it to create, open, and run regular notebooks in addition to serving as a deployment target for apps.

### The app server lifecycle

When you start an app server, it:

1. Connects to the Livebook Teams organization, using its server key to connect to a specific deployment group
2. Downloads and runs all apps that are deployed to that deployment group
3. Waits to receive app deployments and configuration changes from its organization and deployment group

This architecture means app servers are essentially "deployment targets" - they don't store apps themselves but receive and run them as instructed by Livebook Teams.

### The relationship between app servers and deployment groups

A deployment group can have one or more app servers, allowing you to scale horizontally. An app server belongs to exactly one deployment group.

Understanding deployment groups and app servers individually is important, but their real power comes from how they work together:

1. **The deployment group defines the "what"**: What configuration, what security rules, what environment
2. **The app server provides the "where"**: Where the apps actually run, where the compute happens
3. **Livebook Teams orchestrates the "how"**: How apps get from your Livebook to the app server
