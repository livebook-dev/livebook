# Deploy Livebook Apps with Livebook Teams

This tutorial guides you through deploying a Livebook app to a server using Livebook Teams. By the end, you'll have transformed a notebook into an interactive web app running in a
server.

## Prerequisites

Before you begin, make sure you have:

- A [Livebook Teams](https://livebook.dev/teams) account
- Membership in a Livebook Teams organization
- Docker installed

If you're not part of Teams org, here's a [one-minute video showing how to create one](https://www.youtube.com/watch?v=Ox8-JT0JHO4).

## Step 1: Open our example notebook

For this tutorial, we'll use our example notebook that contains a Livebook app.

1. Open Livebook in your browser
2. Click on the **Open** button, to open a notebook
3. Click on the **From URL** tab, for opening a notebook from a URL
4. Fill in the notebook URL input with the URL of our example app: `https://teams.livebook.dev/notebooks/github_stars`
5. Click the **Import** button

You should see the "GitHub Stars" notebook opened:

![github stars livebook notebook](images/github_stars_notebook.png)

## Step 2: Preview the app locally

Before deploying the app, let's preview how it looks like.

1. Click on the **app settings** icon on the sidebar
2. Click on the **Launch preview** button to run your notebook as an app
3. Click on the URL of your local app preview to open the app

You should see your app running inside Livebook. Something like this:

![app preview](images/app_preview.png)

Notice how the notebook is now running as a web app, with all code hidden, showin just the UI
widgets and outputs.

## Step 3: Set up a deployment group

Before being able to deploy a Livebook app, we need to configure configure a deployment group
for your Teams org. A deployment group is TO-DO. Let's do that.

Firt, go back your the notebook.

1. Click on the **app settings** icon on the sidebar
2. Click on the **Deploy with Livebook Teams** button
3. If the notebook is not opened in the context of the workspace of your Teams organization, you'll be asked to change the workspace to your. Do that if needed and click the "Deploy with Livebook Teams" button again.

You should now see a form to create a deployment group, like this:

![deployment group form](images/deployment_group_form.png)

Let's fill in this form and create our deployment group.

1. Give it a name to yor deployment group, let's say "local test".
2. Click the **Add** button

Now we're ready for the last configuration step, set up a Livebook app server.

## Step 4: Set up a Livebook app server

After the step before, you should be seeing a modal like this:

![app server setup instructions](images/app_server_setup.png)

This modal contains instructions to setup an app server. A Livebook app server is an instance of Livebook connected to your Livebook Teams organization. Once setup, you can deploy Livebook apps
to that app server via Livebook Teams.

Let's setup our app server.

The easiest way to setup an app server is to run it as a Docker container. You can do that in any
server infrastructure that runs Docker, such as Fly.io or a Kubernetes cluster.

For our tutorial, we'll setup our app server running in a local Docker container. To do that,
copy and paste the CLI instructions given by your Livebook. The instructions look something
like this:

![CLI command to run an Livebook app server with Docker](images/app_server_docker.png)

Docker will download Livebook image and run it. Wait until that's done.

Once the container is ready, that Livebook instance will connect your Livebook Teams organization, as an app server part of the deployment group you just created. Livebook will show
you a message saying the container is running, like this:

![feedback message saying the app server is running](images/app_server_setup_message.png)

Now we're ready to deploy our app!

## Step 5: Deploy the app

Once your app server is up and running:

1. Return to Livebook
2. You should see a **Deploy** button - click it

![Click Deploy button](images/click_deploy.png)

3. Wait for the deployment to complete
4. When finished, you'll see a URL where your app is now running

![Deployment complete with URL](images/deployment_complete.png)

Click the URL to open your deployed app. You now have a Livebook app running in production!

![Running deployed app](images/deployed_app_running.png)

Notice how the URL shows your app is now running on your Fly.io infrastructure, not on your local machine. Your team members can access this URL to use the app you've created.

## Making updates

When you want to update your deployed app:

1. Open the notebook and make your changes
2. Click **Deploy with Livebook Teams**
3. Select your existing deployment group
4. Click **Deploy**

That's it! Your changes will be pushed to production immediately. Try it now by making a small change to your notebook, like adding exclamation marks to a text cell, and redeploying.

## What's next?

Now that you've successfully deployed a Livebook app, you might want to explore:

- Creating more complex apps with custom widgets
- Setting up different authentication methods
- Connecting your app to databases and APIs
- Building internal tools for your team's specific needs

Check out our other examples in the Learn section to see what else you can build with Livebook apps.

Happy building!
