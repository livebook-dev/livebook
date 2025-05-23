# Deploy Livebook Apps with Livebook Teams

This tutorial guides you through deploying a Livebook app using Livebook Teams. By the end, you'll have transformed a notebook into an interactive web app running in a local server.

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

Before deploying the app, let's preview what it looks like.

1. Click on the **app settings** icon on the sidebar
2. Click on the **Launch preview** button to run your notebook as an app
3. Click on the URL of your local app preview to open the app

You should see your app running inside Livebook. Something like this:

![app preview](images/app_preview.png)

Notice how the notebook is now running as a web app, with all code hidden, showing just the UI widgets and outputs.

## Step 3: Set up a deployment group

Before you can deploy a Livebook app, we need to configure a [deployment group](teams_concepts.md#deployment-groups)
for your Teams org. Let's do that.

First, go back to your notebook. Then:

1. Click on the **app settings** icon on the sidebar
2. Click on the **Deploy with Livebook Teams** button
3. If the notebook is not opened in the context of the workspace of your Teams organization, you'll be asked to change the workspace to your organization's workspace. Do that if needed and click the "Deploy with Livebook Teams" button again.

You should now see a form to create a deployment group, like this:

![deployment group form](images/deployment_group_form.png)

Let's fill in this form to create our deployment group.

1. For the **Name** input, let's use "local test".
2. For the **Clustering** select, change to "Single instance"
3. Click the **Add** button

Now we're ready for the last configuration step, set up a Livebook app server.

## Step 4: Set up a Livebook app server

After the step before, you should be seeing a modal like this:

![app server setup instructions](images/app_server_setup.png)

This modal contains instructions to setup an [app server](teams_concepts.md#app-server). Let's setup our app server.

The easiest way to setup an app server is to run it as a Docker container. You can do that in any
server infrastructure that runs Docker, such as Fly.io or a Kubernetes cluster.

For our tutorial, we'll setup our app server running in a local Docker container. To do that,
copy and paste the CLI instructions given by your Livebook. The instructions look something
like this:

![CLI command to run an Livebook app server with Docker](images/app_server_docker.png)

> #### Use available host ports {: .tip}
>
> You may need to change the host ports in the Docker CLI command if ports
> 8080 or 8081 are already in use on your machine. Here's how:
>
> ```
> # Instead of using the default host ports, 8080 and 8081.
> docker run -p 8080:8080 -p 8081:8081 (rest of the command...)
> ```
>
> ```
> # You can map to different host ports like 9080 and 9081
> docker run -p 9080:8080 -p 9081:8081 (rest of the command...)
> ```

Once you run the CLI command in your terminal, Docker will download Livebook image and run it. Wait until that's done.

Once the container is ready, a Livebook instance will be started and will automatically connect to your Livebook Teams organization. Your Livebook will show a message saying that it knows there's now an app server running connected to your Teams org, like this:

![feedback message saying the app server is running](images/app_server_setup_message.png)

Now we're ready to deploy our app!

## Step 5: Deploy the app

Once your app server is up and running, return to Livebook. You should see a **Deploy** button like this:

![Deploy button](images/deploy_button.png)

Click the button to deploy your Livebook app to your local app server.

Now that the app is deployed, let's access it. Assuming the host port you used in the Docker CLI command was 8080, you can access your app server at `http://localhost:8080` (adjust if necessary).

Once you access it, you will be asked to authenticate. Use the built-in "Sign in with Livebook Teams" authentication mechanism. It will use your Livebook Teams account to authenticate to your app server:

![authentication](images/app_server_authentication.png)

After you sign in, you'll be redirected to your app server, where you'll see the list of deployed
apps, like this:

![deployed apps](images/deployed_apps.png)

Notice how our deployed app is already there in the list.

To access the app, click on its title. You should now see the app running inside your app server:

![deployed app running](images/deployed_app.png)

That's it, you deployed a Livebook app with Livebook Teams! 🎉

## Deploying new versions of your app

When you want to update your deployed app:

1. Open the notebook and make your changes
2. Click on the **app settings** icon on the sidebar
3. Click on the **Deploy with Livebook Teams** button
4. Select your existing deployment group
5. Click **Deploy**

That's it! A new version of your app will be deployed to your app server.

Try it now by changing the title of your notebook and deploying it again.

## What you've learned

You've successfully deployed a Livebook app with Livebook Teams! Along the way, you:

- Transformed a Livebook notebook into an interactive web app
- Created a deployment group to organize your app deployments
- Set up a Livebook app server using Docker
- Deployed your app with a single click
- Accessed your deployed app through Teams authentication
- Learned how to update your app by redeploying

You now have the foundation to deploy Livebook apps for your team's internal tools.

## What's next?

Now that you've successfully deployed a Livebook app to a local app server, you might want to explore deploying to an app server running in the cloud. It's very similar to what we did before.

I'll leave this as an exercise for you, the path goes like this:

1. Go to the home page of your Livebook
2. In the sidebar, click on your Teams workspace
3. Scroll to the **Deployment groups** section of your workspace, you should see something like this:

![deployment groups of a Teams workspace](images/deployment_groups_inside_workspace.png)

Now, click the **Add deployment group** button and fill in the form to create a new deployment group, like we did in [Step 3](#step-3-set-up-a-deployment-group), but now let's use "staging" as the name of the deployment group.

Once you create this new deployment group, you'll see there's **+Deploy** link for that deployment
group:

![add app server to deployment group](images/add_app_server_to_deployment_group.png)

Click on the **+Deploy** link of your staging deployment group to add an app server to it. You should see instructions of different ways to set up an app server:

![instructions to set up an app server](images/instructions_setup_app_server.png)

You can use the Docker instructions to set up an app server in any cloud infrastructure that
supports Docker. There's specific instructions for Fly.io and Kubernetes. Use the one you prefer.

> **Want to understand the concepts?** If you'd like to learn more about deployment groups and app servers, check out our [concepts page](teams_concepts.md) after completing this tutorial.
