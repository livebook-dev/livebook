# How to open notebooks from a private Git repository

This guide shows you how to configure a Git file storage in your Livebook Teams organization, allowing team members to access shared notebooks from a private Git repository inside an [app server](/teams_concepts.md#app-server).

![](images/open-from-git.png)

## Prerequisites

- Membership in a [Livebook Teams](https://livebook.dev/teams) organization
- A private Git repository (GitHub, GitLab, Bitbucket, etc.)

> #### Livebook version requirement {: .info}
> Requires Livebook v0.18 or newer.


## Add a Git file storage to your Teams workspace

1. Open Livebook
2. Navigate to your Teams workspace page by clicking on the team name in the sidebar
3. Scroll to the "File storages" section and click **Add file storage**
4. Select **Git** as the file storage type
5. Fill in the Git repository details:
   - **Repository URL**: Enter the SSH URL of your Git repository (e.g., `git@github.com:username/repo-name.git`)
   - **Branch**: Specify the branch to use (e.g., `main`)
   - **Private key**: Paste an SSH private key that has access to the repository (read-only access is enough)
6. Click **Add** to save the configuration

Once configured, the Git file storage will be synced across all team members and Livebook app servers that belong to your Teams organization.

> #### Which SSH private key should I use? {: .tip}
> Livebook connects to Git repositories over SSH, so you need to provide a private key with access to the repo.
>
> The recommended way is to create a new SSH key pair and add the public key as a deploy key for your Git repository inside your Git provider ([GitHub](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/managing-deploy-keys#set-up-deploy-keys), [GitLab](https://docs.gitlab.com/user/project/deploy_keys/#create-a-project-deploy-key), [Bitbucket](https://support.atlassian.com/bitbucket-cloud/docs/set-up-repository-access-keys-on-linux/), etc.), then use the private key for the Livebook configuration.
>
> Livebook only needs **read-only access**, so when adding the deploy key to your Git repo, you can grant read-only permissions only.

## Open a notebook from the Git repository

After adding the Git file storage, members and app servers of your Teams organization can access notebooks stored in the repository:

1. From the Livebook home page, click **Open** to open a notebook
2. Make sure you're in the **From storage** tab, and click on the file storage dropdown, saying "Disk"
3. Click on the Git file storage you configured (it will be listed with your repository URL)
4. Browse the repository files and click on the notebook file you want to open
5. Click **Fork** to open the notebook in a new session

The notebook will open and be ready to use.

It's worth noting that **Livebook treats notebooks inside a Git file storage as read-only**. That's
why you have to "fork" the notebook, which creates a copy. Any changes you make are local to your session and won't affect the original file in the Git repository.

## Examples of when this feature is useful

This feature is particularly useful when working inside an [app server](/teams_concepts.md#app-server) from your Teams organization and you need to open shared notebooks stored in a Git repository.

Common use cases include:

* **Templates and boilerplate** - Access frequently needed code inside an app server
* **Runbooks** - Open operational runbooks directly in your app server to interact with production systems

## Examples of when this feature is not needed

### Sharing notebooks with team members for local development

If your team members need to edit and collaborate on notebooks in their local Livebook instances, use standard Git workflows instead:

1. Team members clone the Git repository to their local machines
2. Open notebooks directly from the cloned repository
3. Edit, commit, and push changes using Git

**Use Git file storage when**: You need to access the notebook from an app server, since app servers don't have direct access to your Git repository.

### Deploying Livebook apps

If you want to give someone access to a Livebook app (not edit the notebook):

1. [Deploy your notebook as a Livebook app](deploy_app.md)
2. Share your app server URL with users

This allows users to use the app without notebook editing capabilities.

**Use Git file storage when**: Team members need to open shared notebooks as regular notebooks inside an app server, not as deployed apps.

## Security

Your SSH private key is encrypted end-to-end and never accessible to Livebook Teams in plaintext.

Livebook encrypts your SSH private key locally using your Teams key before sending it to Livebook Teams servers. When a Git file storage is added or edited, team members' Livebook instances and app servers receive the encrypted key and decrypt it locally using the same Teams key.

This ensures that only members of your Teams organization can access the private key.
