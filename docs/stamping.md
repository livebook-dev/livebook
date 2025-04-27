# Notebook stamping

Livebook provides a feature called "Notebook Stamping", with the goal of enhancing security and productivity within notebooks.

Whenever you author a notebook, the contents of the notebook is signed with a secret key that belongs to your machine (which you can also find in settings). If the notebook accesses any secret or file system configuration, these permissions are stored within the stamp.

Whenever you open up a notebook stamped by you, it will retain access to secrets and file systems, and you won't have to reenable them. Whenever you open up a notebook stamped by someone else, a warning is displayed, all access is revoked, and must be explicitly enabled. However, remember that stamping only takes care of Livebook resources: when you execute the notebook, the code in the notebook will still have access to the current machine, so always execute third-party code with care.

Note that deploying notebooks as applications do not verify stamps when using your personal workspace. For such, you must use Livebook Teams, which provides an authority for stamping and encrypting notebooks.

## Secure deployments with Livebook Teams

When using Livebook Teams, notebooks are stamped with a private key that belongs to your organization/workspace. This means you can share notebooks within your organization, and if the notebook accesses any secret/file system resource, the access rules are transparently retained.

Furthermore, when deploying with Livebook Teams, Livebook guarantees that all of the notebooks belong to your organization and that the stamps are valid, eliminating the chance that someone in your organization accidentally deploys an external notebook that has not been previously reviewed by a team member.

Livebook Teams stamping works in two steps:

1. The notebook is encrypted using your Livebook Teams key and then sent to the Livebook Teams server. Since Livebook Teams do not have access to your Livebook Teams key, Livebook Teams cannot read the content of your notebooks

2. The Livebook Teams server then stamps the encrypted notebook using a private key, that is only available within Livebook Teams server. The members of your organization only have access to the public key, which validates the stamp, without giving past or future employees the option to forge stamps

The steps above ensure that the contents are only visible to your team members and only team members with access to the Livebook Teams can stamp notebooks.
