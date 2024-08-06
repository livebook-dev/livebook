# Shared secrets management

## Overview

This feature allows your team to share Livebook secrets among team members in an easy way.

Just add a secret to your organization workspace, and Livebook Teams will synchronize it with the Livebook of every member of your organization.

![](images/add_shared_secret.png)

You can also create such secrets directly from a notebook.

![](images/add_shared_secret_from_notebook.png)

Shared secrets updates and deletions will also be synchronized.

## How it works

Here's a video showing how that feature works.

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/GENSmArO1AI?si=pvMJt1Ihr5UBCkPP" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

## Security strengths

Livebook Teams cannot access the plain text version of your organization's secrets.

Livebook encrypts your secrets locally in your machine using your Teams key. Then, the secret is sent encrypted to Livebook Teams servers.

When a new synchronization is needed, Livebook Teams sends the encrypted secret to the Livebook of team members, and their Livebook decrypts the secret in their local machines using the same Teams key.
