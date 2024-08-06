# Shared file storages

## Overview

This feature allows your team to share the configuration of S3-compatible storages.

Livebook file storages are used to store notebooks and their files.

Whenever you add (update or detach) a file storage to your organization workspace, Livebook Teams will synchronize that with the Livebook of every member of your organization.

![](images/add_shared_file_storage.png)

## How it works

Here's a video showing how that feature works.

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/NkrTHShRCYE?si=9LhjlER91nyEbZxi" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

## Security strengths

Livebook Teams cannot access the credentials of your S3 (compatible) account.

Livebook encrypts your S3 credentials locally in your machine using your Teams key. Then, they're sent encrypted to Livebook Teams servers.

When a new synchronization is needed, Livebook Teams sends the encrypted credentials to the Livebook of team members, and their Livebook decrypts that in their local machines using the same Teams key.
