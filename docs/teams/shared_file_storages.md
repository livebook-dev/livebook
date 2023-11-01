# Shared file storages

## Overview

This feature allows your team to share the configuration of S3-compatible storages.

Livebook file storages are used to store notebooks and their files.

Whenever you add (update or detach) a file storage to your organization hub, Livebook Teams will synchronize that with the Livebook of every member of your organization.

![](assets/add_shared_file_storage.png)

## How it works

< ADD A VIDEO HERE >

## Security strengths

Livebook Teams cannot access the credentials of your S3-compatible account.

Livebook encrypts your S3-compatible credentials locally in your machine using your Teams key. Then, they're sent encrypted to Livebook Teams servers.

When a new synchronization is needed, Livebook Teams sends the encrypted credentials to the Livebook of team members, and their Livebook decrypts that in their local machines using the same Teams key.
