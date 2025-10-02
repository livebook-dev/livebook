# Shared file storages

This feature allows your team to share S3-compatible buckets and Git repos as Livebook file storages.

Livebook file storages are used to store notebooks and their files.

Whenever you add, update, or detach a file storage from your organization workspace, Livebook Teams will synchronize it to every Livebook instance connected to your Teams organization, including team members and app servers in deployment groups.

![](images/add_shared_file_storage.png)

## How it works

Here's a video showing how this feature works.

<iframe width="560" height="315" src="https://www.youtube-nocookie.com/embed/NkrTHShRCYE?si=9LhjlER91nyEbZxi" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>

## Synchronization scope

Shared file storages are synchronized to:

1. **Team member Livebook instances**: Every member of your organization receives the shared file storage configuration in their local Livebook instance
2. **App servers**: All app servers across all deployment groups in your organization also receive the shared file storage configuration

## Read/write permissions

File storage permissions depend on the storage type:

- **S3 file storages**: Support both read and write operations. You can create, modify, and delete notebooks and files.
- **Git file storages**: Read-only access. You can view and use notebooks and files, but cannot modify them directly through the file storage.

## Security

Livebook Teams cannot access sensitive data from your file storages, including S3 credentials or Git SSH private keys.

Livebook encrypts your file storage credentials (S3 credentials or Git SSH private keys) locally on your machine using your Teams key. The encrypted credentials are then sent to Livebook Teams servers.

When a new synchronization is needed, Livebook Teams sends the encrypted credentials to the Livebook instances of team members and app servers. These Livebook instances decrypt the credentials locally using the same Teams key.
