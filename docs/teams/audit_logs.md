# How to configure audit logs for code execution

This guide shows you how to configure Livebook to log who ran what code and when.

This is useful when your team uses Livebook to execute code against production systems and needs to maintain audit logs of code execution.

## Configure audit logging

Livebook can capture three key pieces of information in audit logs:

- Timestamp
- User identity
- Code executed

Livebook includes timestamps in logs by default, but user identity and code execution need to be added via configuring the following environment variables.

### Set log metadata

Configure the `LIVEBOOK_LOG_METADATA` environment variable:

```
LIVEBOOK_LOG_METADATA="users,code,session_mode,event"
```

Each key adds the following information to the log:

- `users` - User identity (name and email)
- `code` - The code being executed
- `session_mode` - Execution context: `default` for regular notebook sessions, `app` for local app previews (different from deployed Livebook apps)
- `event` - Set to `code.evaluate` for code evaluaton log entries

### Set the log format to JSON

Configure JSON log format for easier parsing in log aggregators:

```
LIVEBOOK_LOG_FORMAT=json
```

### Set the log level to info

Code execution logs require the `info` log level or higher. Configure the log level:

```
LIVEBOOK_LOG_LEVEL=info
```

## Understand the code execution log format

After configuring the environment variables, your logs will contain entries like this:

```
{"message":"Evaluating code","time":"2025-10-06T19:19:19.851Z","metadata":{"code":"1 + 1","event":"code.evaluate","users":[{"id":"1","name":"Hugo Baraúna","email":"alice@email.com"}],"session_mode":"default"},"severity":"info"}
{"message":"Evaluating code","time":"2025-10-06T19:19:40.131Z","metadata":{"code":"query = from u in User, select: u.name\nRepo.all(query)","event":"code.evaluate","users":[{"id":"1","name":"Hugo Baraúna","email":"alice@email.com"}],"session_mode":"default"},"severity":"info"}
{"message":"Evaluating code","time":"2025-10-06T19:19:59.748Z","metadata":{"code":"Repo.all(Organization)","event":"code.evaluate","users":[{"id":"1","name":"Hugo Baraúna","email":"alice@email.com"}],"session_mode":"default"},"severity":"info"}
```

Here's one of those log entries formatted for readability:

```
{
  "message": "Evaluating code",
  "time": "2025-10-06T19:19:40.131Z",
  "metadata": {
    "code": "query = from u in User, select: u.name\nRepo.all(query)",
    "event": "code.evaluate",
    "users": [
      {
        "id": "1",
        "name": "Hugo Baraúna",
        "email": "alice@email.com"
      }
    ],
    "session_mode": "default"
  },
  "severity": "info"
}
```

Each log entry contains three key pieces of audit information:

- **When** - The `time` property contains an ISO 8601 timestamp with UTC timezone (e.g., `2025-10-03T20:49:22.231Z`).
- **Who** - The `metadata.users` array contains user objects with `id`, `name`, and `email`. Multiple users may appear when collaborating on the same notebook session.
- **Code** - The `metadata.code` property contains the exact code that was executed. Multi-line code includes newline characters (`\n`).

## Filtering code execution logs

If you're sending Livebook logs to a log aggregator, you can filter specifically for code execution events using the `metadata.event` field:

```
metadata.event: "code.evaluate"
```

This filter isolates code execution audit logs from other log entries, making it easier to track and analyze code execution activity in your monitoring tools.

## User identity sources

User identities come from the authentication methods configured in your Livebook Teams organization. The following authentication methods are supported:

* **Livebook Teams account** - Members of your Livebook Teams organization authenticate using their Teams accounts

* **Email domain** - Users authenticate using email accounts from specific domains, such as your company's Google Workspace domain

* **OpenID Connect Single Sign-On (SSO)** - Users authenticate via an OpenID Connect provider, such as Okta, Microsoft Entra, or Keycloak

When users authenticate through any of these providers, their identity information (name and email) becomes available in the audit logs.

## Logs in deployed Livebook apps

Code execution logs are **not** generated when users interact with deployed Livebook apps. This is intentional.

When someone uses a deployed Livebook app, they don't see or decide which code executes—they simply interact with an application interface. The code execution happens behind the scenes as part of the app's functionality.

Code execution audit logs are designed for contexts where users can see and choose to execute code:

* **Regular notebook sessions** - Users write and run code in notebooks
* **App previews** - Users preview how their notebook looks as an app while still in the notebook editor (using the preview button)

In these scenarios, users are making deliberate decisions about what code to execute, making audit logging meaningful and appropriate.

If you need to log events within a deployed Livebook app, use Elixir's `Logger` module directly in your notebook code to log whatever information is relevant to your use case.

## Troubleshooting

### Logs aren't appearing

If you don't see code execution logs after configuration:

1. **Verify log level** - Ensure `LIVEBOOK_LOG_LEVEL=info` is set. Code execution logs won't appear at higher levels like `warning` or `error`.

2. **Check environment variables** - Restart Livebook after setting environment variables. Changes require a restart to take effect.

3. **Confirm authentication** - User identity only appears when users are authenticated. Verify your authentication method is configured correctly in your Livebook Teams organization.

### Verify configuration is working

To confirm audit logging is configured correctly:

1. Execute a simple code cell (e.g., `1 + 1`) in a notebook
2. Check your logs for an entry with:
   - `"message": "Evaluating code"`
   - `"metadata.event": "code.evaluate"`
   - `"metadata.users"` containing your user information
   - `"metadata.code"` containing `"1 + 1"`

If all these fields appear, your audit logging is working correctly.
