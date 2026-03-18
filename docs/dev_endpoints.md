# Dev endpoints

Livebook exposes a set of local HTTP endpoints under `/dev` that allow development tools to programmatically control Livebook. These are intended for editor integrations and local tooling.

## Enabling

Dev endpoints are disabled by default. To enable them, go to **Settings → Dev endpoints** and toggle **Enable dev endpoints**.

## Security

The endpoints are only accessible from local tools making direct HTTP requests. Any request that includes an `Origin` header (which browsers automatically attach) is rejected with a `403` error, preventing cross-site access from web pages.

## Endpoints

All endpoints accept and return JSON. On error, the response body is:

```json
{ "status": "error", "message": "..." }
```

### POST /dev/sync

Syncs the on-disk file into the running session. If the file content differs from the in-memory notebook, the session is updated to reflect the changes.

**Request body:**

```json
{ "file": "/absolute/path/to/notebook.livemd" }
```

**Success response `200`:**

```json
{ "status": "ok" }
```

**Error response `404`** — no session is open for the given file.

### POST /dev/open

Opens a notebook file in Livebook. If a session for the file is already open, it is returned as-is. Otherwise, the file is read and a new session is created.

**Request body:**

```json
{ "file": "/absolute/path/to/notebook.livemd" }
```

**Success response `200`:**

```json
{ "path": "/sessions/abc123" }
```

The `path` can be appended to Livebook's base URL to navigate directly to the session.

**Error response `422`** — the file could not be read (e.g. it does not exist).

### POST /dev/restamp

Takes two versions of a notebook source:

- `old_source` - original notebook source with valid stamp (or none)
- `new_source` - updated notebook source with outdated stamp (or none)

The response includes the new source with a fresh stamp, if applicable. Any information stored in the `old_source` stamp (such as names of the enabled secrets) is also transferred to the new stamp.

Note that if `new_source` already has a valid stamp, it is returned as is.

This is endpoint is useful when a tool rewrites a notebook file and the stamp needs to be updated, for example, to deploy the notebook as a Livebook App.

**Request body:**

```json
{
  "old_source": "# Notebook\n\n...",
  "new_source": "# Notebook\n\n..."
}
```

**Success response `200`:**

```json
{ "source": "# Notebook\n\n..." }
```

**Error response `422`** — `old_source` contains an invalid stamp.

## Usage tips

### Watching notebook files

If you prefer to edit notebooks using your usual editor, a convenient workflow may be to setup a file watcher and automatically synchronize the changes into the corresponding session open in Livebook.

For example, to watch all notebooks in the current directory using `fswatch`, you can run:

```shell
fswatch *.livemd | xargs -I{} curl -X POST http://localhost:32123/dev/sync -H 'content-type: application/json' -d '{"file":"{}"}'
```

### Claude Code hooks

If you are editing notebooks using Claude Code, you can add hooks to automatically synchronize file changes into the corresponding session open in Livebook.

**.claude/settings.json**

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/livebook.sh prewrite"
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": ".claude/hooks/livebook.sh postwrite"
          }
        ]
      }
    ]
  }
}
```

**.claude/hooks/livebook.sh**

```shell
#!/bin/bash

livebook_url="http://localhost:32123"

file_path=$(jq -r '.tool_input.file_path // empty')

[[ "$file_path" == *.livemd ]] || exit 0

call_endpoint() {
  local endpoint="$1"
  local error
  error=$(curl -sfS -X POST "$endpoint" \
    -H 'Content-Type: application/json' \
    -d "{\"file\": \"$file_path\"}" 2>&1)
  if [[ $? -ne 0 ]]; then
    jq -n --arg msg "Livebook hook error: $error" '{systemMessage: $msg}'
    exit 0
  fi
}

case "$1" in
  prewrite)
    [[ -f "$file_path" ]] && call_endpoint "$livebook_url/dev/open"
    ;;
  postwrite)
    call_endpoint "$livebook_url/dev/open"
    call_endpoint "$livebook_url/dev/sync"
    ;;
  *)
    jq -n --arg msg "Usage: $0 prewrite|postwrite" '{systemMessage: $msg}'
    exit 0
    ;;
esac

exit 0
```

### Git-based restamping

If you store notebooks in a Git repository, you can use the following script to restamp changed notebooks. The script assumes that the committed notebooks have valid stamps and the changed notebooks are not yet committed.

```elixir
Mix.install([
  {:req, "~> 0.5.17"}
])

livebook_url = System.get_env("LIVEBOOK_URL", "http://localhost:32123")

{output, 0} = System.cmd("git", ["diff", "--name-only", "--diff-filter=M", "HEAD"])
paths =  String.split(output, "\n", trim: true)

for path <- paths, Path.extname(path) == ".livemd" do
  {old_source, 0} = System.cmd("git", ["show", "HEAD:#{path}"])
  new_source = File.read!(path)

  %{status: 200, body: %{"source" => restamped_source}} =
    Req.post!("#{livebook_url}/dev/restamp",
      json: %{old_source: old_source, new_source: new_source}
    )

  if restamped_source != new_source do
    File.write!(path, restamped_source)
    IO.puts("Restamped: #{path}")
  end
end
```
