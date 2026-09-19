# omp (`~/.omp/agent`) — portable bits only

`models.yml` here is a verbatim copy of `~/.omp/agent/models.yml` on the old
machine. Install it with:

```sh
cp omp/models.yml ~/.omp/agent/models.yml
```

## Secrets stay in the environment

The file lists **environment variable names**, never literals:

| provider key | env var it reads | where to set it |
|---|---|---|
| `bai` (`https://api.b.ai/v1`) | `B_API_TOKEN` | shell rc, or `~/.config/environment.d/` |
| `xkiro` (`https://api.xkiro.com/v1`) | `XKIRO_API_KEY` | same |

Nothing else is needed for the file to parse; the providers simply stay
unavailable until those variables exist. Both are OpenAI-compatible proxies,
hence `api: openai-completions` + `disableStrictTools: true` (the Anthropic
proxies reject OpenAI's strict JSON-schema tool calling).

## What is deliberately NOT copied

Everything else under `~/.omp/agent/` is machine state, not config — copying it
across machines is what you don't want:

| path | what it is | why to skip |
|---|---|---|
| `agent.db`, `history.db`, `models.db` (+ `-wal`, `-shm`) | sqlite databases | per-machine state; WALs beside a freshly copied DB can corrupt it |
| `sessions/` | agent transcripts (~500 MB on the old box) | huge, machine-specific |
| `blobs/`, `cache/` | cached model/tool payloads | regenerated |
| `tools/` | installed tool payloads (~39 MB) | regenerated |
| `skills/` | installed skills | copy by hand only if you actually curated them |
| `terminal-sessions/` | recorded terminal state | machine-specific |
| `config.yml` | `modelRoles`, `setupVersion`, `defaultThinkingLevel` | contains no secrets; copy it by hand if you want the same model roles (it is not in this repo) |

The `-wal`/`-shm` files in particular must never be copied without their
matching `.db` — they are live write-ahead logs.
