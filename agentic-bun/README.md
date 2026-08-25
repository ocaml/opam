# Agentic Bun Framework

Run **many Claude Code agents at once against a local model**, from a single Bun
process. Every agent is a real `claude` CLI process launched through
`ollama launch claude`, so each one gets Claude Code's full built-in tool loop —
file I/O, shell, search — pointed at a model running on your own machine.

The framework owns everything *around* those processes: the fleet definition,
dependency ordering, concurrency, prompt templating, passing one agent's output
into the next agent's prompt, streaming observability, retries, timeouts and
artifacts.

```
agentic run examples/review-fleet.json
        │
        ├── preflight ── is Ollama up? which models? warm the model
        │
        ├── layer 1 ── implement ────────────────┐
        │                                        │  final text →
        ├── layer 2 ── review_correctness ┐      │  blackboard →
        │              review_tests       ├──────┤  next prompt
        │              review_style       ┘      │
        │              (3 processes at once)     │
        └── layer 3 ── fix ──────────────────────┘
```

## Why it spawns Claude Code instead of implementing its own tool loop

`ollama launch claude` (Ollama v0.15+) starts Anthropic's Claude Code CLI wired
to a local Ollama model. Claude Code already has a complete, battle-tested
chat + tool-calling loop. Writing another one inside this framework would mean
competing with the harness we are spawning — two loops, two tool schemas, two
sets of permissions.

So the split is: **Claude Code decides what to do inside one agent; this
framework decides which agents exist, what they are told, in what order they
run, and what happens to their answers.**

Ollama's own HTTP API is used for exactly three things — checking the server is
up, listing models, and warming a model into memory. No inference traffic goes
through this code.

## Requirements

| | |
| --- | --- |
| [Bun](https://bun.sh) | >= 1.1 |
| [Claude Code](https://claude.com/claude-code) | `claude` on `PATH` |
| [Ollama](https://ollama.com) | v0.15+ for `ollama launch`; older versions work in `direct` mode |
| A tool-calling model | e.g. `ollama pull qwen2.5-coder` — Claude Code is useless without tool calls |

Zero runtime dependencies. TypeScript and `@types/bun` are dev-only.

```sh
cd agentic-bun
bun install
bun run bin/agentic.ts doctor
```

`doctor` tells you exactly what is missing:

```
bun            1.3.11
claude CLI     ok
ollama binary  ok
ollama launch  supported
ollama server  up at http://127.0.0.1:11434 (v0.15.2)
models         llama3.1:8b, qwen2.5-coder:latest
suggested      qwen2.5-coder:latest
✔ ready to run a fleet
```

## Quickstart

```sh
# One task, four agents in parallel plus a synthesiser
bun run bin/agentic.ts run -p "audit src/ for unchecked error paths" -n 4

# Answer a few questions instead
bun run bin/agentic.ts wizard

# Run a plan file, and see the exact commands first
bun run bin/agentic.ts run examples/review-fleet.json --dry-run
bun run bin/agentic.ts run examples/review-fleet.json --var task="add retry to the uploader"
```

Install the `agentic` command globally with `bun link`, or keep using
`bun run bin/agentic.ts`.

## Commands

```
agentic run [plan.json]   Run a fleet from a plan file, or from -p with a preset
agentic wizard            Answer a few questions, get a plan, optionally run it
agentic doctor            Check bun, claude, ollama, launch support and models
agentic models            List installed Ollama models
agentic init [file]       Write an example plan
```

Key flags (`agentic help` has them all):

| flag | meaning |
| --- | --- |
| `-p, --prompt <text>` | Task text; builds a plan from a preset |
| `--preset <name>` | `fanout` (default), `pipeline`, `review`, `solo` |
| `-n, --agents <n>` | Agents in the preset |
| `-m, --model <name>` | Ollama model |
| `-c, --concurrency <n>` | Agents running at once (default 3) |
| `--mode <m>` | `auto` (default), `launch`, `direct` |
| `--permission-mode <m>` | `default`, `acceptEdits` (default), `bypassPermissions`, `plan` |
| `--var k=v` | Template variable, repeatable |
| `--dry-run` | Print the resolved command per agent and exit |
| `--json` | Machine-readable result on stdout (logs go to stderr) |
| `--fail-fast` | Stop the whole fleet on the first failure |

Exit codes: `0` success, `1` at least one agent failed, `2` bad usage or plan.

### Presets

| preset | shape |
| --- | --- |
| `solo` | one agent does the whole task |
| `fanout` | N workers on different angles → one synthesiser |
| `pipeline` | N agents in a chain, each building on the last |
| `review` | implementer → parallel reviewers → fixer |

## Plan files

A plan is JSON (comments allowed) describing a DAG of agents.

```jsonc
{
  "version": 1,
  "name": "review-fleet",
  "concurrency": 3,
  "vars": { "task": "add input validation to the config loader" },
  "agents": [
    {
      "id": "implement",
      "prompt": "Implement this:\n\n{{vars.task}}\n\nList every file you changed."
    },
    {
      "id": "review",
      "dependsOn": ["implement"],
      "permissionMode": "plan",          // read-only reviewer
      "prompt": "Review this work:\n\n{{outputs.implement}}\n\nConcrete defects only."
    },
    {
      "id": "fix",
      "dependsOn": ["review"],
      "prompt": "Apply the correct findings:\n\n{{outputs.review}}"
    }
  ]
}
```

### Fleet fields

| field | default | meaning |
| --- | --- | --- |
| `model` | first tool-capable installed | Ollama model for every agent |
| `ollamaHost` | `http://127.0.0.1:11434` | where Ollama listens |
| `launchMode` | `auto` | `auto` \| `launch` \| `direct` |
| `launchCommand` | `["ollama","launch","claude","--model","{{model}}"]` | escape hatch if Ollama's flags change |
| `promptVia` | `stdin` | `stdin` \| `arg` |
| `concurrency` | `3` | agents in flight at once |
| `failFast` | `false` | cancel the rest on the first failure |
| `cwd` | process cwd | working directory for agents |
| `artifactsDir` | `<cwd>/.agentic/runs` | where transcripts and summaries go |
| `vars` | `{}` | values for `{{vars.*}}` |
| `permissionMode`, `maxTurns`, `timeoutMs`, `retries`, `allowedTools`, `disallowedTools`, `systemPrompt`, `env` | see below | defaults every agent inherits |

### Agent fields

`id` and `prompt` are required. Everything else — `role`, `model`, `cwd`,
`addDirs`, `systemPrompt`, `allowedTools`, `disallowedTools`, `permissionMode`,
`maxTurns`, `timeoutMs`, `retries`, `dependsOn`, `outputKey`, `env` — overrides
the fleet default for that one agent.

Agent defaults: `permissionMode: acceptEdits`, `maxTurns: 24`,
`timeoutMs: 600000`, `retries: 1`.

> `acceptEdits` rather than `default`, because a `--print` agent cannot answer a
> permission prompt — under `default` every write silently fails. Use
> `permissionMode: "plan"` for agents that should only read, and
> `bypassPermissions` only when you have read the plan you are running.

### Templating

Prompts are rendered just before the agent is spawned, so they can quote work
that has already finished:

| placeholder | source |
| --- | --- |
| `{{vars.x}}` | the plan's `vars`, plus `--var x=…` |
| `{{outputs.<agentId>}}` | that agent's final answer |
| `{{board.<key>}}` | blackboard, including any `outputKey` publications |
| `{{env.X}}` | process environment |
| `{{agent.id}}`, `{{agent.role}}`, `{{fleet.name}}` | run metadata |

A placeholder that resolves to nothing is a hard error raised **before** the
process starts — a silently empty prompt fragment is worse than a failed run,
because the agent still "succeeds".

Referencing `{{outputs.a}}` without `dependsOn: ["a"]` will fail for the same
reason: the output does not exist yet. The test suite enforces this for every
shipped example.

## Artifacts

Each run writes to `.agentic/runs/<timestamp>-<name>/`:

```
scan_src.jsonl     raw Claude Code NDJSON transcript, one line per event
scan_src.md        that agent's final answer
summary.json       full machine-readable result: statuses, timings, tools, board
summary.md         table of agents plus every answer
```

## Library use

```ts
import { buildPreset, Fleet, resolveFleet } from "./src/index.ts";

const plan = buildPreset("fanout", "find the slowest test in the suite", { agents: 3 });
const fleet = new Fleet(resolveFleet(plan, { concurrency: 2, model: "qwen2.5-coder" }));

fleet.bus.on("agent:tool", ({ id, tool }) => console.log(id, tool.name));
fleet.bus.on("agent:end", ({ result }) => console.log(result.id, result.status));

const run = await fleet.run();
console.log(run.board.synthesis);
```

Events: `fleet:start`, `fleet:end`, `agent:queued`, `agent:start`, `agent:text`,
`agent:tool`, `agent:tool-result`, `agent:retry`, `agent:end`, `agent:skipped`.

## How a run behaves

- **Ordering** — agents run as soon as their dependencies have *succeeded*, up
  to `concurrency`. Duplicate ids, unknown dependencies and cycles are rejected
  before anything is spawned.
- **Failure** — a failed agent skips its transitive dependents; unrelated
  branches keep running. `failFast` cancels everything instead.
- **Retries** — `retries` extra attempts with exponential backoff. A non-zero
  exit, a Claude Code error result, or a spawn failure all count.
- **Timeouts** — `timeoutMs` sends `SIGTERM`, escalates to `SIGKILL`, and stops
  waiting on the pipes shortly after the child exits (Claude Code's own
  grandchildren can otherwise hold stdout open forever).
- **Ctrl-C** — kills live children, marks the rest cancelled, still writes
  artifacts.

## Launch modes

| mode | command | when |
| --- | --- | --- |
| `launch` | `ollama launch claude --model M -- --print --output-format stream-json …` | Ollama v0.15+ (preferred) |
| `direct` | `claude --print …` with `ANTHROPIC_BASE_URL=<ollamaHost>`, `ANTHROPIC_MODEL=M` | no `launch` subcommand |
| `auto` | probes `ollama launch --help`, picks one, warns if it fell back | default |

In `launch` mode any inherited `ANTHROPIC_BASE_URL` / `ANTHROPIC_AUTH_TOKEN` is
removed from the child environment, so a stale value in your shell cannot
silently redirect agents at a different endpoint.

`direct` mode sets `ANTHROPIC_BASE_URL` to the Ollama server root, because the
Anthropic client appends `/v1/messages` itself. If your Ollama build exposes the
Anthropic surface elsewhere, set `ANTHROPIC_BASE_URL` in the plan's `env` block —
plan and agent `env` are applied after these defaults and win.

Prompts go over **stdin** by default, which avoids argv length limits and
quoting problems entirely.

## Layout

```
src/
  types.ts         every shared type
  plan.ts          load / validate / resolve plans, presets
  launcher.ts      agent spec + mode → argv and environment
  ollama.ts        reachability, model list, warm-up, launch-support probe
  scheduler.ts     generic DAG execution with a concurrency limit
  agent.ts         one child process: spawn, stream, timeout, retry
  stream.ts        Claude Code NDJSON → normalised events
  orchestrator.ts  the Fleet — read this one first
  blackboard.ts    how one agent's answer reaches the next agent's prompt
  template.ts      {{placeholder}} rendering
  events.ts        typed event bus
  logger.ts        leveled, per-agent coloured output
  cli.ts           argument parsing and commands
  wizard.ts        the interactive plan builder
```

## Tests

```sh
bun test          # 153 tests
bun run check     # tsc --noEmit && bun test
```

`test/e2e.test.ts` runs the real `Bun.spawn` path against a shell script that
speaks Claude Code's NDJSON protocol, so spawning, stdin delivery, incremental
parsing, exit codes, timeouts and artifact writing are all covered without
Ollama installed. Everything else uses an injected process runner.

## A real run

Below is an actual terminal transcript. `claude` is stood in for by a script
that speaks the same NDJSON protocol (this machine has no Ollama), so the
agent *answers* are canned — the fleet mechanics, parallelism, output passing
and artifacts are all real.

```console
$ agentic run plan.json
▸ fleet "todo-sweep" — 3 agents, concurrency 2
mode: launch · model: qwen2.5-coder:latest · cwd: /home/you/project
artifacts: /home/you/project/.agentic/runs/2026-08-25_03-37-25-todo-sweep
[scan_src] start (attempt 1/2, launch, qwen2.5-coder:latest)
[scan_tests] start (attempt 1/2, launch, qwen2.5-coder:latest)
[scan_src] session 7f3a91c2 · 4 tools
[scan_src] Grep({"pattern":"TODO","path":"src"})
[scan_src] → src/stream.ts:41: // TODO: handle partial frames
[scan_src] 1 open TODO in the stream parser (partial frames).
[scan_src] done in 0.0s · 1 tool call · 3 turns
[scan_tests] session 7f3a91c2 · 4 tools
[scan_tests] Grep({"pattern":"TODO","path":"test"})
[scan_tests] → test/e2e.test.ts:12: // TODO: cover SIGINT
[scan_tests] 1 open TODO in the e2e tests (SIGINT coverage).
[scan_tests] done in 0.0s · 1 tool call · 3 turns
[triage] start (attempt 1/2, launch, qwen2.5-coder:latest)
[triage] session 7f3a91c2 · 4 tools
[triage] Grep({"pattern":"TODO","path":"all"})
[triage] → none
[triage] Both TODOs are real; the stream one matters, the test one is a nice-to-have.
[triage] done in 0.0s · 1 tool call · 3 turns
✔ 3/3 agents succeeded in 0.0s (peak parallelism 2)
```

`scan_src` and `scan_tests` are independent, so both processes start
immediately; `triage` waits for both and receives their answers interpolated
into its prompt. The written `summary.md`:

```markdown
# todo-sweep

- run: `2026-08-25_03-37-25-todo-sweep`
- duration: 0.0s
- result: ok

| agent | role | status | turns | tools | duration |
| --- | --- | --- | --- | --- | --- |
| scan_src | scan src | succeeded | 3 | 1 | 0.0s |
| scan_tests | scan tests | succeeded | 3 | 1 | 0.0s |
| triage | triage | succeeded | 3 | 1 | 0.0s |

## scan_src

1 open TODO in the stream parser (partial frames).

## scan_tests

1 open TODO in the e2e tests (SIGINT coverage).

## triage

Both TODOs are real; the stream one matters, the test one is a nice-to-have.
```

And the same fleet, resolved but not run:

```console
$ agentic run plan.json --dry-run
▸ layer 1 — 2 agents in parallel
[scan_src] ollama launch claude --model qwen2.5-coder:latest -- --print --output-format stream-json --verbose --permission-mode acceptEdits --max-turns 24
[scan_src] stdin 37 chars
[scan_tests] ollama launch claude --model qwen2.5-coder:latest -- --print --output-format stream-json --verbose --permission-mode acceptEdits --max-turns 24
[scan_tests] stdin 38 chars
▸ layer 2 — 1 agent
[triage] ollama launch claude --model qwen2.5-coder:latest -- --print --output-format stream-json --verbose --permission-mode acceptEdits --max-turns 24
[triage] stdin 88 chars
```

## Note on the surrounding repository

This lives in a checkout of the OCaml [opam](https://github.com/ocaml/opam)
source tree, which is unrelated to it. `agentic-bun/dune` tells dune not to
descend into this directory, so `node_modules/` and `.agentic/` never enter
opam's build graph and opam's own build is untouched.

## Non-goals

No custom chat/tool loop, no daemon, no server, no database, no non-Ollama
backends, no TUI. See [SPEC.md](SPEC.md).
