# SPEC — Agentic Bun Framework

Status: implemented (v0.1.0)
Target runtime: Bun >= 1.1
Language: TypeScript, zero runtime dependencies

## 1. Goal

Run **many Claude Code agents at once, against a local model**, from a single Bun
process. Each agent is a real `claude` CLI process spawned through
`ollama launch claude`, which points Claude Code at a local Ollama model instead
of the Anthropic API. The framework owns everything *around* those processes:
fleet definition, dependency ordering, concurrency, prompt templating, result
passing, streaming observability, retries, timeouts and artifacts.

The framework deliberately does **not** implement its own chat/tool-calling loop.
Claude Code already has one, and `ollama launch claude` is exactly the seam that
attaches it to a local model. Re-implementing the loop would mean competing with
the harness we are spawning.

## 2. Non-goals

- No custom chat/tool loop, no tool schema, no direct `/api/chat` traffic for
  agent turns. (Ollama's HTTP API is used only for preflight: health, model
  list, model warm-up.)
- No daemon, no server, no database.
- No non-Ollama backends.
- No TUI. Plain, streaming, line-oriented terminal output.

## 3. Functional requirements

### FR-1 Launch
- **FR-1.1** Spawn agents via `ollama launch claude [--model M] -- <claude args>`.
- **FR-1.2** Fall back to spawning `claude` directly with the Ollama-compatible
  Anthropic environment (`ANTHROPIC_BASE_URL=<ollamaHost>`, auth token, model
  overrides) when `ollama launch` is unavailable. Mode selectable:
  `auto` (default) | `launch` | `direct`.
- **FR-1.3** The launch command line is overridable (`launchCommand`), so a
  future change in Ollama's flag surface does not require a code change.
- **FR-1.4** Prompts are delivered on **stdin** by default (no argv length or
  quoting limits); `promptVia: "arg"` is available.

### FR-2 Fleet
- **FR-2.1** A fleet is a list of agents forming a DAG via `dependsOn`.
- **FR-2.2** Ready agents run in parallel up to `concurrency`.
- **FR-2.3** Duplicate ids, unknown dependencies and cycles are rejected before
  anything is spawned.
- **FR-2.4** A failed agent skips its transitive dependents; independent
  branches keep running. `failFast` cancels the whole run instead.

### FR-3 Data flow
- **FR-3.1** Prompts are templates: `{{vars.x}}`, `{{outputs.<agentId>}}`,
  `{{board.<key>}}`, `{{env.X}}`, `{{agent.id}}`.
- **FR-3.2** Every agent's final text is published to a shared blackboard under
  its id and, optionally, under `outputKey`.
- **FR-3.3** Unresolved placeholders are a hard error, raised before spawning.

### FR-4 Process control
- **FR-4.1** Per-agent `timeoutMs`, enforced by killing the process tree.
- **FR-4.2** Per-agent `retries` with exponential backoff.
- **FR-4.3** SIGINT stops the fleet, kills live children, still writes artifacts.

### FR-5 Observability
- **FR-5.1** Parse Claude Code's `--output-format stream-json` NDJSON and print
  legible per-agent lines: assistant text, tool calls, tool results, result.
- **FR-5.2** Malformed / unknown events never crash a run.
- **FR-5.3** Raw NDJSON transcript, per-agent markdown, `summary.json` and
  `summary.md` written under `.agentic/runs/<runId>/`.

### FR-6 CLI
`agentic run|wizard|doctor|models|init|help`, plus `--dry-run` which prints the
exact resolved argv+env per agent and spawns nothing.

## 4. Acceptance criteria

- `bun test` passes; the suite covers templating, DAG scheduling (order,
  concurrency, cycles, failure cascade), NDJSON parsing, launch-argv building,
  plan validation, agent retry/timeout, and a full fake-process fleet run.
- `bun run typecheck` (`tsc --noEmit`) is clean.
- `agentic doctor` reports bun/claude/ollama/model status without throwing when
  Ollama is absent.
- `agentic run examples/review-fleet.json --dry-run` prints one resolved command
  per agent, in dependency order.
- A developer can read `src/orchestrator.ts` top to bottom and follow the whole
  run.
- Zero runtime dependencies in `package.json`.

## 5. Task list

- [x] T1 types, config resolution, logger
- [x] T2 NDJSON stream parser + Claude event normalizer
- [x] T3 launcher (launch/direct argv + env)
- [x] T4 DAG scheduler with concurrency + failure cascade
- [x] T5 blackboard + prompt templating
- [x] T6 agent process runner (retry, timeout, streaming, artifacts)
- [x] T7 orchestrator / fleet
- [x] T8 plan loading, validation, presets
- [x] T9 CLI + wizard + doctor
- [x] T10 tests, examples, README
