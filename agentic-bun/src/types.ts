/**
 * Core types for the Agentic Bun Framework.
 *
 * A "fleet" is a DAG of agents. Every agent is one `claude` process, spawned
 * through `ollama launch claude` so that Claude Code's own tool loop runs
 * against a local Ollama model.
 */

/** How the underlying `claude` process is started. */
export type LaunchMode =
  /** Use `ollama launch claude` if it exists, otherwise fall back to `direct`. */
  | "auto"
  /** Always `ollama launch claude`. Fails loudly if unavailable. */
  | "launch"
  /** Spawn `claude` directly with Ollama's Anthropic-compatible env vars. */
  | "direct";

/** Claude Code permission modes we pass through verbatim. */
export type PermissionMode =
  | "default"
  | "acceptEdits"
  | "bypassPermissions"
  | "plan";

/** How the prompt reaches the child process. */
export type PromptDelivery = "stdin" | "arg";

export type AgentStatus =
  | "pending"
  | "running"
  | "succeeded"
  | "failed"
  | "timeout"
  | "skipped"
  | "cancelled";

/** One agent in a fleet. Anything omitted falls back to the fleet defaults. */
export interface AgentSpec {
  /** Unique within the fleet. Used for dependencies, templating and artifacts. */
  id: string;
  /** Free-text label shown in logs, e.g. "reviewer". */
  role?: string;
  /** Prompt template. See {@link render} for the supported placeholders. */
  prompt: string;
  /** Ollama model for this agent. Defaults to the fleet model. */
  model?: string;
  /** Extra system prompt appended to Claude Code's own (`--append-system-prompt`). */
  systemPrompt?: string;
  /** Working directory for the child process. Defaults to the fleet cwd. */
  cwd?: string;
  /** Extra directories the agent may touch (`--add-dir`). */
  addDirs?: string[];
  allowedTools?: string[];
  disallowedTools?: string[];
  permissionMode?: PermissionMode;
  /** Hard cap on Claude Code's agent turns (`--max-turns`). */
  maxTurns?: number;
  timeoutMs?: number;
  /** Extra attempts after the first failure. */
  retries?: number;
  /** Ids of agents that must succeed before this one starts. */
  dependsOn?: string[];
  /** Additional blackboard key to publish this agent's final text under. */
  outputKey?: string;
  /** Extra environment variables for the child process. */
  env?: Record<string, string>;
}

/** A complete fleet definition — this is what a plan file contains. */
export interface FleetPlan {
  /** Schema version. Only `1` is understood. */
  version?: number;
  name?: string;
  description?: string;
  /** Default model for every agent. */
  model?: string;
  /** Ollama host, e.g. `http://127.0.0.1:11434`. */
  ollamaHost?: string;
  launchMode?: LaunchMode;
  /**
   * Explicit launch command template. `{{model}}` is substituted; the resolved
   * claude arguments are appended after a `--` separator.
   * Default: `["ollama", "launch", "claude", "--model", "{{model}}"]`.
   */
  launchCommand?: string[];
  promptVia?: PromptDelivery;
  cwd?: string;
  /** Max agents running at the same time. Default 3. */
  concurrency?: number;
  /** Abort the whole fleet on the first agent failure. Default false. */
  failFast?: boolean;
  /** Skip the Ollama reachability/model check. Default false. */
  skipPreflight?: boolean;
  /** Skip loading the model into memory before the run. Default false. */
  skipWarm?: boolean;
  /** `keep_alive` used when warming the model. Default "30m". */
  keepAlive?: string;
  /** Where run artifacts go. Default `<cwd>/.agentic/runs`. */
  artifactsDir?: string;
  /** Disable artifact writing entirely. */
  noArtifacts?: boolean;
  /** Values available to prompts as `{{vars.*}}`. */
  vars?: Record<string, string>;
  /** Fleet-wide agent defaults. */
  permissionMode?: PermissionMode;
  maxTurns?: number;
  timeoutMs?: number;
  retries?: number;
  allowedTools?: string[];
  disallowedTools?: string[];
  systemPrompt?: string;
  env?: Record<string, string>;
  agents: AgentSpec[];
}

/** A plan after defaults have been folded in — every field is present. */
export interface ResolvedFleet {
  version: number;
  name: string;
  description: string;
  model: string;
  ollamaHost: string;
  launchMode: LaunchMode;
  launchCommand: string[];
  promptVia: PromptDelivery;
  cwd: string;
  concurrency: number;
  failFast: boolean;
  skipPreflight: boolean;
  skipWarm: boolean;
  keepAlive: string;
  artifactsDir: string;
  noArtifacts: boolean;
  vars: Record<string, string>;
  env: Record<string, string>;
  agents: ResolvedAgent[];
}

/** An agent after defaults have been folded in. */
export interface ResolvedAgent {
  id: string;
  role: string;
  prompt: string;
  model: string;
  systemPrompt: string;
  cwd: string;
  addDirs: string[];
  allowedTools: string[];
  disallowedTools: string[];
  permissionMode: PermissionMode;
  maxTurns: number;
  timeoutMs: number;
  retries: number;
  dependsOn: string[];
  outputKey: string;
  env: Record<string, string>;
}

/** A tool invocation observed in an agent's stream. */
export interface ToolCallRecord {
  name: string;
  /** Truncated single-line rendering of the tool input. */
  summary: string;
  at: number;
}

export interface AgentResult {
  id: string;
  role: string;
  status: AgentStatus;
  /** Number of process spawns, including retries. */
  attempts: number;
  /** Final assistant text (Claude Code's `result` payload). */
  text: string;
  toolCalls: ToolCallRecord[];
  startedAt: number;
  endedAt: number;
  durationMs: number;
  exitCode: number | null;
  numTurns: number;
  sessionId: string;
  costUsd: number;
  /** Count of stream events observed. */
  events: number;
  stderr: string;
  error?: string;
}

export interface FleetRunResult {
  runId: string;
  name: string;
  startedAt: number;
  endedAt: number;
  durationMs: number;
  ok: boolean;
  artifactsDir: string;
  results: AgentResult[];
  /** Final blackboard contents. */
  board: Record<string, string>;
}

/** The command + environment used to spawn one agent. */
export interface LaunchPlan {
  mode: "launch" | "direct";
  cmd: string[];
  env: Record<string, string>;
  cwd: string;
  /** Prompt text piped to stdin, or `null` when passed on argv. */
  stdin: string | null;
}
