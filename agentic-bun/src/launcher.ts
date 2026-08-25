/**
 * Turning an agent spec into an actual command line.
 *
 * Two shapes, same claude arguments:
 *
 *   launch:  ollama launch claude --model <m> -- --print --output-format ...
 *   direct:  claude --print --output-format ...   (+ ANTHROPIC_* env pointing
 *                                                  at Ollama's OpenAI-compatible
 *                                                  endpoint)
 *
 * `ollama launch claude` (Ollama v0.15+) is the intended path: it configures
 * Claude Code against a local model and runs it. `direct` exists so the fleet
 * still runs on an Ollama build without the subcommand, and so the whole thing
 * is testable without Ollama installed.
 */

import type { LaunchMode, LaunchPlan, ResolvedAgent, ResolvedFleet } from "./types.ts";

/** Default `ollama launch` invocation. `{{model}}` is substituted at build time. */
export const DEFAULT_LAUNCH_COMMAND = ["ollama", "launch", "claude", "--model", "{{model}}"];

/**
 * The claude arguments shared by both modes. Non-interactive, streaming NDJSON
 * (`--verbose` is required by Claude Code for stream-json under `--print`).
 */
export function buildClaudeArgs(agent: ResolvedAgent, promptOnArgv: string | null): string[] {
  const args: string[] = ["--print", "--output-format", "stream-json", "--verbose"];

  if (agent.permissionMode) args.push("--permission-mode", agent.permissionMode);
  if (agent.maxTurns > 0) args.push("--max-turns", String(agent.maxTurns));
  if (agent.allowedTools.length > 0) args.push("--allowed-tools", agent.allowedTools.join(","));
  if (agent.disallowedTools.length > 0)
    args.push("--disallowed-tools", agent.disallowedTools.join(","));
  for (const dir of agent.addDirs) args.push("--add-dir", dir);
  if (agent.systemPrompt.trim().length > 0)
    args.push("--append-system-prompt", agent.systemPrompt);
  if (promptOnArgv !== null) args.push(promptOnArgv);

  return args;
}

/**
 * Environment that points Claude Code at Ollama's Anthropic-compatible API.
 * Used in `direct` mode; `ollama launch claude` sets the equivalent itself.
 *
 * `host` is the server root, not an endpoint: the Anthropic client appends
 * `/v1/messages` to `ANTHROPIC_BASE_URL` itself. If your Ollama build exposes
 * the Anthropic surface somewhere else, override `ANTHROPIC_BASE_URL` through
 * the plan's `env` block — it is applied after these defaults.
 */
export function directEnv(host: string, model: string): Record<string, string> {
  const base = host.replace(/\/+$/, "");
  return {
    ANTHROPIC_BASE_URL: base,
    ANTHROPIC_AUTH_TOKEN: "ollama",
    ANTHROPIC_API_KEY: "ollama",
    ANTHROPIC_MODEL: model,
    ANTHROPIC_SMALL_FAST_MODEL: model,
    ANTHROPIC_DEFAULT_HAIKU_MODEL: model,
    ANTHROPIC_DEFAULT_SONNET_MODEL: model,
    ANTHROPIC_DEFAULT_OPUS_MODEL: model,
    // Local-only run: no point paying for telemetry round trips.
    CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC: "1",
  };
}

/** Substitute `{{model}}` in a launch command template. */
export function renderLaunchCommand(template: string[], model: string): string[] {
  return template.map((part) => part.replaceAll("{{model}}", model));
}

/**
 * Resolve `auto` against what is actually installed.
 * `launchAvailable` comes from {@link probeLaunchSupport}.
 */
export function resolveMode(mode: LaunchMode, launchAvailable: boolean): "launch" | "direct" {
  if (mode === "direct") return "direct";
  if (mode === "launch") return "launch";
  return launchAvailable ? "launch" : "direct";
}

export interface BuildOptions {
  /** Result of resolving `auto`. */
  mode: "launch" | "direct";
  /** Fleet-level settings the agent inherits. */
  fleet: Pick<ResolvedFleet, "launchCommand" | "ollamaHost" | "promptVia" | "env">;
  /** Parent environment. Defaults to `process.env`. */
  parentEnv?: Record<string, string | undefined>;
}

/** Build the full spawn plan for one agent. */
export function buildLaunchPlan(agent: ResolvedAgent, opts: BuildOptions): LaunchPlan {
  const viaStdin = opts.fleet.promptVia !== "arg";
  const claudeArgs = buildClaudeArgs(agent, viaStdin ? null : agent.prompt);

  const cmd =
    opts.mode === "launch"
      ? [...renderLaunchCommand(opts.fleet.launchCommand, agent.model), "--", ...claudeArgs]
      : ["claude", ...claudeArgs];

  const parent = opts.parentEnv ?? (process.env as Record<string, string | undefined>);
  const env: Record<string, string> = {};
  for (const [key, value] of Object.entries(parent)) {
    if (typeof value === "string") env[key] = value;
  }
  if (opts.mode === "direct") {
    Object.assign(env, directEnv(opts.fleet.ollamaHost, agent.model));
  } else {
    // `ollama launch` owns these; a stale value inherited from the parent shell
    // would silently redirect the agent at the wrong endpoint.
    delete env.ANTHROPIC_BASE_URL;
    delete env.ANTHROPIC_AUTH_TOKEN;
  }
  Object.assign(env, opts.fleet.env, agent.env);
  env.AGENTIC_AGENT_ID = agent.id;
  env.AGENTIC_AGENT_ROLE = agent.role;

  return {
    mode: opts.mode,
    cmd,
    env,
    cwd: agent.cwd,
    stdin: viaStdin ? agent.prompt : null,
  };
}

/** Shell-ish rendering of a command, for `--dry-run` and logs. */
export function formatCommand(cmd: string[]): string {
  return cmd
    .map((part) => (/[\s"'$`\\|&;<>()*?]/.test(part) ? JSON.stringify(part) : part))
    .join(" ");
}
