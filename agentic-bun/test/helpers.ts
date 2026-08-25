/** Shared fixtures: fake Claude Code processes, so tests never spawn anything. */

import type { ExecFn } from "../src/agent.ts";
import type { LaunchPlan } from "../src/types.ts";

export interface FakeRun {
  /** NDJSON lines the fake process writes to stdout. */
  lines?: string[];
  stderr?: string;
  exitCode?: number | null;
  timedOut?: boolean;
  error?: string;
  /** Emit stdout in pieces to exercise the incremental parser. */
  chunkSize?: number;
}

/** Build the NDJSON a successful Claude Code `--print --output-format stream-json` run emits. */
export function claudeStream(opts: {
  text: string;
  tools?: { name: string; input: unknown }[];
  sessionId?: string;
  numTurns?: number;
  isError?: boolean;
}): string[] {
  const sessionId = opts.sessionId ?? "sess-1234abcd";
  const lines: string[] = [
    JSON.stringify({
      type: "system",
      subtype: "init",
      session_id: sessionId,
      model: "qwen2.5-coder:latest",
      tools: ["Read", "Write", "Bash"],
    }),
  ];
  for (const tool of opts.tools ?? []) {
    lines.push(
      JSON.stringify({
        type: "assistant",
        session_id: sessionId,
        message: {
          role: "assistant",
          content: [{ type: "tool_use", id: "tu_1", name: tool.name, input: tool.input }],
        },
      }),
    );
    lines.push(
      JSON.stringify({
        type: "user",
        session_id: sessionId,
        message: {
          role: "user",
          content: [{ type: "tool_result", tool_use_id: "tu_1", content: "tool ok" }],
        },
      }),
    );
  }
  lines.push(
    JSON.stringify({
      type: "assistant",
      session_id: sessionId,
      message: { role: "assistant", content: [{ type: "text", text: opts.text }] },
    }),
  );
  lines.push(
    JSON.stringify({
      type: "result",
      subtype: opts.isError ? "error_during_execution" : "success",
      is_error: opts.isError ?? false,
      duration_ms: 1234,
      num_turns: opts.numTurns ?? 2,
      result: opts.text,
      session_id: sessionId,
      total_cost_usd: 0,
    }),
  );
  return lines;
}

/**
 * An {@link ExecFn} driven by a script. `plan` is the resolved launch plan, so a
 * test can assert on the argv/stdin it was handed.
 */
export function fakeExec(
  script: (plan: LaunchPlan, attempt: number) => FakeRun,
  seen?: LaunchPlan[],
): ExecFn {
  const attempts = new Map<string, number>();
  return async (plan, { handlers }) => {
    seen?.push(plan);
    const key = plan.env.AGENTIC_AGENT_ID ?? "";
    const attempt = (attempts.get(key) ?? 0) + 1;
    attempts.set(key, attempt);

    const run = script(plan, attempt);
    const payload = (run.lines ?? []).join("\n") + (run.lines?.length ? "\n" : "");
    const size = run.chunkSize ?? payload.length;
    for (let i = 0; i < payload.length; i += Math.max(1, size)) {
      handlers.onStdout(payload.slice(i, i + Math.max(1, size)));
    }
    if (run.stderr) handlers.onStderr(run.stderr);

    const outcome: Awaited<ReturnType<ExecFn>> = {
      exitCode: run.exitCode === undefined ? 0 : run.exitCode,
      timedOut: run.timedOut ?? false,
    };
    if (run.error) outcome.error = run.error;
    return outcome;
  };
}

/** Probes that report a healthy local Ollama without touching the network. */
export const happyProbes = {
  launchSupport: async () => true,
  ollama: async (host: string) => ({
    reachable: true,
    host,
    version: "0.15.0",
    models: ["qwen2.5-coder:latest", "llama3.1:8b"],
  }),
  warm: async () => true,
};
