/**
 * One agent = one `claude` child process, driven to completion.
 *
 * This module owns spawning, streaming, timeouts and retries. The actual
 * process creation is injected ({@link ExecFn}) so the whole lifecycle is
 * testable without Ollama, Claude Code, or a network.
 */

import type { EventBus } from "./events.ts";
import { Logger, oneLine } from "./logger.ts";
import { buildLaunchPlan, formatCommand } from "./launcher.ts";
import { NdjsonParser, interpret } from "./stream.ts";
import type {
  AgentResult,
  AgentStatus,
  LaunchPlan,
  ResolvedAgent,
  ResolvedFleet,
  ToolCallRecord,
} from "./types.ts";

export interface ProcessHandlers {
  onStdout: (chunk: string) => void;
  onStderr: (chunk: string) => void;
}

export interface ProcessOutcome {
  exitCode: number | null;
  timedOut: boolean;
  /** Spawn-level failure (binary missing, cwd gone, ...). */
  error?: string;
}

export type ExecFn = (
  plan: LaunchPlan,
  opts: { timeoutMs: number; signal?: AbortSignal; handlers: ProcessHandlers },
) => Promise<ProcessOutcome>;

/**
 * How long to keep draining stdout/stderr after the child has exited.
 *
 * Claude Code spawns its own children (the Bash tool). If we kill Claude Code,
 * a grandchild can still hold the write end of the pipe open, so the read side
 * never reaches EOF. Waiting on the child's exit and then draining for a bounded
 * time keeps a timeout from turning into a hang.
 */
const DRAIN_MS = 1500;

/** Real process execution via Bun.spawn. */
export const bunExec: ExecFn = async (plan, { timeoutMs, signal, handlers }) => {
  let proc: ReturnType<typeof Bun.spawn>;
  try {
    proc = Bun.spawn(plan.cmd, {
      cwd: plan.cwd,
      env: plan.env,
      stdin: plan.stdin === null ? "ignore" : new TextEncoder().encode(plan.stdin),
      stdout: "pipe",
      stderr: "pipe",
    });
  } catch (error) {
    return {
      exitCode: null,
      timedOut: false,
      error: error instanceof Error ? error.message : String(error),
    };
  }

  let timedOut = false;
  const kill = (hard: boolean) => {
    try {
      proc.kill(hard ? "SIGKILL" : "SIGTERM");
    } catch {
      // Already gone.
    }
  };

  const timer =
    timeoutMs > 0
      ? setTimeout(() => {
          timedOut = true;
          kill(false);
          // Escalate if it ignores SIGTERM.
          setTimeout(() => kill(true), 3000).unref?.();
        }, timeoutMs)
      : undefined;

  const onAbort = () => kill(false);
  signal?.addEventListener("abort", onAbort, { once: true });

  /** Read a stream to EOF, with a handle to abandon it if it stops ending. */
  const pump = (
    stream: ReadableStream<Uint8Array> | undefined,
    sink: (text: string) => void,
  ): { done: Promise<void>; stop: () => void } => {
    if (!stream) return { done: Promise.resolve(), stop: () => {} };
    const reader = stream.getReader();
    const done = (async () => {
      const decoder = new TextDecoder();
      while (true) {
        const { done: finished, value } = await reader.read();
        if (finished) break;
        if (value) sink(decoder.decode(value, { stream: true }));
      }
      const tail = decoder.decode();
      if (tail) sink(tail);
    })().catch(() => {
      // Cancelled below, or the pipe broke: the exit code is the source of truth.
    });
    return { done, stop: () => void reader.cancel().catch(() => {}) };
  };

  const out = pump(proc.stdout as ReadableStream<Uint8Array> | undefined, handlers.onStdout);
  const err = pump(proc.stderr as ReadableStream<Uint8Array> | undefined, handlers.onStderr);

  try {
    const exitCode = await proc.exited;
    // Give the readers a bounded chance to drain, then abandon them.
    await Promise.race([Promise.all([out.done, err.done]), Bun.sleep(DRAIN_MS)]);
    out.stop();
    err.stop();
    return { exitCode, timedOut };
  } catch (error) {
    kill(true);
    out.stop();
    err.stop();
    return {
      exitCode: null,
      timedOut,
      error: error instanceof Error ? error.message : String(error),
    };
  } finally {
    if (timer) clearTimeout(timer);
    signal?.removeEventListener("abort", onAbort);
  }
};

export interface AgentContext {
  fleet: ResolvedFleet;
  mode: "launch" | "direct";
  bus: EventBus;
  log: Logger;
  exec?: ExecFn;
  signal?: AbortSignal;
  /** Base delay for retry backoff. Default 1000ms. */
  backoffMs?: number;
  /** Receives every raw NDJSON line, for transcript files. */
  sink?: (agentId: string, line: string) => void;
  /** Print assistant text as it streams. Default true. */
  streamText?: boolean;
}

/** Accumulated state of a single process attempt. */
interface AttemptState {
  text: string;
  streamedText: string[];
  toolCalls: ToolCallRecord[];
  events: number;
  numTurns: number;
  sessionId: string;
  costUsd: number;
  resultSeen: boolean;
  resultIsError: boolean;
  stderr: string;
}

function newAttempt(): AttemptState {
  return {
    text: "",
    streamedText: [],
    toolCalls: [],
    events: 0,
    numTurns: 0,
    sessionId: "",
    costUsd: 0,
    resultSeen: false,
    resultIsError: false,
    stderr: "",
  };
}

/**
 * Run one agent to completion, retrying on failure.
 * Never throws: a failure is reported as a result with a non-success status.
 */
export async function runAgent(agent: ResolvedAgent, ctx: AgentContext): Promise<AgentResult> {
  const exec = ctx.exec ?? bunExec;
  const backoff = ctx.backoffMs ?? 1000;
  const startedAt = Date.now();
  const maxAttempts = Math.max(1, agent.retries + 1);

  let attempt = 0;
  let last: { state: AttemptState; status: AgentStatus; exitCode: number | null; error?: string } = {
    state: newAttempt(),
    status: "failed",
    exitCode: null,
    error: "not started",
  };

  while (attempt < maxAttempts) {
    if (ctx.signal?.aborted) {
      last = { state: last.state, status: "cancelled", exitCode: null, error: "cancelled" };
      break;
    }
    attempt += 1;

    const plan = buildLaunchPlan(agent, {
      mode: ctx.mode,
      fleet: ctx.fleet,
    });

    ctx.bus.emit("agent:start", {
      id: agent.id,
      role: agent.role,
      attempt,
      cmd: plan.cmd,
    });
    ctx.log.agent(
      agent.id,
      `start (attempt ${attempt}/${maxAttempts}, ${plan.mode}, ${agent.model || "model unset"})`,
      "meta",
    );
    ctx.log.debug(`${agent.id}: ${formatCommand(plan.cmd)}`);

    const state = newAttempt();
    const parser = new NdjsonParser((value, line) => {
      state.events += 1;
      ctx.sink?.(agent.id, line);
      for (const event of interpret(value)) {
        switch (event.kind) {
          case "init":
            state.sessionId ||= event.sessionId;
            ctx.log.agent(
              agent.id,
              `session ${event.sessionId.slice(0, 8) || "?"} · ${event.tools.length} tools`,
              "meta",
            );
            break;
          case "text":
            state.streamedText.push(event.text);
            if (ctx.streamText !== false) ctx.log.agent(agent.id, oneLine(event.text, 300));
            ctx.bus.emit("agent:text", { id: agent.id, text: event.text });
            break;
          case "thinking":
            ctx.log.debug(`${agent.id} thinking: ${oneLine(event.text, 120)}`);
            break;
          case "tool-use": {
            const record: ToolCallRecord = {
              name: event.name,
              summary: event.summary,
              at: Date.now(),
            };
            state.toolCalls.push(record);
            ctx.log.agent(agent.id, `${event.name}(${event.summary})`, "tool");
            ctx.bus.emit("agent:tool", { id: agent.id, tool: record });
            break;
          }
          case "tool-result":
            ctx.log.agent(
              agent.id,
              `${event.isError ? "✖" : "→"} ${oneLine(event.summary, 140)}`,
              "meta",
            );
            ctx.bus.emit("agent:tool-result", {
              id: agent.id,
              summary: event.summary,
              isError: event.isError,
            });
            break;
          case "result":
            state.resultSeen = true;
            state.resultIsError = event.isError;
            state.text = event.text;
            state.numTurns = event.numTurns;
            state.costUsd = event.costUsd;
            state.sessionId ||= event.sessionId;
            break;
          case "unknown":
            ctx.log.debug(`${agent.id} unhandled: ${oneLine(event.raw, 160)}`);
            break;
        }
      }
    });

    const outcome = await exec(plan, {
      timeoutMs: agent.timeoutMs,
      signal: ctx.signal,
      handlers: {
        onStdout: (chunk) => parser.push(chunk),
        onStderr: (chunk) => {
          // Keep the tail only; Claude Code can be chatty on stderr.
          state.stderr = (state.stderr + chunk).slice(-8000);
        },
      },
    });
    parser.flush();

    if (!state.text && state.streamedText.length > 0) {
      // No `result` event (killed, or an older output format): fall back to the
      // assistant text we did see.
      state.text = state.streamedText.join("\n").trim();
    }

    const status: AgentStatus = outcome.timedOut
      ? "timeout"
      : ctx.signal?.aborted
        ? "cancelled"
        : outcome.error
          ? "failed"
          : outcome.exitCode === 0 && (!state.resultSeen || !state.resultIsError)
            ? "succeeded"
            : "failed";

    last = { state, status, exitCode: outcome.exitCode, error: outcome.error };

    if (status === "succeeded" || status === "cancelled") break;

    if (attempt < maxAttempts) {
      const delayMs = backoff * 2 ** (attempt - 1);
      const reason = outcome.error ?? (outcome.timedOut ? "timeout" : `exit ${outcome.exitCode}`);
      ctx.bus.emit("agent:retry", { id: agent.id, attempt, delayMs, reason });
      ctx.log.agent(agent.id, `retry in ${delayMs}ms after ${reason}`, "meta");
      if (delayMs > 0) await Bun.sleep(delayMs);
    }
  }

  const endedAt = Date.now();
  const result: AgentResult = {
    id: agent.id,
    role: agent.role,
    status: last.status,
    attempts: attempt,
    text: last.state.text,
    toolCalls: last.state.toolCalls,
    startedAt,
    endedAt,
    durationMs: endedAt - startedAt,
    exitCode: last.exitCode,
    numTurns: last.state.numTurns,
    sessionId: last.state.sessionId,
    costUsd: last.state.costUsd,
    events: last.state.events,
    stderr: last.state.stderr,
  };
  if (last.error) result.error = last.error;
  else if (last.status !== "succeeded") {
    result.error =
      last.status === "timeout"
        ? `timed out after ${agent.timeoutMs}ms`
        : last.state.resultIsError
          ? last.state.text || "claude reported an error result"
          : `claude exited with code ${last.exitCode}`;
  }

  ctx.bus.emit("agent:end", { id: agent.id, result });
  return result;
}
