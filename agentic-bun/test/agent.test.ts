import { describe, expect, test } from "bun:test";
import { runAgent } from "../src/agent.ts";
import { EventBus } from "../src/events.ts";
import { Logger } from "../src/logger.ts";
import { resolveFleet } from "../src/plan.ts";
import type { LaunchPlan, ResolvedAgent, ResolvedFleet } from "../src/types.ts";
import { claudeStream, fakeExec, type FakeRun } from "./helpers.ts";

const silent = new Logger("silent", { color: false, write: () => {} });

function setup(agentOverrides: Partial<ResolvedAgent> = {}): {
  agent: ResolvedAgent;
  fleet: ResolvedFleet;
} {
  const fleet = resolveFleet({
    name: "t",
    model: "qwen2.5-coder:latest",
    cwd: "/tmp",
    agents: [{ id: "a", role: "worker", prompt: "do it" }],
  });
  return { agent: { ...fleet.agents[0]!, retries: 0, ...agentOverrides }, fleet };
}

function ctx(exec: ReturnType<typeof fakeExec>, extra: Record<string, unknown> = {}) {
  const { fleet } = setup();
  return {
    fleet,
    mode: "direct" as const,
    bus: new EventBus(),
    log: silent,
    exec,
    backoffMs: 0,
    streamText: false,
    ...extra,
  };
}

describe("runAgent", () => {
  test("a clean run succeeds and reports what happened", async () => {
    const { agent } = setup();
    const result = await runAgent(
      agent,
      ctx(
        fakeExec(() => ({
          lines: claudeStream({
            text: "finished the job",
            tools: [{ name: "Bash", input: { command: "ls" } }],
            numTurns: 4,
          }),
        })),
      ),
    );
    expect(result.status).toBe("succeeded");
    expect(result.text).toBe("finished the job");
    expect(result.numTurns).toBe(4);
    expect(result.sessionId).toBe("sess-1234abcd");
    expect(result.toolCalls.map((t) => t.name)).toEqual(["Bash"]);
    expect(result.attempts).toBe(1);
    expect(result.error).toBeUndefined();
  });

  test("output arriving in tiny chunks parses identically", async () => {
    const { agent } = setup();
    const result = await runAgent(
      agent,
      ctx(fakeExec(() => ({ lines: claudeStream({ text: "chunked" }), chunkSize: 3 }))),
    );
    expect(result.status).toBe("succeeded");
    expect(result.text).toBe("chunked");
  });

  test("a non-zero exit is a failure with the exit code recorded", async () => {
    const { agent } = setup();
    const result = await runAgent(
      agent,
      ctx(fakeExec(() => ({ lines: [], exitCode: 3, stderr: "claude blew up" }))),
    );
    expect(result.status).toBe("failed");
    expect(result.exitCode).toBe(3);
    expect(result.stderr).toContain("claude blew up");
    expect(result.error).toContain("code 3");
  });

  test("an error result event fails even when the process exits 0", async () => {
    const { agent } = setup();
    const result = await runAgent(
      agent,
      ctx(fakeExec(() => ({ lines: claudeStream({ text: "hit the turn limit", isError: true }) }))),
    );
    expect(result.status).toBe("failed");
    expect(result.error).toBe("hit the turn limit");
  });

  test("a timeout is reported as such, not as a generic failure", async () => {
    const { agent } = setup({ timeoutMs: 50 });
    const result = await runAgent(
      agent,
      ctx(fakeExec(() => ({ lines: [], exitCode: null, timedOut: true }))),
    );
    expect(result.status).toBe("timeout");
    expect(result.error).toContain("timed out after 50ms");
  });

  test("a spawn failure is captured rather than thrown", async () => {
    const { agent } = setup();
    const result = await runAgent(
      agent,
      ctx(fakeExec(() => ({ exitCode: null, error: "claude: command not found" }))),
    );
    expect(result.status).toBe("failed");
    expect(result.error).toBe("claude: command not found");
  });

  test("retries until it succeeds and counts the attempts", async () => {
    const { agent } = setup({ retries: 2 });
    const result = await runAgent(
      agent,
      ctx(
        fakeExec((_plan, attempt): FakeRun =>
          attempt < 3
            ? { lines: [], exitCode: 1 }
            : { lines: claudeStream({ text: "third time lucky" }) },
        ),
      ),
    );
    expect(result.status).toBe("succeeded");
    expect(result.attempts).toBe(3);
    expect(result.text).toBe("third time lucky");
  });

  test("gives up after the retry budget", async () => {
    const { agent } = setup({ retries: 1 });
    let calls = 0;
    const result = await runAgent(
      agent,
      ctx(
        fakeExec(() => {
          calls += 1;
          return { lines: [], exitCode: 1 };
        }),
      ),
    );
    expect(calls).toBe(2);
    expect(result.status).toBe("failed");
    expect(result.attempts).toBe(2);
  });

  test("falls back to streamed assistant text when no result event arrives", async () => {
    const { agent } = setup();
    const partial = claudeStream({ text: "partial answer" }).slice(0, -1);
    const result = await runAgent(agent, ctx(fakeExec(() => ({ lines: partial }))));
    expect(result.status).toBe("succeeded");
    expect(result.text).toBe("partial answer");
  });

  test("an aborted signal stops the agent before spawning", async () => {
    const { agent } = setup();
    const controller = new AbortController();
    controller.abort();
    let spawned = false;
    const result = await runAgent(
      agent,
      ctx(
        fakeExec(() => {
          spawned = true;
          return {};
        }),
        { signal: controller.signal },
      ),
    );
    expect(spawned).toBe(false);
    expect(result.status).toBe("cancelled");
    expect(result.attempts).toBe(0);
  });

  test("emits the lifecycle events observers rely on", async () => {
    const { agent, fleet } = setup();
    const bus = new EventBus();
    const seen: string[] = [];
    bus.on("agent:start", () => seen.push("start"));
    bus.on("agent:tool", (p) => seen.push(`tool:${p.tool.name}`));
    bus.on("agent:tool-result", () => seen.push("tool-result"));
    bus.on("agent:text", () => seen.push("text"));
    bus.on("agent:end", (p) => seen.push(`end:${p.result.status}`));

    await runAgent(agent, {
      fleet,
      mode: "direct",
      bus,
      log: silent,
      backoffMs: 0,
      streamText: false,
      exec: fakeExec(() => ({
        lines: claudeStream({ text: "ok", tools: [{ name: "Read", input: { file_path: "x" } }] }),
      })),
    });
    expect(seen).toEqual(["start", "tool:Read", "tool-result", "text", "end:succeeded"]);
  });

  test("a throwing event listener does not break the run", async () => {
    const { agent, fleet } = setup();
    const bus = new EventBus();
    bus.on("agent:text", () => {
      throw new Error("observer exploded");
    });
    const result = await runAgent(agent, {
      fleet,
      mode: "direct",
      bus,
      log: silent,
      backoffMs: 0,
      streamText: false,
      exec: fakeExec(() => ({ lines: claudeStream({ text: "still fine" }) })),
    });
    expect(result.status).toBe("succeeded");
  });

  test("the prompt reaches the child on stdin", async () => {
    const { agent } = setup();
    const seen: LaunchPlan[] = [];
    await runAgent(
      agent,
      ctx(fakeExec(() => ({ lines: claudeStream({ text: "ok" }) }), seen)),
    );
    expect(seen[0]!.stdin).toBe("do it");
    expect(seen[0]!.cwd).toBe("/tmp");
  });

  test("every raw NDJSON line is offered to the transcript sink", async () => {
    const { agent } = setup();
    const lines: string[] = [];
    await runAgent(
      agent,
      ctx(fakeExec(() => ({ lines: claudeStream({ text: "ok" }) })), {
        sink: (_id: string, line: string) => lines.push(line),
      }),
    );
    expect(lines).toHaveLength(3);
    expect(JSON.parse(lines[0]!).type).toBe("system");
  });
});
