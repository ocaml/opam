import { afterAll, describe, expect, test } from "bun:test";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { Fleet } from "../src/orchestrator.ts";
import { Logger } from "../src/logger.ts";
import { resolveFleet } from "../src/plan.ts";
import type { FleetPlan, LaunchPlan } from "../src/types.ts";
import { claudeStream, fakeExec, happyProbes, type FakeRun } from "./helpers.ts";

const silent = new Logger("silent", { color: false, write: () => {} });
const temps: string[] = [];

afterAll(async () => {
  await Promise.all(temps.map((dir) => rm(dir, { recursive: true, force: true })));
});

async function tempDir(): Promise<string> {
  const dir = await mkdtemp(join(tmpdir(), "agentic-test-"));
  temps.push(dir);
  return dir;
}

/** Build a fleet whose child processes are faked. */
function makeFleet(
  plan: FleetPlan,
  script: (plan: LaunchPlan, attempt: number) => FakeRun,
  seen?: LaunchPlan[],
): Fleet {
  const spec = resolveFleet({ noArtifacts: true, ...plan });
  return new Fleet(spec, {
    log: silent,
    exec: fakeExec(script, seen),
    backoffMs: 0,
    streamText: false,
    probes: happyProbes,
  });
}

/** Reply with whatever prompt the agent was given, so tests can assert on it. */
const echoPrompt = (plan: LaunchPlan): FakeRun => ({
  lines: claudeStream({ text: `echo:${plan.stdin ?? ""}` }),
});

describe("Fleet.run", () => {
  test("runs a single agent and reports success", async () => {
    const fleet = makeFleet(
      { name: "solo", agents: [{ id: "a", prompt: "do it" }] },
      () => ({ lines: claudeStream({ text: "done" }) }),
    );
    const run = await fleet.run();
    expect(run.ok).toBe(true);
    expect(run.results).toHaveLength(1);
    expect(run.results[0]!.text).toBe("done");
    expect(run.board.a).toBe("done");
  });

  test("passes an upstream agent's output into a downstream prompt", async () => {
    const fleet = makeFleet(
      {
        name: "chain",
        vars: { topic: "widgets" },
        agents: [
          { id: "first", prompt: "research {{vars.topic}}" },
          { id: "second", dependsOn: ["first"], prompt: "build on: {{outputs.first}}" },
        ],
      },
      echoPrompt,
    );
    const run = await fleet.run();
    expect(run.ok).toBe(true);
    expect(run.results[0]!.text).toBe("echo:research widgets");
    expect(run.results[1]!.text).toBe("echo:build on: echo:research widgets");
  });

  test("publishes to outputKey as well as the agent id", async () => {
    const fleet = makeFleet(
      {
        name: "keys",
        agents: [
          { id: "a", prompt: "p", outputKey: "findings" },
          { id: "b", dependsOn: ["a"], prompt: "saw {{board.findings}}" },
        ],
      },
      echoPrompt,
    );
    const run = await fleet.run();
    expect(run.board.findings).toBe("echo:p");
    expect(run.results[1]!.text).toBe("echo:saw echo:p");
  });

  test("runs independent agents in parallel up to the concurrency limit", async () => {
    let inFlight = 0;
    let peak = 0;
    const fleet = makeFleet(
      {
        name: "parallel",
        concurrency: 2,
        agents: Array.from({ length: 4 }, (_, i) => ({ id: `a${i}`, prompt: "p" })),
      },
      () => {
        inFlight += 1;
        peak = Math.max(peak, inFlight);
        inFlight -= 1;
        return { lines: claudeStream({ text: "ok" }) };
      },
    );
    const run = await fleet.run();
    expect(run.ok).toBe(true);
    expect(peak).toBeLessThanOrEqual(2);
    expect(run.results).toHaveLength(4);
  });

  test("a failed agent skips its dependents and marks the run not ok", async () => {
    const fleet = makeFleet(
      {
        name: "failing",
        agents: [
          { id: "a", prompt: "p", retries: 0 },
          { id: "b", dependsOn: ["a"], prompt: "p" },
          { id: "c", prompt: "p" },
        ],
      },
      (plan) =>
        plan.env.AGENTIC_AGENT_ID === "a"
          ? { lines: [], exitCode: 1 }
          : { lines: claudeStream({ text: "ok" }) },
    );
    const run = await fleet.run();
    expect(run.ok).toBe(false);
    const byId = new Map(run.results.map((r) => [r.id, r]));
    expect(byId.get("a")!.status).toBe("failed");
    expect(byId.get("b")!.status).toBe("skipped");
    expect(byId.get("c")!.status).toBe("succeeded");
  });

  test("an unresolvable placeholder fails the agent before spawning it", async () => {
    let spawned = 0;
    const fleet = makeFleet(
      { name: "bad-template", agents: [{ id: "a", prompt: "needs {{vars.absent}}" }] },
      () => {
        spawned += 1;
        return { lines: claudeStream({ text: "ok" }) };
      },
    );
    const run = await fleet.run();
    expect(spawned).toBe(0);
    expect(run.ok).toBe(false);
    expect(run.results[0]!.status).toBe("failed");
    expect(run.results[0]!.error).toContain("{{vars.absent}}");
  });

  test("failFast cancels the agents that have not started", async () => {
    const fleet = makeFleet(
      {
        name: "fail-fast",
        concurrency: 1,
        failFast: true,
        agents: [
          { id: "a", prompt: "p", retries: 0 },
          { id: "b", prompt: "p" },
        ],
      },
      (plan) =>
        plan.env.AGENTIC_AGENT_ID === "a"
          ? { lines: [], exitCode: 1 }
          : { lines: claudeStream({ text: "ok" }) },
    );
    const run = await fleet.run();
    expect(run.results.find((r) => r.id === "b")!.status).toBe("cancelled");
  });

  test("per-agent retries are honoured inside a fleet", async () => {
    const fleet = makeFleet(
      { name: "retry", agents: [{ id: "a", prompt: "p", retries: 2 }] },
      (_plan, attempt) =>
        attempt < 3 ? { lines: [], exitCode: 1 } : { lines: claudeStream({ text: "ok" }) },
    );
    const run = await fleet.run();
    expect(run.ok).toBe(true);
    expect(run.results[0]!.attempts).toBe(3);
  });

  test("preflight fills in the model and reaches every agent", async () => {
    const seen: LaunchPlan[] = [];
    const fleet = makeFleet(
      { name: "model", agents: [{ id: "a", prompt: "p" }] },
      () => ({ lines: claudeStream({ text: "ok" }) }),
      seen,
    );
    await fleet.run();
    // happyProbes reports qwen2.5-coder first; resolveModel prefers tool-capable.
    expect(fleet.spec.model).toBe("qwen2.5-coder:latest");
    expect(seen[0]!.cmd).toContain("qwen2.5-coder:latest");
  });

  test("an agent-level model pin survives preflight", async () => {
    const seen: LaunchPlan[] = [];
    const fleet = makeFleet(
      { name: "pin", agents: [{ id: "a", prompt: "p", model: "llama3.1:8b" }] },
      () => ({ lines: claudeStream({ text: "ok" }) }),
      seen,
    );
    await fleet.run();
    expect(seen[0]!.cmd).toContain("llama3.1:8b");
  });

  test("launch mode is chosen when `ollama launch` is available", async () => {
    const seen: LaunchPlan[] = [];
    const fleet = makeFleet(
      { name: "launch", agents: [{ id: "a", prompt: "p" }] },
      () => ({ lines: claudeStream({ text: "ok" }) }),
      seen,
    );
    await fleet.run();
    expect(seen[0]!.cmd.slice(0, 3)).toEqual(["ollama", "launch", "claude"]);
  });

  test("direct mode is chosen when it is not", async () => {
    const spec = resolveFleet({ name: "d", noArtifacts: true, agents: [{ id: "a", prompt: "p" }] });
    const seen: LaunchPlan[] = [];
    const fleet = new Fleet(spec, {
      log: silent,
      exec: fakeExec(() => ({ lines: claudeStream({ text: "ok" }) }), seen),
      backoffMs: 0,
      streamText: false,
      probes: { ...happyProbes, launchSupport: async () => false },
    });
    await fleet.run();
    expect(seen[0]!.cmd[0]).toBe("claude");
    expect(seen[0]!.env.ANTHROPIC_BASE_URL).toBe("http://127.0.0.1:11434");
  });

  test("a fleet-level cancellation stops the run", async () => {
    const controller = new AbortController();
    const spec = resolveFleet({
      name: "cancel",
      concurrency: 1,
      noArtifacts: true,
      agents: [
        { id: "a", prompt: "p" },
        { id: "b", prompt: "p" },
      ],
    });
    const fleet = new Fleet(spec, {
      log: silent,
      backoffMs: 0,
      streamText: false,
      probes: happyProbes,
      signal: controller.signal,
      exec: fakeExec(() => {
        controller.abort();
        return { lines: claudeStream({ text: "ok" }) };
      }),
    });
    const run = await fleet.run();
    expect(run.results.find((r) => r.id === "b")!.status).toBe("cancelled");
  });

  test("preflight warns about an unreachable Ollama without crashing", async () => {
    const spec = resolveFleet({ name: "down", noArtifacts: true, agents: [{ id: "a", prompt: "p" }] });
    const fleet = new Fleet(spec, {
      log: silent,
      probes: {
        launchSupport: async () => false,
        ollama: async (host) => ({
          reachable: false,
          host,
          version: "",
          models: [],
          error: "ECONNREFUSED",
        }),
        warm: async () => false,
      },
    });
    const report = await fleet.preflight();
    expect(report.mode).toBe("direct");
    expect(report.warnings.join(" ")).toContain("not reachable");
    expect(report.warnings.join(" ")).toContain("ollama serve");
  });

  test("preflight warns when the model is not known to support tool calling", async () => {
    const spec = resolveFleet({
      name: "weak",
      model: "tinyllama",
      noArtifacts: true,
      agents: [{ id: "a", prompt: "p" }],
    });
    const fleet = new Fleet(spec, { log: silent, probes: happyProbes });
    const report = await fleet.preflight();
    expect(report.warnings.join(" ")).toContain("tool-calling");
  });
});

describe("Fleet.dryRun", () => {
  test("returns dependency layers and one command per agent, spawning nothing", async () => {
    const fleet = makeFleet(
      {
        name: "dry",
        agents: [
          { id: "a", prompt: "p" },
          { id: "b", prompt: "p" },
          { id: "c", dependsOn: ["a", "b"], prompt: "merge {{outputs.a}}" },
        ],
      },
      () => {
        throw new Error("dry run must not spawn");
      },
    );
    const { layers, plans } = await fleet.dryRun();
    expect(layers).toEqual([["a", "b"], ["c"]]);
    expect(plans).toHaveLength(3);
    // The unresolved upstream placeholder stays visible instead of exploding.
    expect(plans[2]!.plan.stdin).toContain("{{outputs.a}}");
  });
});

describe("artifacts", () => {
  test("writes transcripts, per-agent output and both summaries", async () => {
    const dir = await tempDir();
    const spec = resolveFleet({
      name: "artifacts",
      artifactsDir: dir,
      agents: [
        { id: "a", prompt: "p" },
        { id: "b", dependsOn: ["a"], prompt: "after {{outputs.a}}" },
      ],
    });
    const fleet = new Fleet(spec, {
      log: silent,
      backoffMs: 0,
      streamText: false,
      probes: happyProbes,
      exec: fakeExec(() => ({
        lines: claudeStream({ text: "written", tools: [{ name: "Write", input: { path: "x" } }] }),
      })),
    });
    const run = await fleet.run();

    expect(run.artifactsDir.startsWith(dir)).toBe(true);
    expect(await Bun.file(join(run.artifactsDir, "a.jsonl")).exists()).toBe(true);
    expect(await Bun.file(join(run.artifactsDir, "a.md")).text()).toContain("written");

    const summary = JSON.parse(await Bun.file(join(run.artifactsDir, "summary.json")).text());
    expect(summary.runId).toBe(run.runId);
    expect(summary.results).toHaveLength(2);
    expect(summary.preflight.mode).toBe("launch");

    const markdown = await Bun.file(join(run.artifactsDir, "summary.md")).text();
    expect(markdown).toContain("| a | a | succeeded |");
    expect(markdown).toContain("## b");

    const transcript = (await Bun.file(join(run.artifactsDir, "a.jsonl")).text())
      .trim()
      .split("\n");
    expect(transcript.length).toBe(5);
    expect(JSON.parse(transcript[0]!).type).toBe("system");
  });

  test("noArtifacts writes nothing", async () => {
    const dir = await tempDir();
    const fleet = makeFleet(
      { name: "none", artifactsDir: dir, noArtifacts: true, agents: [{ id: "a", prompt: "p" }] },
      () => ({ lines: claudeStream({ text: "ok" }) }),
    );
    const run = await fleet.run();
    expect(run.artifactsDir).toBe("");
    expect(await Bun.file(join(dir, "summary.json")).exists()).toBe(false);
  });
});
