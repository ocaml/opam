/**
 * End-to-end through real OS processes: no fake exec, no injected runner.
 *
 * A shell shim stands in for `claude` and speaks the same NDJSON protocol, so
 * this exercises Bun.spawn, stdin delivery, incremental stdout parsing, exit
 * codes, timeouts and artifact writing exactly as a real run would.
 */

import { afterAll, describe, expect, test } from "bun:test";
import { chmod, mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { Fleet } from "../src/orchestrator.ts";
import { Logger } from "../src/logger.ts";
import { resolveFleet } from "../src/plan.ts";
import type { FleetPlan } from "../src/types.ts";

const silent = new Logger("silent", { color: false, write: () => {} });
const dirs: string[] = [];

afterAll(async () => {
  await Promise.all(dirs.map((d) => rm(d, { recursive: true, force: true })));
});

async function workspace(): Promise<string> {
  const dir = await mkdtemp(join(tmpdir(), "agentic-e2e-"));
  dirs.push(dir);
  return dir;
}

/** A shim that reads the prompt on stdin and answers in Claude Code's format. */
const ECHO_SHIM = `#!/bin/sh
prompt=$(cat)
printf '%s\\n' '{"type":"system","subtype":"init","session_id":"e2e","model":"shim","tools":["Bash"]}'
printf '%s\\n' '{"type":"assistant","message":{"content":[{"type":"tool_use","name":"Bash","input":{"command":"echo hi"}}]}}'
printf '%s\\n' '{"type":"user","message":{"content":[{"type":"tool_result","content":"hi"}]}}'
printf '{"type":"result","subtype":"success","is_error":false,"num_turns":2,"session_id":"e2e","total_cost_usd":0,"result":"%s"}\\n' "$prompt"
`;

const FAIL_SHIM = `#!/bin/sh
cat > /dev/null
echo "shim exploded" >&2
exit 7
`;

const SLOW_SHIM = `#!/bin/sh
cat > /dev/null
sleep 30
`;

async function writeShim(dir: string, name: string, body: string): Promise<string> {
  const path = join(dir, name);
  await Bun.write(path, body);
  await chmod(path, 0o755);
  return path;
}

/** Run a plan for real, with `shim` standing in for the claude binary. */
function fleetWith(shim: string, plan: FleetPlan, artifactsDir?: string): Fleet {
  const spec = resolveFleet({
    launchMode: "launch",
    launchCommand: [shim],
    skipPreflight: true,
    skipWarm: true,
    noArtifacts: artifactsDir === undefined,
    ...(artifactsDir ? { artifactsDir } : {}),
    ...plan,
  });
  return new Fleet(spec, { log: silent, backoffMs: 0, streamText: false });
}

describe("end to end with real processes", () => {
  test("a two-agent chain spawns real processes and passes output downstream", async () => {
    const dir = await workspace();
    const shim = await writeShim(dir, "claude-shim.sh", ECHO_SHIM);
    const fleet = fleetWith(shim, {
      name: "e2e-chain",
      vars: { subject: "the parser" },
      agents: [
        { id: "first", prompt: "look at {{vars.subject}}" },
        { id: "second", dependsOn: ["first"], prompt: "then: {{outputs.first}}" },
      ],
    });

    const run = await fleet.run();
    expect(run.ok).toBe(true);
    expect(run.results[0]!.text).toBe("look at the parser");
    expect(run.results[0]!.toolCalls.map((t) => t.name)).toEqual(["Bash"]);
    expect(run.results[0]!.numTurns).toBe(2);
    expect(run.results[0]!.sessionId).toBe("e2e");
    expect(run.results[1]!.text).toBe("then: look at the parser");
  });

  test("four agents really do run concurrently", async () => {
    const dir = await workspace();
    const shim = await writeShim(dir, "claude-shim.sh", ECHO_SHIM);
    const fleet = fleetWith(shim, {
      name: "e2e-parallel",
      concurrency: 4,
      agents: Array.from({ length: 4 }, (_, i) => ({ id: `a${i}`, prompt: `job ${i}` })),
    });

    const started: number[] = [];
    fleet.bus.on("agent:start", () => started.push(Date.now()));
    const run = await fleet.run();

    expect(run.ok).toBe(true);
    expect(started).toHaveLength(4);
    // All four launch before the first finishes; a serial run could not do this.
    expect(Math.max(...started) - Math.min(...started)).toBeLessThan(run.durationMs);
    expect(run.results.map((r) => r.text)).toEqual(["job 0", "job 1", "job 2", "job 3"]);
  });

  test("a non-zero exit is captured with its stderr, and dependents are skipped", async () => {
    const dir = await workspace();
    const shim = await writeShim(dir, "claude-fail.sh", FAIL_SHIM);
    const fleet = fleetWith(shim, {
      name: "e2e-fail",
      agents: [
        { id: "boom", prompt: "p", retries: 0 },
        { id: "after", dependsOn: ["boom"], prompt: "p" },
      ],
    });
    const run = await fleet.run();
    expect(run.ok).toBe(false);
    expect(run.results[0]!.status).toBe("failed");
    expect(run.results[0]!.exitCode).toBe(7);
    expect(run.results[0]!.stderr).toContain("shim exploded");
    expect(run.results[1]!.status).toBe("skipped");
  });

  test("a hung process is killed at the timeout", async () => {
    const dir = await workspace();
    const shim = await writeShim(dir, "claude-slow.sh", SLOW_SHIM);
    const fleet = fleetWith(shim, {
      name: "e2e-timeout",
      agents: [{ id: "hang", prompt: "p", timeoutMs: 300, retries: 0 }],
    });
    const started = Date.now();
    const run = await fleet.run();
    expect(run.results[0]!.status).toBe("timeout");
    expect(Date.now() - started).toBeLessThan(10_000);
  }, 20_000);

  test("artifacts land on disk for a real run", async () => {
    const dir = await workspace();
    const shim = await writeShim(dir, "claude-shim.sh", ECHO_SHIM);
    const artifacts = join(dir, "runs");
    const fleet = fleetWith(
      shim,
      { name: "e2e-artifacts", agents: [{ id: "solo", prompt: "hello world" }] },
      artifacts,
    );
    const run = await fleet.run();

    expect(await Bun.file(join(run.artifactsDir, "solo.md")).text()).toContain("hello world");
    const transcript = (await Bun.file(join(run.artifactsDir, "solo.jsonl")).text()).trim();
    expect(transcript.split("\n")).toHaveLength(4);
    const summary = JSON.parse(await Bun.file(join(run.artifactsDir, "summary.json")).text());
    expect(summary.ok).toBe(true);
    expect(summary.results[0].status).toBe("succeeded");
  });
});
