import { afterEach, describe, expect, test } from "bun:test";
import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { main, VERSION } from "../src/cli.ts";

const realStdout = process.stdout.write.bind(process.stdout);
const realStderr = process.stderr.write.bind(process.stderr);
const dirs: string[] = [];

afterEach(async () => {
  process.stdout.write = realStdout;
  process.stderr.write = realStderr;
  await Promise.all(dirs.splice(0).map((d) => rm(d, { recursive: true, force: true })));
});

/** Run the CLI with stdout/stderr captured. */
async function run(args: string[]): Promise<{ code: number; out: string; err: string }> {
  let out = "";
  let err = "";
  process.stdout.write = ((chunk: string) => {
    out += chunk;
    return true;
  }) as typeof process.stdout.write;
  process.stderr.write = ((chunk: string) => {
    err += chunk;
    return true;
  }) as typeof process.stderr.write;
  const code = await main(args);
  return { code, out, err };
}

describe("cli", () => {
  test("help exits 0 and lists the commands", async () => {
    const { code, out } = await run(["help"]);
    expect(code).toBe(0);
    expect(out).toContain("agentic <command>");
    expect(out).toContain("wizard");
    expect(out).toContain("doctor");
  });

  test("no arguments prints usage", async () => {
    const { code, out } = await run([]);
    expect(code).toBe(0);
    expect(out).toContain("USAGE");
  });

  test("--version prints the version", async () => {
    const { code, out } = await run(["--version"]);
    expect(code).toBe(0);
    expect(out.trim()).toBe(VERSION);
  });

  test("an unknown command exits 2", async () => {
    const { code } = await run(["frobnicate", "-q"]);
    expect(code).toBe(2);
  });

  test("an unknown flag exits 2 with usage", async () => {
    const { code, err } = await run(["run", "--nonsense"]);
    expect(code).toBe(2);
    expect(err).toContain("USAGE");
  });

  test("run with neither a plan nor -p exits 2 and points at the wizard", async () => {
    const { code, err } = await run(["run"]);
    expect(code).toBe(2);
    expect(err).toContain("agentic wizard");
  });

  test("run with a missing plan file exits 2", async () => {
    const { code, err } = await run(["run", "/nonexistent/plan.json"]);
    expect(code).toBe(2);
    expect(err).toContain("plan file not found");
  });

  test("an unknown preset exits 2", async () => {
    const { code, err } = await run(["run", "-p", "task", "--preset", "nope", "--dry-run"]);
    expect(code).toBe(2);
    expect(err).toContain('unknown preset "nope"');
  });

  test("an invalid --mode exits 2", async () => {
    const { code, err } = await run(["run", "-p", "t", "--mode", "sideways", "--dry-run"]);
    expect(code).toBe(2);
    expect(err).toContain("--mode must be");
  });

  test("a malformed --var exits 2", async () => {
    const { code, err } = await run(["run", "-p", "t", "--var", "novalue", "--dry-run"]);
    expect(code).toBe(2);
    expect(err).toContain("key=value");
  });

  test("--dry-run resolves commands in dependency order and spawns nothing", async () => {
    const { code, out } = await run([
      "run",
      "-p",
      "check the tests",
      "-n",
      "2",
      "--preset",
      "fanout",
      "--dry-run",
      "--no-preflight",
      "--json",
    ]);
    expect(code).toBe(0);
    const payload = JSON.parse(out);
    expect(payload.layers).toEqual([["worker1", "worker2"], ["synthesis"]]);
    expect(payload.plans).toHaveLength(3);
    expect(payload.plans[0].plan.cmd).toContain("--output-format");
  });

  test("--dry-run on a plan file honours its dependencies", async () => {
    const dir = await mkdtemp(join(tmpdir(), "agentic-cli-"));
    dirs.push(dir);
    const planPath = join(dir, "plan.json");
    await Bun.write(
      planPath,
      JSON.stringify({
        name: "p",
        agents: [
          { id: "a", prompt: "first" },
          { id: "b", dependsOn: ["a"], prompt: "second {{outputs.a}}" },
        ],
      }),
    );
    const { code, out } = await run(["run", planPath, "--dry-run", "--no-preflight", "--json"]);
    expect(code).toBe(0);
    expect(JSON.parse(out).layers).toEqual([["a"], ["b"]]);
  });

  test("an invalid plan file exits 2 with the reason", async () => {
    const dir = await mkdtemp(join(tmpdir(), "agentic-cli-"));
    dirs.push(dir);
    const planPath = join(dir, "bad.json");
    await Bun.write(planPath, JSON.stringify({ agents: [{ id: "a" }] }));
    const { code, err } = await run(["run", planPath, "--dry-run"]);
    expect(code).toBe(2);
    expect(err).toContain("non-empty prompt");
  });

  test("init writes an example plan and refuses to overwrite it", async () => {
    const dir = await mkdtemp(join(tmpdir(), "agentic-cli-"));
    dirs.push(dir);
    const target = join(dir, "plan.json");

    const first = await run(["init", target]);
    expect(first.code).toBe(0);
    expect(await Bun.file(target).exists()).toBe(true);

    const second = await run(["init", target]);
    expect(second.code).toBe(2);
    expect(second.err).toContain("already exists");
  });

  test("the plan written by init is itself valid and runnable", async () => {
    const dir = await mkdtemp(join(tmpdir(), "agentic-cli-"));
    dirs.push(dir);
    const target = join(dir, "plan.json");
    await run(["init", target]);
    const { code, out } = await run(["run", target, "--dry-run", "--no-preflight", "--json"]);
    expect(code).toBe(0);
    expect(JSON.parse(out).layers).toEqual([["implement"], ["review"], ["fix"]]);
  });

  test("--json keeps stdout clean for machine consumption", async () => {
    const { out } = await run([
      "run",
      "-p",
      "t",
      "-n",
      "1",
      "--preset",
      "solo",
      "--dry-run",
      "--no-preflight",
      "--json",
    ]);
    expect(() => JSON.parse(out)).not.toThrow();
  });
});
