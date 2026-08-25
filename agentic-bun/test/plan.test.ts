import { describe, expect, test } from "bun:test";
import {
  buildPreset,
  DEFAULTS,
  parsePlan,
  PlanError,
  resolveFleet,
  stripJsonComments,
  validatePlan,
} from "../src/plan.ts";
import { validateGraph } from "../src/scheduler.ts";
import { placeholders } from "../src/template.ts";

describe("stripJsonComments", () => {
  test("removes line and block comments", () => {
    expect(stripJsonComments('{ // hi\n "a": 1 /* there */ }')).toContain('"a": 1');
    expect(stripJsonComments('{ // hi\n "a": 1 }')).not.toContain("hi");
  });

  test("leaves comment-like text inside strings alone", () => {
    const text = '{"url": "http://x/y", "note": "a /* b */ c"}';
    expect(JSON.parse(stripJsonComments(text))).toEqual({
      url: "http://x/y",
      note: "a /* b */ c",
    });
  });

  test("survives escaped quotes", () => {
    const text = '{"a": "he said \\"hi\\" // not a comment"}';
    expect(JSON.parse(stripJsonComments(text)).a).toBe('he said "hi" // not a comment');
  });
});

describe("validatePlan", () => {
  const ok = { agents: [{ id: "a", prompt: "p" }] };

  test("accepts a minimal plan", () => {
    expect(validatePlan(ok).agents).toHaveLength(1);
  });

  test("rejects non-objects and empty agent lists", () => {
    expect(() => validatePlan(null)).toThrow(PlanError);
    expect(() => validatePlan({ agents: [] })).toThrow(/non-empty/);
    expect(() => validatePlan({})).toThrow(/non-empty/);
  });

  test("rejects an unsupported version", () => {
    expect(() => validatePlan({ version: 2, ...ok })).toThrow(/unsupported plan version/);
  });

  test("rejects bad ids, duplicates and empty prompts", () => {
    expect(() => validatePlan({ agents: [{ id: "has space", prompt: "p" }] })).toThrow(/id must/);
    expect(() => validatePlan({ agents: [{ id: "a", prompt: "p" }, { id: "a", prompt: "q" }] })).toThrow(
      /duplicate agent id/,
    );
    expect(() => validatePlan({ agents: [{ id: "a", prompt: "  " }] })).toThrow(/non-empty prompt/);
  });

  test("rejects dangling dependencies", () => {
    expect(() =>
      validatePlan({ agents: [{ id: "a", prompt: "p", dependsOn: ["ghost"] }] }),
    ).toThrow(/unknown agent "ghost"/);
  });

  test("rejects invalid enum values", () => {
    expect(() => validatePlan({ ...ok, permissionMode: "nope" })).toThrow(/permissionMode/);
    expect(() => validatePlan({ ...ok, launchMode: "nope" })).toThrow(/launchMode/);
    expect(() => validatePlan({ ...ok, promptVia: "nope" })).toThrow(/promptVia/);
  });

  test("rejects wrong types with a pointed message", () => {
    expect(() => validatePlan({ ...ok, allowedTools: "Read" })).toThrow(/array of strings/);
    expect(() => validatePlan({ ...ok, vars: { a: 1 } })).toThrow(/string values/);
    expect(() => validatePlan({ ...ok, concurrency: -1 })).toThrow(/non-negative/);
  });

  test("parsePlan reports invalid JSON with the source name", () => {
    expect(() => parsePlan("{ nope", "my-plan.json")).toThrow(/my-plan.json: not valid JSON/);
  });

  test("parsePlan accepts comments", () => {
    const plan = parsePlan('{ /* c */ "agents": [{ "id": "a", "prompt": "p" }] }');
    expect(plan.agents[0]!.id).toBe("a");
  });
});

describe("resolveFleet", () => {
  test("applies built-in defaults", () => {
    const fleet = resolveFleet({ agents: [{ id: "a", prompt: "p" }] });
    expect(fleet.concurrency).toBe(DEFAULTS.concurrency);
    expect(fleet.agents[0]!.permissionMode).toBe(DEFAULTS.permissionMode);
    expect(fleet.agents[0]!.maxTurns).toBe(DEFAULTS.maxTurns);
    expect(fleet.agents[0]!.role).toBe("a");
  });

  test("agent values beat fleet values, which beat defaults", () => {
    const fleet = resolveFleet({
      maxTurns: 5,
      model: "fleet-model",
      agents: [
        { id: "a", prompt: "p" },
        { id: "b", prompt: "p", maxTurns: 9, model: "agent-model" },
      ],
    });
    expect(fleet.agents[0]!.maxTurns).toBe(5);
    expect(fleet.agents[0]!.model).toBe("fleet-model");
    expect(fleet.agents[1]!.maxTurns).toBe(9);
    expect(fleet.agents[1]!.model).toBe("agent-model");
  });

  test("overrides beat everything", () => {
    const fleet = resolveFleet(
      { maxTurns: 5, concurrency: 2, agents: [{ id: "a", prompt: "p", maxTurns: 9 }] },
      { maxTurns: 1, concurrency: 7, model: "cli-model" },
    );
    expect(fleet.concurrency).toBe(7);
    expect(fleet.agents[0]!.maxTurns).toBe(9); // agent pin still wins
    expect(fleet.agents[0]!.model).toBe("cli-model");
  });

  test("relative paths resolve against the fleet cwd", () => {
    const fleet = resolveFleet({
      cwd: "/tmp/work",
      artifactsDir: "out",
      agents: [{ id: "a", prompt: "p", cwd: "sub", addDirs: ["extra"] }],
    });
    expect(fleet.artifactsDir).toBe("/tmp/work/out");
    expect(fleet.agents[0]!.cwd).toBe("/tmp/work/sub");
    expect(fleet.agents[0]!.addDirs).toEqual(["/tmp/work/sub/extra"]);
  });

  test("absolute paths are left alone", () => {
    const fleet = resolveFleet({
      cwd: "/tmp/work",
      artifactsDir: "/var/out",
      agents: [{ id: "a", prompt: "p", cwd: "/srv" }],
    });
    expect(fleet.artifactsDir).toBe("/var/out");
    expect(fleet.agents[0]!.cwd).toBe("/srv");
  });

  test("vars merge, with overrides on top", () => {
    const fleet = resolveFleet(
      { vars: { a: "1", b: "2" }, agents: [{ id: "x", prompt: "p" }] },
      { vars: { b: "override" } },
    );
    expect(fleet.vars).toEqual({ a: "1", b: "override" });
  });
});

describe("presets", () => {
  const graph = (plan: ReturnType<typeof buildPreset>) =>
    plan.agents.map((a) => ({ id: a.id, dependsOn: a.dependsOn ?? [] }));

  test("every preset produces a valid plan and a valid DAG", () => {
    for (const preset of ["solo", "fanout", "pipeline", "review"] as const) {
      const plan = buildPreset(preset, "do a thing", { agents: 3 });
      expect(() => validatePlan(plan)).not.toThrow();
      expect(() => validateGraph(graph(plan))).not.toThrow();
    }
  });

  test("fanout is N workers plus a synthesiser that depends on all of them", () => {
    const plan = buildPreset("fanout", "task", { agents: 4 });
    expect(plan.agents).toHaveLength(5);
    const synthesis = plan.agents.at(-1)!;
    expect(synthesis.id).toBe("synthesis");
    expect(synthesis.dependsOn).toEqual(["worker1", "worker2", "worker3", "worker4"]);
  });

  test("pipeline chains each step onto the previous one", () => {
    const plan = buildPreset("pipeline", "task", { agents: 3 });
    expect(plan.agents.map((a) => a.dependsOn ?? [])).toEqual([[], ["step1"], ["step2"]]);
  });

  test("review fans reviewers out of the implementer and back into the fixer", () => {
    const plan = buildPreset("review", "task", { agents: 3 });
    expect(plan.agents.map((a) => a.id)).toEqual(["implement", "review1", "review2", "fix"]);
    expect(plan.agents[1]!.permissionMode).toBe("plan");
    expect(plan.agents.at(-1)!.dependsOn).toEqual(["review1", "review2"]);
  });

  test("solo ignores the agent count", () => {
    expect(buildPreset("solo", "task", { agents: 5 }).agents).toHaveLength(1);
  });

  test("agent count is clamped to something sane", () => {
    expect(buildPreset("fanout", "t", { agents: 0 }).agents).toHaveLength(2);
    expect(buildPreset("fanout", "t", { agents: 99 }).agents).toHaveLength(13);
  });

  test("every placeholder a preset uses is one the fleet can supply", () => {
    for (const preset of ["fanout", "pipeline", "review"] as const) {
      const plan = buildPreset(preset, "task", { agents: 3 });
      const ids = new Set(plan.agents.map((a) => a.id));
      for (const agent of plan.agents) {
        for (const name of placeholders(agent.prompt)) {
          const [root, key] = name.split(".");
          if (root === "vars") expect(Object.keys(plan.vars ?? {})).toContain(key!);
          else if (root === "outputs") {
            expect(ids.has(key!)).toBe(true);
            expect(agent.dependsOn ?? []).toContain(key!);
          } else throw new Error(`preset used an unexpected placeholder root: ${name}`);
        }
      }
    }
  });
});
