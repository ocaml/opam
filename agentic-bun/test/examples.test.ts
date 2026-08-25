import { describe, expect, test } from "bun:test";
import { Glob } from "bun";
import { join } from "node:path";
import { loadPlan, resolveFleet } from "../src/plan.ts";
import { topoLayers } from "../src/scheduler.ts";
import { placeholders } from "../src/template.ts";

const root = join(import.meta.dir, "..", "examples");
const files = [...new Glob("*.json").scanSync(root)].sort();

describe("shipped examples", () => {
  test("there are examples to check", () => {
    expect(files.length).toBeGreaterThan(0);
  });

  for (const file of files) {
    describe(file, () => {
      test("parses, validates and forms a DAG", async () => {
        const plan = await loadPlan(join(root, file));
        const fleet = resolveFleet(plan);
        expect(fleet.agents.length).toBeGreaterThan(0);
        expect(() =>
          topoLayers(fleet.agents.map((a) => ({ id: a.id, dependsOn: a.dependsOn }))),
        ).not.toThrow();
      });

      test("every placeholder can actually be resolved at run time", async () => {
        const plan = await loadPlan(join(root, file));
        const ids = new Set(plan.agents.map((a) => a.id));
        const keys = new Set(Object.keys(plan.vars ?? {}));
        const outputKeys = new Set(
          plan.agents.map((a) => a.outputKey).filter((k): k is string => Boolean(k)),
        );
        for (const agent of plan.agents) {
          for (const name of placeholders(agent.prompt)) {
            const [root_, key] = name.split(".");
            if (root_ === "vars") {
              expect(keys).toContain(key!);
            } else if (root_ === "outputs") {
              // Referencing an agent's output requires depending on it, or the
              // prompt renders before that agent has run.
              expect(ids).toContain(key!);
              expect(agent.dependsOn ?? []).toContain(key!);
            } else if (root_ === "board") {
              expect([...outputKeys, ...ids]).toContain(key!);
            } else if (root_ !== "env" && root_ !== "agent" && root_ !== "fleet") {
              throw new Error(`${file}: ${agent.id} uses unknown placeholder {{${name}}}`);
            }
          }
        }
      });
    });
  }
});
