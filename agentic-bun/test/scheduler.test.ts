import { describe, expect, test } from "bun:test";
import {
  executeGraph,
  findCycle,
  GraphError,
  topoLayers,
  validateGraph,
  type GraphNode,
} from "../src/scheduler.ts";

const node = (id: string, ...deps: string[]): GraphNode => ({ id, dependsOn: deps });

describe("graph validation", () => {
  test("rejects duplicate ids", () => {
    expect(() => validateGraph([node("a"), node("a")])).toThrow(GraphError);
  });

  test("rejects unknown dependencies", () => {
    expect(() => validateGraph([node("a", "ghost")])).toThrow(/unknown agent "ghost"/);
  });

  test("rejects self-dependency", () => {
    expect(() => validateGraph([node("a", "a")])).toThrow(/depends on itself/);
  });

  test("detects cycles", () => {
    expect(() => validateGraph([node("a", "c"), node("b", "a"), node("c", "b")])).toThrow(
      /dependency cycle/,
    );
    expect(findCycle([node("a", "b"), node("b", "a")])).not.toBeNull();
    expect(findCycle([node("a"), node("b", "a")])).toBeNull();
  });
});

describe("topoLayers", () => {
  test("groups independent nodes together", () => {
    expect(topoLayers([node("a"), node("b"), node("c", "a", "b")])).toEqual([["a", "b"], ["c"]]);
  });

  test("chains produce one node per layer", () => {
    expect(topoLayers([node("a"), node("b", "a"), node("c", "b")])).toEqual([["a"], ["b"], ["c"]]);
  });
});

describe("executeGraph", () => {
  test("runs dependents only after their dependencies", async () => {
    const order: string[] = [];
    const { statuses } = await executeGraph([node("a"), node("b", "a"), node("c", "b")], {
      concurrency: 4,
      run: async (n) => {
        order.push(n.id);
        return true;
      },
    });
    expect(order).toEqual(["a", "b", "c"]);
    expect([...statuses.values()]).toEqual(["succeeded", "succeeded", "succeeded"]);
  });

  test("respects the concurrency limit", async () => {
    let inFlight = 0;
    let peak = 0;
    const nodes = Array.from({ length: 8 }, (_, i) => node(`n${i}`));
    const result = await executeGraph(nodes, {
      concurrency: 3,
      run: async () => {
        inFlight += 1;
        peak = Math.max(peak, inFlight);
        await Bun.sleep(5);
        inFlight -= 1;
        return true;
      },
    });
    expect(peak).toBe(3);
    expect(result.peakParallelism).toBe(3);
  });

  test("runs independent nodes in parallel", async () => {
    let peak = 0;
    let inFlight = 0;
    await executeGraph([node("a"), node("b"), node("c")], {
      concurrency: 3,
      run: async () => {
        inFlight += 1;
        peak = Math.max(peak, inFlight);
        await Bun.sleep(5);
        inFlight -= 1;
        return true;
      },
    });
    expect(peak).toBe(3);
  });

  test("skips the transitive dependents of a failure but keeps other branches", async () => {
    const ran: string[] = [];
    const skipped: string[] = [];
    const { statuses } = await executeGraph(
      [node("a"), node("b", "a"), node("c", "b"), node("d")],
      {
        concurrency: 4,
        run: async (n) => {
          ran.push(n.id);
          return n.id !== "a";
        },
        onSkip: (n) => skipped.push(n.id),
      },
    );
    expect(statuses.get("a")).toBe("failed");
    expect(statuses.get("b")).toBe("skipped");
    expect(statuses.get("c")).toBe("skipped");
    expect(statuses.get("d")).toBe("succeeded");
    expect(ran).not.toContain("b");
    expect(skipped.sort()).toEqual(["b", "c"]);
  });

  test("failFast cancels everything not yet started", async () => {
    const { statuses } = await executeGraph([node("a"), node("b", "a"), node("c")], {
      concurrency: 1,
      failFast: true,
      run: async (n) => n.id !== "a",
    });
    expect(statuses.get("a")).toBe("failed");
    expect(statuses.get("b")).toBe("cancelled");
    expect(statuses.get("c")).toBe("cancelled");
  });

  test("a rejecting run is treated as a failure, not a crash", async () => {
    const { statuses } = await executeGraph([node("a"), node("b", "a")], {
      concurrency: 2,
      run: async () => {
        throw new Error("boom");
      },
    });
    expect(statuses.get("a")).toBe("failed");
    expect(statuses.get("b")).toBe("skipped");
  });

  test("an already-aborted signal cancels the whole graph", async () => {
    const controller = new AbortController();
    controller.abort();
    let ran = false;
    const { statuses } = await executeGraph([node("a")], {
      concurrency: 1,
      signal: controller.signal,
      run: async () => {
        ran = true;
        return true;
      },
    });
    expect(ran).toBe(false);
    expect(statuses.get("a")).toBe("cancelled");
  });

  test("aborting mid-run cancels the nodes that have not started", async () => {
    const controller = new AbortController();
    const started: string[] = [];
    const { statuses } = await executeGraph([node("a"), node("b"), node("c")], {
      concurrency: 1,
      signal: controller.signal,
      run: async (n) => {
        started.push(n.id);
        controller.abort();
        return true;
      },
    });
    expect(started).toEqual(["a"]);
    expect(statuses.get("a")).toBe("succeeded");
    expect(statuses.get("b")).toBe("cancelled");
    expect(statuses.get("c")).toBe("cancelled");
  });
});
