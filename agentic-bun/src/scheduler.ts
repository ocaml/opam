/**
 * DAG scheduling. Deliberately generic and free of any agent/process concepts
 * so it can be tested on its own: give it nodes with `dependsOn` and a `run`
 * function, it gives you back what happened.
 */

export interface GraphNode {
  id: string;
  dependsOn: string[];
}

export type NodeStatus = "succeeded" | "failed" | "skipped" | "cancelled";

export class GraphError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "GraphError";
  }
}

/** Reject duplicate ids, dangling dependencies and cycles. */
export function validateGraph(nodes: GraphNode[]): void {
  const seen = new Set<string>();
  for (const node of nodes) {
    if (!node.id || node.id.trim().length === 0) throw new GraphError("node with empty id");
    if (seen.has(node.id)) throw new GraphError(`duplicate agent id: ${node.id}`);
    seen.add(node.id);
  }
  for (const node of nodes) {
    for (const dep of node.dependsOn) {
      if (!seen.has(dep)) {
        throw new GraphError(`agent "${node.id}" depends on unknown agent "${dep}"`);
      }
      if (dep === node.id) throw new GraphError(`agent "${node.id}" depends on itself`);
    }
  }
  const cycle = findCycle(nodes);
  if (cycle) throw new GraphError(`dependency cycle: ${cycle.join(" -> ")}`);
}

/** Return one cycle as a path, or null when the graph is acyclic. */
export function findCycle(nodes: GraphNode[]): string[] | null {
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const state = new Map<string, "visiting" | "done">();
  const path: string[] = [];

  const visit = (id: string): string[] | null => {
    const current = state.get(id);
    if (current === "done") return null;
    if (current === "visiting") {
      const start = path.indexOf(id);
      return [...path.slice(start >= 0 ? start : 0), id];
    }
    state.set(id, "visiting");
    path.push(id);
    for (const dep of byId.get(id)?.dependsOn ?? []) {
      const found = visit(dep);
      if (found) return found;
    }
    path.pop();
    state.set(id, "done");
    return null;
  };

  for (const node of nodes) {
    const found = visit(node.id);
    if (found) return found;
  }
  return null;
}

/**
 * Group nodes into dependency layers. Everything in layer N can run in
 * parallel once layer N-1 is done. Used for `--dry-run` and plan printing;
 * the executor itself is finer-grained than this.
 */
export function topoLayers(nodes: GraphNode[]): string[][] {
  validateGraph(nodes);
  const remaining = new Map(nodes.map((n) => [n.id, new Set(n.dependsOn)]));
  const layers: string[][] = [];
  while (remaining.size > 0) {
    const ready = [...remaining.entries()]
      .filter(([, deps]) => deps.size === 0)
      .map(([id]) => id);
    if (ready.length === 0) throw new GraphError("dependency cycle detected while layering");
    layers.push(ready);
    for (const id of ready) remaining.delete(id);
    for (const deps of remaining.values()) {
      for (const id of ready) deps.delete(id);
    }
  }
  return layers;
}

export interface ExecuteOptions<N extends GraphNode> {
  /** Max nodes in flight. Values < 1 are treated as 1. */
  concurrency: number;
  /** Cancel everything not yet started when a node fails. */
  failFast?: boolean;
  /** Run one node. Resolve `true` for success, `false` for failure. Never rejects. */
  run: (node: N) => Promise<boolean>;
  /** Called when a node will not run at all. `status` distinguishes a skip
   * (an upstream dependency failed) from a cancellation (fail-fast or abort). */
  onSkip?: (node: N, reason: string, status: "skipped" | "cancelled") => void;
  /** External cancellation (e.g. SIGINT). */
  signal?: AbortSignal;
}

export interface ExecuteResult {
  statuses: Map<string, NodeStatus>;
  /** Ids in the order they were started. */
  startOrder: string[];
  /** Highest number of nodes in flight at once. */
  peakParallelism: number;
}

/**
 * Run the graph. Nodes whose dependencies failed (transitively) are skipped;
 * independent branches keep going unless `failFast` is set.
 */
export async function executeGraph<N extends GraphNode>(
  nodes: N[],
  opts: ExecuteOptions<N>,
): Promise<ExecuteResult> {
  validateGraph(nodes);

  const limit = Math.max(1, Math.floor(opts.concurrency) || 1);
  const byId = new Map(nodes.map((n) => [n.id, n]));
  const statuses = new Map<string, NodeStatus>();
  const pending = new Set(nodes.map((n) => n.id));
  const startOrder: string[] = [];
  const active = new Map<string, Promise<void>>();
  let peakParallelism = 0;
  let aborted = false;

  const settle = (id: string, status: NodeStatus): void => {
    statuses.set(id, status);
    pending.delete(id);
  };

  /** Skip a node and, transitively, everything downstream of it. */
  const cascadeSkip = (id: string, reason: string): void => {
    for (const node of nodes) {
      if (!pending.has(node.id)) continue;
      if (!node.dependsOn.includes(id)) continue;
      settle(node.id, "skipped");
      opts.onSkip?.(node, reason, "skipped");
      cascadeSkip(node.id, `dependency "${id}" did not succeed`);
    }
  };

  const readyNodes = (): N[] => {
    const ready: N[] = [];
    for (const id of pending) {
      if (active.has(id)) continue;
      const node = byId.get(id);
      if (!node) continue;
      if (node.dependsOn.every((dep) => statuses.get(dep) === "succeeded")) ready.push(node);
    }
    // Keep plan order stable so logs and artifacts are reproducible.
    return ready.sort((a, b) => nodes.indexOf(a) - nodes.indexOf(b));
  };

  const cancelRest = (reason: string): void => {
    aborted = true;
    for (const id of [...pending]) {
      if (active.has(id)) continue;
      settle(id, "cancelled");
      const node = byId.get(id);
      if (node) opts.onSkip?.(node, reason, "cancelled");
    }
  };

  if (opts.signal?.aborted) {
    cancelRest("cancelled before start");
    return { statuses, startOrder, peakParallelism };
  }
  const onAbort = () => cancelRest("cancelled");
  opts.signal?.addEventListener("abort", onAbort, { once: true });

  try {
    while (pending.size > 0) {
      if (!aborted) {
        for (const node of readyNodes()) {
          if (active.size >= limit) break;
          startOrder.push(node.id);
          const task = opts
            .run(node)
            .then((ok) => {
              settle(node.id, ok ? "succeeded" : "failed");
              if (!ok) {
                if (opts.failFast) cancelRest(`fail-fast: "${node.id}" failed`);
                else cascadeSkip(node.id, `dependency "${node.id}" failed`);
              }
            })
            .catch(() => {
              // `run` is documented not to reject; treat a rejection as failure.
              settle(node.id, "failed");
              if (opts.failFast) cancelRest(`fail-fast: "${node.id}" threw`);
              else cascadeSkip(node.id, `dependency "${node.id}" failed`);
            })
            .finally(() => {
              active.delete(node.id);
            });
          active.set(node.id, task);
          peakParallelism = Math.max(peakParallelism, active.size);
        }
      }

      if (active.size === 0) {
        // Nothing running and nothing startable: the rest is unreachable.
        if (pending.size > 0) {
          for (const id of [...pending]) {
            const status = aborted ? "cancelled" : "skipped";
            settle(id, status);
            const node = byId.get(id);
            if (node) opts.onSkip?.(node, aborted ? "cancelled" : "dependencies unmet", status);
          }
        }
        break;
      }

      await Promise.race(active.values());
    }
  } finally {
    opts.signal?.removeEventListener("abort", onAbort);
  }

  await Promise.allSettled(active.values());
  return { statuses, startOrder, peakParallelism };
}
