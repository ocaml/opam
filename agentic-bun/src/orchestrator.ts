/**
 * The Fleet: everything that happens between "here is a plan" and "here are
 * the results". Read this file top to bottom to understand a whole run.
 *
 *   preflight (ollama reachable? model installed? warm it)
 *        │
 *        ▼
 *   resolve launch mode (ollama launch claude  vs  direct claude)
 *        │
 *        ▼
 *   executeGraph  ── for each ready agent, up to `concurrency`:
 *        │              render prompt from vars + upstream outputs
 *        │              spawn claude, stream its NDJSON, retry on failure
 *        │              publish final text to the blackboard
 *        ▼
 *   artifacts (per-agent transcript + summary.json + summary.md)
 */

import { mkdir } from "node:fs/promises";
import { join } from "node:path";
import { runAgent, type ExecFn } from "./agent.ts";
import { Blackboard } from "./blackboard.ts";
import { EventBus } from "./events.ts";
import { buildLaunchPlan, resolveMode } from "./launcher.ts";
import { Logger } from "./logger.ts";
import {
  looksToolCapable,
  probe as probeOllama,
  probeLaunchSupport,
  resolveModel,
  warm as warmModel,
  type OllamaProbe,
} from "./ollama.ts";
import { render, TemplateError } from "./template.ts";
import type {
  AgentResult,
  FleetRunResult,
  LaunchPlan,
  ResolvedAgent,
  ResolvedFleet,
} from "./types.ts";
import { executeGraph, topoLayers } from "./scheduler.ts";

export interface FleetOptions {
  log?: Logger;
  bus?: EventBus;
  /** Process runner. Swapped out in tests. */
  exec?: ExecFn;
  signal?: AbortSignal;
  /** Retry backoff base. Set to 0 in tests. */
  backoffMs?: number;
  /** Print assistant text as it streams. Default true. */
  streamText?: boolean;
  /** Injected probes, so a run can be exercised without Ollama installed. */
  probes?: {
    launchSupport?: () => Promise<boolean>;
    ollama?: (host: string) => Promise<OllamaProbe>;
    warm?: (model: string, host: string, keepAlive: string) => Promise<boolean>;
  };
}

export interface PreflightReport {
  mode: "launch" | "direct";
  model: string;
  probe: OllamaProbe | null;
  warmed: boolean;
  warnings: string[];
}

/** "1 turn" / "2 turns". */
function plural(count: number, noun: string): string {
  return `${count} ${noun}${count === 1 ? "" : "s"}`;
}

/** Deterministic-ish run id: sortable timestamp plus the fleet name. */
function makeRunId(name: string): string {
  const stamp = new Date().toISOString().replace(/[:.]/g, "-").replace("T", "_").slice(0, 19);
  const slug = name.replace(/[^A-Za-z0-9_-]+/g, "-").replace(/^-|-$/g, "") || "fleet";
  return `${stamp}-${slug}`;
}

export class Fleet {
  readonly board = new Blackboard();
  readonly bus: EventBus;
  readonly log: Logger;
  private readonly results = new Map<string, AgentResult>();

  constructor(
    readonly spec: ResolvedFleet,
    private readonly opts: FleetOptions = {},
  ) {
    this.bus = opts.bus ?? new EventBus();
    this.log = opts.log ?? new Logger();
  }

  /** Check the environment and decide how agents will be launched. */
  async preflight(): Promise<PreflightReport> {
    const warnings: string[] = [];
    const launchProbe = this.opts.probes?.launchSupport ?? probeLaunchSupport;
    const ollamaProbe = this.opts.probes?.ollama ?? probeOllama;
    const warm = this.opts.probes?.warm ?? warmModel;

    let mode: "launch" | "direct";
    if (this.spec.launchMode === "auto") {
      const available = await launchProbe();
      mode = resolveMode("auto", available);
      if (!available) {
        warnings.push(
          "`ollama launch claude` is unavailable — falling back to launching `claude` directly against " +
            `${this.spec.ollamaHost}/v1 (needs Ollama v0.15+ for the launch subcommand).`,
        );
      }
    } else {
      mode = resolveMode(this.spec.launchMode, true);
    }

    let probeResult: OllamaProbe | null = null;
    let model = this.spec.model;
    let warmed = false;

    if (this.spec.skipPreflight) {
      if (!model) warnings.push("no model configured and preflight skipped");
      return { mode, model, probe: null, warmed, warnings };
    }

    probeResult = await ollamaProbe(this.spec.ollamaHost);
    if (!probeResult.reachable) {
      warnings.push(
        `Ollama is not reachable at ${this.spec.ollamaHost} (${probeResult.error ?? "no response"}). ` +
          "Start it with `ollama serve`.",
      );
    }

    model = resolveModel(model || undefined, probeResult.models);
    if (!model) {
      warnings.push("no model available — pull one, e.g. `ollama pull qwen2.5-coder`");
    } else {
      if (probeResult.reachable && !probeResult.models.includes(model)) {
        warnings.push(`model "${model}" is not installed (\`ollama pull ${model}\`)`);
      }
      if (!looksToolCapable(model)) {
        warnings.push(
          `model "${model}" is not on the known tool-calling list — Claude Code needs tool calls to do anything`,
        );
      }
      if (probeResult.reachable && !this.spec.skipWarm) {
        warmed = await warm(model, this.spec.ollamaHost, this.spec.keepAlive);
        if (!warmed) warnings.push(`could not warm "${model}" (the first agent will be slower)`);
      }
    }

    // Propagate the resolved model to agents that did not pin their own.
    for (const agent of this.spec.agents) {
      if (!agent.model) agent.model = model;
    }
    this.spec.model = model;

    return { mode, model, probe: probeResult, warmed, warnings };
  }

  /** Context handed to the prompt templater for one agent. */
  private templateContext(agent: ResolvedAgent): Record<string, unknown> {
    const outputs: Record<string, string> = {};
    for (const [id, result] of this.results) outputs[id] = result.text;
    return {
      vars: this.spec.vars,
      outputs,
      board: this.board.snapshot(),
      env: process.env,
      agent: { id: agent.id, role: agent.role, model: agent.model },
      fleet: { name: this.spec.name, model: this.spec.model, cwd: this.spec.cwd },
    };
  }

  /**
   * Resolve every agent's command without running anything, in dependency
   * layers. Prompts that depend on upstream output cannot be rendered yet, so
   * their placeholders are left visible.
   */
  async dryRun(): Promise<{ layers: string[][]; plans: { id: string; plan: LaunchPlan }[] }> {
    const { mode } = await this.preflight();
    const layers = topoLayers(this.spec.agents.map((a) => ({ id: a.id, dependsOn: a.dependsOn })));
    const plans = this.spec.agents.map((agent) => {
      const ctx = this.templateContext(agent);
      let prompt = agent.prompt;
      try {
        prompt = render(agent.prompt, ctx);
      } catch {
        // Upstream outputs do not exist yet — that is expected in a dry run.
      }
      return {
        id: agent.id,
        plan: buildLaunchPlan({ ...agent, prompt }, { mode, fleet: this.spec }),
      };
    });
    return { layers, plans };
  }

  async run(): Promise<FleetRunResult> {
    const startedAt = Date.now();
    const runId = makeRunId(this.spec.name);
    const artifactsDir = join(this.spec.artifactsDir, runId);
    const transcripts = new Map<string, string[]>();

    this.log.step(
      `fleet "${this.spec.name}" — ${this.spec.agents.length} agents, concurrency ${this.spec.concurrency}`,
    );

    const pre = await this.preflight();
    for (const warning of pre.warnings) this.log.warn(warning);
    this.log.info(
      `mode: ${pre.mode} · model: ${pre.model || "(unset)"} · cwd: ${this.spec.cwd}` +
        (pre.warmed ? " · warmed" : ""),
    );

    if (!this.spec.noArtifacts) {
      await mkdir(artifactsDir, { recursive: true });
      this.log.info(`artifacts: ${artifactsDir}`);
    }

    this.bus.emit("fleet:start", {
      runId,
      name: this.spec.name,
      agents: this.spec.agents.map((a) => a.id),
    });

    const nodes = this.spec.agents.map((agent) => ({
      id: agent.id,
      dependsOn: agent.dependsOn,
      agent,
    }));

    const execution = await executeGraph(nodes, {
      concurrency: this.spec.concurrency,
      failFast: this.spec.failFast,
      ...(this.opts.signal ? { signal: this.opts.signal } : {}),
      onSkip: (node, reason, status) => {
        this.results.set(node.id, this.blankResult(node.agent, status, reason));
        this.bus.emit("agent:skipped", { id: node.id, reason });
        this.log.agent(node.id, `${status} — ${reason}`, "meta");
      },
      run: async (node) => {
        const agent = node.agent;

        // Render the prompt now: upstream outputs only exist at this point.
        let prompt: string;
        try {
          prompt = render(agent.prompt, this.templateContext(agent));
        } catch (error) {
          const message =
            error instanceof TemplateError ? error.message : String((error as Error).message);
          this.log.error(`${agent.id}: ${message}`);
          this.results.set(agent.id, this.blankResult(agent, "failed", message));
          return false;
        }

        this.bus.emit("agent:queued", { id: agent.id, role: agent.role });

        const result = await runAgent(
          { ...agent, prompt },
          {
            fleet: this.spec,
            mode: pre.mode,
            bus: this.bus,
            log: this.log,
            ...(this.opts.exec ? { exec: this.opts.exec } : {}),
            ...(this.opts.signal ? { signal: this.opts.signal } : {}),
            ...(this.opts.backoffMs !== undefined ? { backoffMs: this.opts.backoffMs } : {}),
            ...(this.opts.streamText !== undefined ? { streamText: this.opts.streamText } : {}),
            sink: this.spec.noArtifacts
              ? undefined
              : (id, line) => {
                  const lines = transcripts.get(id) ?? [];
                  lines.push(line);
                  transcripts.set(id, lines);
                },
          },
        );

        this.results.set(agent.id, result);
        if (result.status === "succeeded") {
          this.board.set(agent.id, result.text);
          if (agent.outputKey) this.board.set(agent.outputKey, result.text);
          this.log.agent(
            agent.id,
            `done in ${(result.durationMs / 1000).toFixed(1)}s · ${plural(result.toolCalls.length, "tool call")} · ${plural(result.numTurns, "turn")}`,
            "meta",
          );
        } else {
          this.log.error(`${agent.id}: ${result.status} — ${result.error ?? "no detail"}`);
        }
        return result.status === "succeeded";
      },
    });

    // Any node the scheduler settled without our `run`/`onSkip` seeing it.
    for (const [id, status] of execution.statuses) {
      if (this.results.has(id)) continue;
      const agent = this.spec.agents.find((a) => a.id === id);
      if (agent) this.results.set(id, this.blankResult(agent, status, status));
    }

    const endedAt = Date.now();
    const results = this.spec.agents
      .map((a) => this.results.get(a.id))
      .filter((r): r is AgentResult => Boolean(r));

    const runResult: FleetRunResult = {
      runId,
      name: this.spec.name,
      startedAt,
      endedAt,
      durationMs: endedAt - startedAt,
      ok: results.every((r) => r.status === "succeeded"),
      artifactsDir: this.spec.noArtifacts ? "" : artifactsDir,
      results,
      board: this.board.snapshot(),
    };

    if (!this.spec.noArtifacts) {
      await this.writeArtifacts(artifactsDir, runResult, transcripts, pre);
    }

    this.bus.emit("fleet:end", { runId, ok: runResult.ok, durationMs: runResult.durationMs });
    this.printSummary(runResult, execution.peakParallelism);
    return runResult;
  }

  private blankResult(agent: ResolvedAgent, status: AgentResult["status"], error: string): AgentResult {
    const now = Date.now();
    return {
      id: agent.id,
      role: agent.role,
      status,
      attempts: 0,
      text: "",
      toolCalls: [],
      startedAt: now,
      endedAt: now,
      durationMs: 0,
      exitCode: null,
      numTurns: 0,
      sessionId: "",
      costUsd: 0,
      events: 0,
      stderr: "",
      error,
    };
  }

  private async writeArtifacts(
    dir: string,
    run: FleetRunResult,
    transcripts: Map<string, string[]>,
    pre: PreflightReport,
  ): Promise<void> {
    const writes: Promise<unknown>[] = [];
    for (const [id, lines] of transcripts) {
      writes.push(Bun.write(join(dir, `${id}.jsonl`), lines.join("\n") + "\n"));
    }
    for (const result of run.results) {
      if (!result.text) continue;
      writes.push(Bun.write(join(dir, `${result.id}.md`), result.text + "\n"));
    }
    writes.push(
      Bun.write(
        join(dir, "summary.json"),
        JSON.stringify(
          {
            ...run,
            preflight: { mode: pre.mode, model: pre.model, warnings: pre.warnings },
            fleet: { ...this.spec, agents: this.spec.agents.map((a) => ({ ...a, prompt: a.prompt })) },
          },
          null,
          2,
        ) + "\n",
      ),
    );
    writes.push(Bun.write(join(dir, "summary.md"), this.markdownSummary(run)));
    await Promise.all(writes);
  }

  private markdownSummary(run: FleetRunResult): string {
    const lines = [
      `# ${run.name}`,
      "",
      `- run: \`${run.runId}\``,
      `- duration: ${(run.durationMs / 1000).toFixed(1)}s`,
      `- result: ${run.ok ? "ok" : "failed"}`,
      "",
      "| agent | role | status | turns | tools | duration |",
      "| --- | --- | --- | --- | --- | --- |",
    ];
    for (const r of run.results) {
      lines.push(
        `| ${r.id} | ${r.role} | ${r.status} | ${r.numTurns} | ${r.toolCalls.length} | ${(r.durationMs / 1000).toFixed(1)}s |`,
      );
    }
    for (const r of run.results) {
      lines.push("", `## ${r.id}`, "");
      if (r.error) lines.push(`> error: ${r.error}`, "");
      lines.push(r.text || "_no output_");
    }
    return lines.join("\n") + "\n";
  }

  private printSummary(run: FleetRunResult, peak: number): void {
    const ok = run.results.filter((r) => r.status === "succeeded").length;
    const line = `${ok}/${run.results.length} agents succeeded in ${(run.durationMs / 1000).toFixed(1)}s (peak parallelism ${peak})`;
    if (run.ok) this.log.success(line);
    else this.log.error(line);
    for (const r of run.results) {
      if (r.status === "succeeded") continue;
      this.log.warn(`  ${r.id}: ${r.status}${r.error ? ` — ${r.error}` : ""}`);
    }
  }
}

/** Convenience wrapper: resolve nothing, just run an already-resolved fleet. */
export async function runFleet(
  spec: ResolvedFleet,
  opts: FleetOptions = {},
): Promise<FleetRunResult> {
  return new Fleet(spec, opts).run();
}
