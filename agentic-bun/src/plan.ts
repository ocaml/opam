/**
 * Plan files: loading, validation, default resolution and a few presets.
 *
 * A plan is plain JSON (comments allowed). Everything the framework needs to
 * run a fleet lives in one file, so a run is reproducible and reviewable.
 */

import { isAbsolute, resolve } from "node:path";
import { DEFAULT_LAUNCH_COMMAND } from "./launcher.ts";
import { DEFAULT_HOST } from "./ollama.ts";
import type {
  AgentSpec,
  FleetPlan,
  LaunchMode,
  PermissionMode,
  PromptDelivery,
  ResolvedAgent,
  ResolvedFleet,
} from "./types.ts";

export class PlanError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "PlanError";
  }
}

export const DEFAULTS = {
  concurrency: 3,
  maxTurns: 24,
  timeoutMs: 10 * 60 * 1000,
  retries: 1,
  /**
   * `acceptEdits` rather than `default`: a `--print` agent cannot answer a
   * permission prompt, so `default` makes every write silently fail. Use
   * `bypassPermissions` only when you have read the plan you are running.
   */
  permissionMode: "acceptEdits" as PermissionMode,
  promptVia: "stdin" as PromptDelivery,
  launchMode: "auto" as LaunchMode,
  keepAlive: "30m",
} as const;

const ID_PATTERN = /^[A-Za-z0-9][A-Za-z0-9_.-]*$/;
const PERMISSION_MODES: PermissionMode[] = [
  "default",
  "acceptEdits",
  "bypassPermissions",
  "plan",
];
const LAUNCH_MODES: LaunchMode[] = ["auto", "launch", "direct"];

/** Strip `//` and `/* *\/` comments that sit outside string literals. */
export function stripJsonComments(text: string): string {
  let out = "";
  let inString = false;
  let inLine = false;
  let inBlock = false;
  for (let i = 0; i < text.length; i++) {
    const ch = text[i]!;
    const next = text[i + 1];
    if (inLine) {
      if (ch === "\n") {
        inLine = false;
        out += ch;
      }
      continue;
    }
    if (inBlock) {
      if (ch === "*" && next === "/") {
        inBlock = false;
        i++;
      }
      continue;
    }
    if (inString) {
      out += ch;
      if (ch === "\\") {
        const following = text[i + 1];
        if (following !== undefined) {
          out += following;
          i++;
        }
      } else if (ch === '"') {
        inString = false;
      }
      continue;
    }
    if (ch === '"') {
      inString = true;
      out += ch;
      continue;
    }
    if (ch === "/" && next === "/") {
      inLine = true;
      i++;
      continue;
    }
    if (ch === "/" && next === "*") {
      inBlock = true;
      i++;
      continue;
    }
    out += ch;
  }
  return out;
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

function stringArray(value: unknown, where: string): string[] {
  if (value === undefined) return [];
  if (!Array.isArray(value) || value.some((v) => typeof v !== "string")) {
    throw new PlanError(`${where} must be an array of strings`);
  }
  return value as string[];
}

function stringMap(value: unknown, where: string): Record<string, string> {
  if (value === undefined) return {};
  if (!isRecord(value) || Object.values(value).some((v) => typeof v !== "string")) {
    throw new PlanError(`${where} must be an object of string values`);
  }
  return value as Record<string, string>;
}

function positive(value: unknown, where: string): number | undefined {
  if (value === undefined) return undefined;
  if (typeof value !== "number" || !Number.isFinite(value) || value < 0) {
    throw new PlanError(`${where} must be a non-negative number`);
  }
  return value;
}

/** Validate an arbitrary parsed object as a {@link FleetPlan}. */
export function validatePlan(input: unknown): FleetPlan {
  if (!isRecord(input)) throw new PlanError("plan must be a JSON object");
  if (input.version !== undefined && input.version !== 1) {
    throw new PlanError(`unsupported plan version: ${String(input.version)} (expected 1)`);
  }
  if (!Array.isArray(input.agents) || input.agents.length === 0) {
    throw new PlanError("plan must define a non-empty `agents` array");
  }
  if (
    input.permissionMode !== undefined &&
    !PERMISSION_MODES.includes(input.permissionMode as PermissionMode)
  ) {
    throw new PlanError(`permissionMode must be one of: ${PERMISSION_MODES.join(", ")}`);
  }
  if (input.launchMode !== undefined && !LAUNCH_MODES.includes(input.launchMode as LaunchMode)) {
    throw new PlanError(`launchMode must be one of: ${LAUNCH_MODES.join(", ")}`);
  }
  if (input.promptVia !== undefined && input.promptVia !== "stdin" && input.promptVia !== "arg") {
    throw new PlanError('promptVia must be "stdin" or "arg"');
  }

  const seen = new Set<string>();
  const agents: AgentSpec[] = input.agents.map((raw, index) => {
    if (!isRecord(raw)) throw new PlanError(`agents[${index}] must be an object`);
    const id = raw.id;
    if (typeof id !== "string" || !ID_PATTERN.test(id)) {
      throw new PlanError(
        `agents[${index}].id must match ${ID_PATTERN.source} (got ${JSON.stringify(id)})`,
      );
    }
    if (seen.has(id)) throw new PlanError(`duplicate agent id: ${id}`);
    seen.add(id);
    if (typeof raw.prompt !== "string" || raw.prompt.trim().length === 0) {
      throw new PlanError(`agents[${index}] ("${id}") needs a non-empty prompt`);
    }
    if (
      raw.permissionMode !== undefined &&
      !PERMISSION_MODES.includes(raw.permissionMode as PermissionMode)
    ) {
      throw new PlanError(`agents[${index}] ("${id}").permissionMode is invalid`);
    }

    const spec: AgentSpec = { id, prompt: raw.prompt };
    if (typeof raw.role === "string") spec.role = raw.role;
    if (typeof raw.model === "string") spec.model = raw.model;
    if (typeof raw.systemPrompt === "string") spec.systemPrompt = raw.systemPrompt;
    if (typeof raw.cwd === "string") spec.cwd = raw.cwd;
    if (typeof raw.outputKey === "string") spec.outputKey = raw.outputKey;
    if (raw.permissionMode !== undefined) spec.permissionMode = raw.permissionMode as PermissionMode;
    spec.addDirs = stringArray(raw.addDirs, `agents[${index}].addDirs`);
    spec.allowedTools = stringArray(raw.allowedTools, `agents[${index}].allowedTools`);
    spec.disallowedTools = stringArray(raw.disallowedTools, `agents[${index}].disallowedTools`);
    spec.dependsOn = stringArray(raw.dependsOn, `agents[${index}].dependsOn`);
    spec.env = stringMap(raw.env, `agents[${index}].env`);
    const maxTurns = positive(raw.maxTurns, `agents[${index}].maxTurns`);
    if (maxTurns !== undefined) spec.maxTurns = maxTurns;
    const timeoutMs = positive(raw.timeoutMs, `agents[${index}].timeoutMs`);
    if (timeoutMs !== undefined) spec.timeoutMs = timeoutMs;
    const retries = positive(raw.retries, `agents[${index}].retries`);
    if (retries !== undefined) spec.retries = retries;
    return spec;
  });

  for (const agent of agents) {
    for (const dep of agent.dependsOn ?? []) {
      if (!seen.has(dep)) {
        throw new PlanError(`agent "${agent.id}" depends on unknown agent "${dep}"`);
      }
    }
  }

  const plan: FleetPlan = { agents };
  if (typeof input.name === "string") plan.name = input.name;
  if (typeof input.description === "string") plan.description = input.description;
  if (typeof input.model === "string") plan.model = input.model;
  if (typeof input.ollamaHost === "string") plan.ollamaHost = input.ollamaHost;
  if (input.launchMode !== undefined) plan.launchMode = input.launchMode as LaunchMode;
  if (input.launchCommand !== undefined)
    plan.launchCommand = stringArray(input.launchCommand, "launchCommand");
  if (input.promptVia !== undefined) plan.promptVia = input.promptVia as PromptDelivery;
  if (typeof input.cwd === "string") plan.cwd = input.cwd;
  if (typeof input.failFast === "boolean") plan.failFast = input.failFast;
  if (typeof input.skipPreflight === "boolean") plan.skipPreflight = input.skipPreflight;
  if (typeof input.skipWarm === "boolean") plan.skipWarm = input.skipWarm;
  if (typeof input.keepAlive === "string") plan.keepAlive = input.keepAlive;
  if (typeof input.artifactsDir === "string") plan.artifactsDir = input.artifactsDir;
  if (typeof input.noArtifacts === "boolean") plan.noArtifacts = input.noArtifacts;
  if (typeof input.systemPrompt === "string") plan.systemPrompt = input.systemPrompt;
  if (input.permissionMode !== undefined) plan.permissionMode = input.permissionMode as PermissionMode;
  const concurrency = positive(input.concurrency, "concurrency");
  if (concurrency !== undefined) plan.concurrency = concurrency;
  const maxTurns = positive(input.maxTurns, "maxTurns");
  if (maxTurns !== undefined) plan.maxTurns = maxTurns;
  const timeoutMs = positive(input.timeoutMs, "timeoutMs");
  if (timeoutMs !== undefined) plan.timeoutMs = timeoutMs;
  const retries = positive(input.retries, "retries");
  if (retries !== undefined) plan.retries = retries;
  plan.allowedTools = stringArray(input.allowedTools, "allowedTools");
  plan.disallowedTools = stringArray(input.disallowedTools, "disallowedTools");
  plan.vars = stringMap(input.vars, "vars");
  plan.env = stringMap(input.env, "env");
  return plan;
}

/** Parse plan text (JSON with comments). */
export function parsePlan(text: string, source = "<plan>"): FleetPlan {
  let parsed: unknown;
  try {
    parsed = JSON.parse(stripJsonComments(text));
  } catch (error) {
    throw new PlanError(
      `${source}: not valid JSON — ${error instanceof Error ? error.message : String(error)}`,
    );
  }
  return validatePlan(parsed);
}

export async function loadPlan(path: string): Promise<FleetPlan> {
  const file = Bun.file(path);
  if (!(await file.exists())) throw new PlanError(`plan file not found: ${path}`);
  return parsePlan(await file.text(), path);
}

/** CLI/API overrides applied on top of a plan file. */
export interface FleetOverrides {
  model?: string;
  ollamaHost?: string;
  launchMode?: LaunchMode;
  cwd?: string;
  concurrency?: number;
  maxTurns?: number;
  timeoutMs?: number;
  retries?: number;
  permissionMode?: PermissionMode;
  failFast?: boolean;
  skipPreflight?: boolean;
  skipWarm?: boolean;
  artifactsDir?: string;
  noArtifacts?: boolean;
  vars?: Record<string, string>;
  allowedTools?: string[];
  disallowedTools?: string[];
}

/** Fold plan values, overrides and built-in defaults into a complete fleet. */
export function resolveFleet(plan: FleetPlan, overrides: FleetOverrides = {}): ResolvedFleet {
  const cwd = resolve(overrides.cwd ?? plan.cwd ?? process.cwd());
  const artifactsRoot = overrides.artifactsDir ?? plan.artifactsDir ?? ".agentic/runs";

  const fleet: ResolvedFleet = {
    version: 1,
    name: plan.name ?? "fleet",
    description: plan.description ?? "",
    model: overrides.model ?? plan.model ?? "",
    ollamaHost: (overrides.ollamaHost ?? plan.ollamaHost ?? DEFAULT_HOST).replace(/\/+$/, ""),
    launchMode: overrides.launchMode ?? plan.launchMode ?? DEFAULTS.launchMode,
    launchCommand:
      plan.launchCommand && plan.launchCommand.length > 0
        ? plan.launchCommand
        : [...DEFAULT_LAUNCH_COMMAND],
    promptVia: plan.promptVia ?? DEFAULTS.promptVia,
    cwd,
    concurrency: overrides.concurrency ?? plan.concurrency ?? DEFAULTS.concurrency,
    failFast: overrides.failFast ?? plan.failFast ?? false,
    skipPreflight: overrides.skipPreflight ?? plan.skipPreflight ?? false,
    skipWarm: overrides.skipWarm ?? plan.skipWarm ?? false,
    keepAlive: plan.keepAlive ?? DEFAULTS.keepAlive,
    artifactsDir: isAbsolute(artifactsRoot) ? artifactsRoot : resolve(cwd, artifactsRoot),
    noArtifacts: overrides.noArtifacts ?? plan.noArtifacts ?? false,
    vars: { ...plan.vars, ...overrides.vars },
    env: { ...plan.env },
    agents: [],
  };

  const permissionMode =
    overrides.permissionMode ?? plan.permissionMode ?? DEFAULTS.permissionMode;
  const maxTurns = overrides.maxTurns ?? plan.maxTurns ?? DEFAULTS.maxTurns;
  const timeoutMs = overrides.timeoutMs ?? plan.timeoutMs ?? DEFAULTS.timeoutMs;
  const retries = overrides.retries ?? plan.retries ?? DEFAULTS.retries;
  const allowedTools = overrides.allowedTools ?? plan.allowedTools ?? [];
  const disallowedTools = overrides.disallowedTools ?? plan.disallowedTools ?? [];

  fleet.agents = plan.agents.map<ResolvedAgent>((spec) => {
    const agentCwd = spec.cwd ? (isAbsolute(spec.cwd) ? spec.cwd : resolve(cwd, spec.cwd)) : cwd;
    return {
      id: spec.id,
      role: spec.role ?? spec.id,
      prompt: spec.prompt,
      model: spec.model ?? fleet.model,
      systemPrompt: spec.systemPrompt ?? plan.systemPrompt ?? "",
      cwd: agentCwd,
      addDirs: (spec.addDirs ?? []).map((d) => (isAbsolute(d) ? d : resolve(agentCwd, d))),
      allowedTools: spec.allowedTools?.length ? spec.allowedTools : allowedTools,
      disallowedTools: spec.disallowedTools?.length ? spec.disallowedTools : disallowedTools,
      permissionMode: spec.permissionMode ?? permissionMode,
      maxTurns: spec.maxTurns ?? maxTurns,
      timeoutMs: spec.timeoutMs ?? timeoutMs,
      retries: spec.retries ?? retries,
      dependsOn: spec.dependsOn ?? [],
      outputKey: spec.outputKey ?? "",
      env: spec.env ?? {},
    };
  });

  return fleet;
}

// ---------------------------------------------------------------------------
// Presets — used by `agentic run -p "..."` and the wizard.
// ---------------------------------------------------------------------------

export type PresetName = "fanout" | "pipeline" | "review" | "solo";

export const PRESETS: Record<PresetName, string> = {
  solo: "One agent does the whole task.",
  fanout: "N workers attack the task in parallel, then one synthesiser merges them.",
  pipeline: "N agents in a chain: each one builds on the previous agent's output.",
  review: "One implementer, then parallel reviewers, then one agent applies the fixes.",
};

export interface PresetOptions {
  agents?: number;
  model?: string;
  cwd?: string;
  name?: string;
}

const FANOUT_ANGLES = [
  "correctness and edge cases",
  "structure, naming and readability",
  "performance and resource use",
  "tests and verifiability",
  "security and failure modes",
  "documentation and developer experience",
];

/** Build a ready-to-run plan for a single free-text task. */
export function buildPreset(
  preset: PresetName,
  task: string,
  opts: PresetOptions = {},
): FleetPlan {
  const count = Math.max(1, Math.min(opts.agents ?? 3, 12));
  const base: FleetPlan = {
    version: 1,
    name: opts.name ?? `${preset}-fleet`,
    description: task,
    vars: { task },
    agents: [],
  };
  if (opts.model) base.model = opts.model;
  if (opts.cwd) base.cwd = opts.cwd;

  if (preset === "solo") {
    base.agents = [{ id: "agent", role: "solo", prompt: "{{vars.task}}" }];
    return base;
  }

  if (preset === "pipeline") {
    base.agents = Array.from({ length: count }, (_, i) => {
      const id = `step${i + 1}`;
      const previous = `step${i}`;
      return {
        id,
        role: `step ${i + 1}`,
        dependsOn: i === 0 ? [] : [previous],
        prompt:
          i === 0
            ? `You are step 1 of ${count} working on this task:\n\n{{vars.task}}\n\nDo your part and end with a concise handoff describing what you changed and what remains.`
            : `You are step ${i + 1} of ${count} working on this task:\n\n{{vars.task}}\n\nThe previous step reported:\n\n{{outputs.${previous}}}\n\nContinue from there. End with a concise handoff.`,
      };
    });
    return base;
  }

  if (preset === "review") {
    const reviewers = Math.max(1, count - 1);
    base.agents = [
      {
        id: "implement",
        role: "implementer",
        prompt: `Implement this task:\n\n{{vars.task}}\n\nEnd with a summary of every file you changed and why.`,
      },
      ...Array.from({ length: reviewers }, (_, i) => ({
        id: `review${i + 1}`,
        role: `reviewer (${FANOUT_ANGLES[i % FANOUT_ANGLES.length]})`,
        dependsOn: ["implement"],
        permissionMode: "plan" as const,
        prompt: `Review the work just done for this task:\n\n{{vars.task}}\n\nThe implementer reported:\n\n{{outputs.implement}}\n\nReview it strictly through the lens of ${FANOUT_ANGLES[i % FANOUT_ANGLES.length]}. Read the actual files. List concrete, actionable defects only — no praise, no restating the diff. If you find nothing real, say NO FINDINGS.`,
      })),
      {
        id: "fix",
        role: "fixer",
        dependsOn: Array.from({ length: reviewers }, (_, i) => `review${i + 1}`),
        prompt: `Original task:\n\n{{vars.task}}\n\nReviews came back:\n\n${Array.from(
          { length: reviewers },
          (_, i) => `--- reviewer ${i + 1} ---\n{{outputs.review${i + 1}}}`,
        ).join("\n\n")}\n\nApply the findings that are genuinely correct. Ignore the ones that are wrong or out of scope, and say which you ignored and why.`,
      },
    ];
    return base;
  }

  // fanout
  base.agents = [
    ...Array.from({ length: count }, (_, i) => ({
      id: `worker${i + 1}`,
      role: `worker (${FANOUT_ANGLES[i % FANOUT_ANGLES.length]})`,
      prompt: `Task:\n\n{{vars.task}}\n\nYou are one of ${count} agents working in parallel. Focus on ${FANOUT_ANGLES[i % FANOUT_ANGLES.length]}. Investigate for real — read files, run commands. End with your findings and recommendation.`,
    })),
    {
      id: "synthesis",
      role: "synthesiser",
      dependsOn: Array.from({ length: count }, (_, i) => `worker${i + 1}`),
      prompt: `Task:\n\n{{vars.task}}\n\n${count} agents investigated in parallel:\n\n${Array.from(
        { length: count },
        (_, i) => `--- worker ${i + 1} ---\n{{outputs.worker${i + 1}}}`,
      ).join("\n\n")}\n\nMerge these into one answer. Resolve contradictions by checking the code yourself. Drop anything unsupported.`,
    },
  ];
  return base;
}
