/**
 * Command-line interface.
 *
 *   agentic run [plan.json]   run a fleet
 *   agentic wizard            build a plan by answering a few questions
 *   agentic doctor            check bun / claude / ollama / model
 *   agentic models            list installed Ollama models
 *   agentic init [file]       write an example plan
 */

import { parseArgs } from "node:util";
import { resolve } from "node:path";
import { Logger, type LogLevel } from "./logger.ts";
import { Fleet } from "./orchestrator.ts";
import { formatCommand } from "./launcher.ts";
import {
  buildPreset,
  loadPlan,
  PlanError,
  PRESETS,
  resolveFleet,
  validatePlan,
  type FleetOverrides,
  type PresetName,
} from "./plan.ts";
import { doctor, DEFAULT_HOST, probe } from "./ollama.ts";
import type { FleetPlan, LaunchMode, PermissionMode } from "./types.ts";

export const VERSION = "0.1.0";

const USAGE = `agentic ${VERSION} — multi-agent Claude Code fleets on a local Ollama model

USAGE
  agentic <command> [options]

COMMANDS
  run [plan.json]      Run a fleet from a plan file, or from -p with a preset
  wizard               Answer a few questions, get a plan, optionally run it
  doctor               Check bun, claude, ollama, launch support and models
  models               List installed Ollama models
  init [file]          Write an example plan (default: agentic-plan.json)
  help, version

FLEET OPTIONS
  -p, --prompt <text>      Task text; builds a plan from a preset
      --preset <name>      ${Object.keys(PRESETS).join(" | ")} (default: fanout)
  -n, --agents <n>         Agents in the preset (default: 3)
  -m, --model <name>       Ollama model (default: first tool-capable installed)
      --host <url>         Ollama host (default: ${DEFAULT_HOST})
      --mode <m>           auto | launch | direct (default: auto)
  -c, --concurrency <n>    Agents running at once (default: 3)
      --cwd <dir>          Working directory for every agent
      --permission-mode <m>  default | acceptEdits | bypassPermissions | plan
      --max-turns <n>      Cap on Claude Code turns per agent
      --timeout <sec>      Per-agent timeout
      --retries <n>        Extra attempts per agent
      --allow <a,b>        Allowed tools
      --disallow <a,b>     Disallowed tools
      --var k=v            Template variable (repeatable)
      --fail-fast          Stop the whole fleet on the first failure
      --artifacts <dir>    Artifact root (default: <cwd>/.agentic/runs)
      --no-artifacts       Write nothing to disk
      --no-preflight       Skip the Ollama check
      --no-warm            Do not preload the model
      --dry-run            Print the resolved command per agent and exit
      --json               Emit the run result as JSON on stdout
  -v, --verbose            Debug logging
  -q, --quiet              Errors only

EXAMPLES
  agentic doctor
  agentic run -p "audit src/ for unchecked errors" -n 4
  agentic run examples/review-fleet.json --dry-run
  agentic run plan.json -c 2 --permission-mode plan --json
`;

const OPTIONS = {
  prompt: { type: "string", short: "p" },
  preset: { type: "string" },
  agents: { type: "string", short: "n" },
  model: { type: "string", short: "m" },
  host: { type: "string" },
  mode: { type: "string" },
  concurrency: { type: "string", short: "c" },
  cwd: { type: "string" },
  "permission-mode": { type: "string" },
  "max-turns": { type: "string" },
  timeout: { type: "string" },
  retries: { type: "string" },
  allow: { type: "string" },
  disallow: { type: "string" },
  var: { type: "string", multiple: true },
  "fail-fast": { type: "boolean" },
  artifacts: { type: "string" },
  "no-artifacts": { type: "boolean" },
  "no-preflight": { type: "boolean" },
  "no-warm": { type: "boolean" },
  "dry-run": { type: "boolean" },
  json: { type: "boolean" },
  save: { type: "string" },
  verbose: { type: "boolean", short: "v" },
  quiet: { type: "boolean", short: "q" },
  help: { type: "boolean", short: "h" },
  version: { type: "boolean" },
} as const;

type Values = Partial<Record<keyof typeof OPTIONS, string | boolean | string[]>>;

function str(values: Values, key: keyof typeof OPTIONS): string | undefined {
  const value = values[key];
  return typeof value === "string" ? value : undefined;
}

function bool(values: Values, key: keyof typeof OPTIONS): boolean {
  return values[key] === true;
}

function int(values: Values, key: keyof typeof OPTIONS): number | undefined {
  const raw = str(values, key);
  if (raw === undefined) return undefined;
  const parsed = Number.parseInt(raw, 10);
  if (!Number.isFinite(parsed) || parsed < 0) throw new PlanError(`--${key} must be a number`);
  return parsed;
}

function list(values: Values, key: keyof typeof OPTIONS): string[] | undefined {
  const raw = str(values, key);
  if (raw === undefined) return undefined;
  return raw
    .split(",")
    .map((s) => s.trim())
    .filter(Boolean);
}

function vars(values: Values): Record<string, string> {
  const raw = values.var;
  const entries = Array.isArray(raw) ? raw : typeof raw === "string" ? [raw] : [];
  const out: Record<string, string> = {};
  for (const entry of entries) {
    const index = entry.indexOf("=");
    if (index <= 0) throw new PlanError(`--var expects key=value (got "${entry}")`);
    out[entry.slice(0, index)] = entry.slice(index + 1);
  }
  return out;
}

function overridesFrom(values: Values): FleetOverrides {
  const overrides: FleetOverrides = {};
  const model = str(values, "model");
  if (model) overrides.model = model;
  const host = str(values, "host");
  if (host) overrides.ollamaHost = host;
  const mode = str(values, "mode");
  if (mode) {
    if (!["auto", "launch", "direct"].includes(mode)) {
      throw new PlanError("--mode must be auto, launch or direct");
    }
    overrides.launchMode = mode as LaunchMode;
  }
  const cwd = str(values, "cwd");
  if (cwd) overrides.cwd = resolve(cwd);
  const concurrency = int(values, "concurrency");
  if (concurrency !== undefined) overrides.concurrency = concurrency;
  const maxTurns = int(values, "max-turns");
  if (maxTurns !== undefined) overrides.maxTurns = maxTurns;
  const timeout = int(values, "timeout");
  if (timeout !== undefined) overrides.timeoutMs = timeout * 1000;
  const retries = int(values, "retries");
  if (retries !== undefined) overrides.retries = retries;
  const permissionMode = str(values, "permission-mode");
  if (permissionMode) {
    if (!["default", "acceptEdits", "bypassPermissions", "plan"].includes(permissionMode)) {
      throw new PlanError(`invalid --permission-mode: ${permissionMode}`);
    }
    overrides.permissionMode = permissionMode as PermissionMode;
  }
  if (bool(values, "fail-fast")) overrides.failFast = true;
  if (bool(values, "no-preflight")) overrides.skipPreflight = true;
  if (bool(values, "no-warm")) overrides.skipWarm = true;
  if (bool(values, "no-artifacts")) overrides.noArtifacts = true;
  const artifacts = str(values, "artifacts");
  if (artifacts) overrides.artifactsDir = artifacts;
  const allow = list(values, "allow");
  if (allow) overrides.allowedTools = allow;
  const disallow = list(values, "disallow");
  if (disallow) overrides.disallowedTools = disallow;
  const variables = vars(values);
  if (Object.keys(variables).length > 0) overrides.vars = variables;
  return overrides;
}

function makeLogger(values: Values): Logger {
  const level: LogLevel = bool(values, "quiet")
    ? "error"
    : bool(values, "verbose")
      ? "debug"
      : "info";
  // Progress goes to stderr; stdout carries only usage, --version and --json
  // payloads, so `agentic ... --json | jq` always works.
  return new Logger(level, { write: (s) => process.stderr.write(s) });
}

/** Build the plan for `run`, either from a file or from `-p` + preset. */
async function planFor(values: Values, positionals: string[]): Promise<FleetPlan> {
  const file = positionals[0];
  if (file) return loadPlan(file);

  const prompt = str(values, "prompt");
  if (!prompt) {
    throw new PlanError(
      "nothing to run: pass a plan file, or -p \"<task>\". Try `agentic wizard`.",
    );
  }
  const preset = (str(values, "preset") ?? "fanout") as PresetName;
  if (!(preset in PRESETS)) {
    throw new PlanError(`unknown preset "${preset}" (have: ${Object.keys(PRESETS).join(", ")})`);
  }
  const options: Parameters<typeof buildPreset>[2] = {};
  const agents = int(values, "agents");
  if (agents !== undefined) options.agents = agents;
  const model = str(values, "model");
  if (model) options.model = model;
  const cwd = str(values, "cwd");
  if (cwd) options.cwd = cwd;
  return buildPreset(preset, prompt, options);
}

const EXAMPLE_PLAN = `{
  // A fleet: one implementer, two parallel reviewers, one fixer.
  "version": 1,
  "name": "review-fleet",
  "concurrency": 3,
  "vars": { "task": "describe the task here" },
  "agents": [
    {
      "id": "implement",
      "role": "implementer",
      "prompt": "Implement this task:\\n\\n{{vars.task}}\\n\\nEnd with a summary of every file you changed."
    },
    {
      "id": "review",
      "role": "reviewer",
      "dependsOn": ["implement"],
      "permissionMode": "plan",
      "prompt": "Review this work:\\n\\n{{outputs.implement}}\\n\\nRead the real files. List concrete defects only."
    },
    {
      "id": "fix",
      "role": "fixer",
      "dependsOn": ["review"],
      "prompt": "Apply the correct findings from this review:\\n\\n{{outputs.review}}\\n\\nSay which you ignored and why."
    }
  ]
}
`;

async function cmdRun(values: Values, positionals: string[], log: Logger): Promise<number> {
  const plan = validatePlan(await planFor(values, positionals));
  const spec = resolveFleet(plan, overridesFrom(values));

  const controller = new AbortController();
  const onSigint = () => {
    log.warn("interrupted — stopping agents");
    controller.abort();
  };
  process.on("SIGINT", onSigint);

  try {
    const fleet = new Fleet(spec, { log, signal: controller.signal });

    if (bool(values, "dry-run")) {
      const { layers, plans } = await fleet.dryRun();
      const byId = new Map(plans.map((p) => [p.id, p.plan]));
      log.info("");
      layers.forEach((layer, index) => {
        log.step(`layer ${index + 1} — ${layer.length} agent${layer.length > 1 ? "s in parallel" : ""}`);
        for (const id of layer) {
          const launch = byId.get(id);
          if (!launch) continue;
          log.agent(id, formatCommand(launch.cmd), "tool");
          log.agent(id, `cwd ${launch.cwd}`, "meta");
          if (launch.stdin !== null) {
            log.agent(id, `stdin ${launch.stdin.length} chars`, "meta");
          }
        }
      });
      if (bool(values, "json")) {
        process.stdout.write(JSON.stringify({ layers, plans }, null, 2) + "\n");
      }
      return 0;
    }

    const result = await fleet.run();
    if (bool(values, "json")) {
      process.stdout.write(JSON.stringify(result, null, 2) + "\n");
    }
    return result.ok ? 0 : 1;
  } finally {
    process.off("SIGINT", onSigint);
  }
}

async function cmdWizard(values: Values, log: Logger): Promise<number> {
  const { runWizard } = await import("./wizard.ts");
  const host = str(values, "host") ?? DEFAULT_HOST;
  const { plan, savePath, runNow } = await runWizard(host);

  const target = str(values, "save") ?? savePath;
  if (target) {
    await Bun.write(target, JSON.stringify(plan, null, 2) + "\n");
    log.success(`plan written to ${target}`);
  }
  if (!runNow) {
    log.info("not running. `agentic run " + (target || "<plan.json>") + "` when you are ready.");
    return 0;
  }

  const spec = resolveFleet(plan, overridesFrom(values));
  const result = await new Fleet(spec, { log }).run();
  return result.ok ? 0 : 1;
}

async function cmdDoctor(values: Values, log: Logger): Promise<number> {
  const host = str(values, "host") ?? DEFAULT_HOST;
  const report = await doctor(host, str(values, "model"));
  const mark = (ok: boolean) => (ok ? log.paint("ok", "green") : log.paint("missing", "red"));

  log.info(`bun            ${report.bunVersion || "unknown"}`);
  log.info(`claude CLI     ${mark(report.claude)}`);
  log.info(`ollama binary  ${mark(report.ollamaBinary)}`);
  log.info(
    `ollama launch  ${report.launchSupport ? log.paint("supported", "green") : log.paint("unavailable (will use direct mode)", "yellow")}`,
  );
  log.info(
    `ollama server  ${report.probe.reachable ? log.paint(`up at ${report.probe.host}${report.probe.version ? ` (v${report.probe.version})` : ""}`, "green") : log.paint(`unreachable at ${report.probe.host}`, "red")}`,
  );
  log.info(`models         ${report.probe.models.length > 0 ? report.probe.models.join(", ") : "(none)"}`);
  log.info(`suggested      ${report.suggestedModel || "(none)"}`);

  const problems: string[] = [];
  if (!report.claude) problems.push("Claude Code is not installed — see https://claude.com/claude-code");
  if (!report.ollamaBinary) problems.push("Ollama is not installed — see https://ollama.com");
  if (!report.probe.reachable) problems.push("Ollama server is not running — start it with `ollama serve`");
  if (report.probe.reachable && report.probe.models.length === 0)
    problems.push("no models installed — try `ollama pull qwen2.5-coder`");
  if (report.suggestedModel && !report.toolCapable)
    problems.push(`"${report.suggestedModel}" may not support tool calling, which Claude Code requires`);

  log.info("");
  for (const problem of problems) log.warn(problem);
  if (problems.length === 0) log.success("ready to run a fleet");

  if (bool(values, "json")) process.stdout.write(JSON.stringify(report, null, 2) + "\n");
  return problems.length === 0 ? 0 : 1;
}

async function cmdModels(values: Values, log: Logger): Promise<number> {
  const host = str(values, "host") ?? DEFAULT_HOST;
  const result = await probe(host);
  if (!result.reachable) {
    log.error(`Ollama unreachable at ${result.host}: ${result.error ?? "no response"}`);
    return 1;
  }
  if (bool(values, "json")) {
    process.stdout.write(JSON.stringify(result.models, null, 2) + "\n");
  } else {
    for (const model of result.models) log.info(model);
    if (result.models.length === 0) log.warn("no models installed");
  }
  return 0;
}

async function cmdInit(positionals: string[], log: Logger): Promise<number> {
  const target = positionals[0] ?? "agentic-plan.json";
  if (await Bun.file(target).exists()) {
    log.error(`${target} already exists`);
    return 2;
  }
  await Bun.write(target, EXAMPLE_PLAN);
  log.success(`wrote ${target} — edit it, then \`agentic run ${target}\``);
  return 0;
}

export async function main(argv: string[]): Promise<number> {
  let parsed: { values: Values; positionals: string[] };
  try {
    parsed = parseArgs({
      args: argv,
      options: OPTIONS,
      allowPositionals: true,
      strict: true,
    }) as { values: Values; positionals: string[] };
  } catch (error) {
    process.stderr.write(`${error instanceof Error ? error.message : String(error)}\n\n${USAGE}`);
    return 2;
  }

  const { values, positionals } = parsed;
  const log = makeLogger(values);

  if (bool(values, "version")) {
    process.stdout.write(`${VERSION}\n`);
    return 0;
  }
  const command = positionals[0] ?? (values.prompt ? "run" : "help");
  const rest = positionals.slice(1);
  if (bool(values, "help") || command === "help") {
    process.stdout.write(USAGE);
    return 0;
  }

  try {
    switch (command) {
      case "run":
        return await cmdRun(values, rest, log);
      case "wizard":
        return await cmdWizard(values, log);
      case "doctor":
        return await cmdDoctor(values, log);
      case "models":
        return await cmdModels(values, log);
      case "init":
        return await cmdInit(rest, log);
      case "version":
        process.stdout.write(`${VERSION}\n`);
        return 0;
      default:
        log.error(`unknown command: ${command}`);
        process.stdout.write(USAGE);
        return 2;
    }
  } catch (error) {
    if (error instanceof PlanError) {
      log.error(error.message);
      return 2;
    }
    log.error(error instanceof Error ? (error.stack ?? error.message) : String(error));
    return 1;
  }
}
