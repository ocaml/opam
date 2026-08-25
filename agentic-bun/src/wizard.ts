/**
 * The wizard: a short, linear sequence of plain questions that produces a
 * runnable plan. No TUI, no dependencies — just questions and defaults.
 */

import type { FleetPlan, PermissionMode } from "./types.ts";
import { buildPreset, PRESETS, type PresetName } from "./plan.ts";
import { DEFAULT_HOST, probe, resolveModel } from "./ollama.ts";

export interface WizardResult {
  plan: FleetPlan;
  /** Where the user asked to save the plan, or "" for don't save. */
  savePath: string;
  runNow: boolean;
}

/** One question. Returns the trimmed answer, or the default when blank. */
function ask(question: string, fallback = ""): string {
  const suffix = fallback ? ` [${fallback}]` : "";
  const answer = globalThis.prompt?.(`${question}${suffix}`) ?? "";
  const trimmed = answer.trim();
  return trimmed.length > 0 ? trimmed : fallback;
}

function askChoice<T extends string>(question: string, choices: T[], fallback: T): T {
  const answer = ask(`${question} (${choices.join("/")})`, fallback);
  const match = choices.find((c) => c.toLowerCase() === answer.toLowerCase());
  return match ?? fallback;
}

function askNumber(question: string, fallback: number): number {
  const answer = ask(question, String(fallback));
  const parsed = Number.parseInt(answer, 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

function askYesNo(question: string, fallback: boolean): boolean {
  const answer = ask(`${question} (y/n)`, fallback ? "y" : "n");
  return /^y/i.test(answer);
}

export async function runWizard(host: string = DEFAULT_HOST): Promise<WizardResult> {
  console.log("agentic — build a fleet of local Claude Code agents\n");

  const task = ask("1. What should the fleet accomplish?");
  if (!task) throw new Error("a task is required");

  const cwd = ask("2. Which directory does it concern?", process.cwd());

  console.log("");
  for (const [name, description] of Object.entries(PRESETS)) {
    console.log(`     ${name.padEnd(9)} ${description}`);
  }
  const preset = askChoice<PresetName>(
    "3. Fleet shape",
    ["fanout", "pipeline", "review", "solo"],
    "fanout",
  );

  const agents = preset === "solo" ? 1 : askNumber("4. How many agents?", 3);

  // Only ask about the model when there is a real choice to make.
  const installed = await probe(host);
  const suggested = resolveModel(undefined, installed.models);
  let model = suggested;
  if (installed.models.length > 1) {
    console.log(`\n     installed: ${installed.models.join(", ")}`);
    model = ask("5. Which model?", suggested);
  } else if (installed.models.length === 0) {
    model = ask("5. Which model? (none detected)", suggested || "qwen2.5-coder");
  }

  const permissionMode = askChoice<PermissionMode>(
    "6. Permissions",
    ["acceptEdits", "plan", "bypassPermissions", "default"],
    "acceptEdits",
  );

  const plan = buildPreset(preset, task, { agents, model, cwd, name: `${preset}-fleet` });
  plan.permissionMode = permissionMode;
  plan.ollamaHost = host;

  const savePath = ask("7. Save the plan to a file? (blank to skip)", "");
  const runNow = askYesNo("8. Run it now?", true);

  return { plan, savePath, runNow };
}
