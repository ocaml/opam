/** Public API of the Agentic Bun Framework. */

export type {
  AgentResult,
  AgentSpec,
  AgentStatus,
  FleetPlan,
  FleetRunResult,
  LaunchMode,
  LaunchPlan,
  PermissionMode,
  PromptDelivery,
  ResolvedAgent,
  ResolvedFleet,
  ToolCallRecord,
} from "./types.ts";

export { Fleet, runFleet, type FleetOptions, type PreflightReport } from "./orchestrator.ts";
export { runAgent, bunExec, type AgentContext, type ExecFn, type ProcessOutcome } from "./agent.ts";
export { EventBus, type FleetEvent, type FleetEventMap } from "./events.ts";
export { Blackboard } from "./blackboard.ts";
export { Logger, oneLine, type LogLevel } from "./logger.ts";
export {
  buildClaudeArgs,
  buildLaunchPlan,
  directEnv,
  formatCommand,
  renderLaunchCommand,
  resolveMode,
  DEFAULT_LAUNCH_COMMAND,
} from "./launcher.ts";
export {
  buildPreset,
  DEFAULTS,
  loadPlan,
  parsePlan,
  PlanError,
  PRESETS,
  resolveFleet,
  stripJsonComments,
  validatePlan,
  type FleetOverrides,
  type PresetName,
} from "./plan.ts";
export {
  doctor,
  hasBinary,
  looksToolCapable,
  probe,
  probeLaunchSupport,
  resolveModel,
  warm,
  DEFAULT_HOST,
  TOOL_CAPABLE_HINTS,
  type Doctor,
  type OllamaProbe,
} from "./ollama.ts";
export {
  executeGraph,
  findCycle,
  GraphError,
  topoLayers,
  validateGraph,
  type GraphNode,
  type NodeStatus,
} from "./scheduler.ts";
export { interpret, NdjsonParser, type StreamEvent } from "./stream.ts";
export { lookup, placeholders, render, TemplateError } from "./template.ts";
export { runWizard, type WizardResult } from "./wizard.ts";
