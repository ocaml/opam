/**
 * Prompt templating.
 *
 * Placeholders are `{{ dotted.path }}`. The context a fleet builds is:
 *
 *   vars.<name>       values from the plan's `vars` block (and --var)
 *   outputs.<agentId> the final text of a completed agent
 *   board.<key>       any blackboard key (includes `outputKey` publications)
 *   env.<NAME>        process environment
 *   agent.id / agent.role / agent.model
 *
 * A placeholder that resolves to nothing is an error: a silently empty prompt
 * fragment is far worse than a failed run, because the agent still "succeeds".
 */

export type TemplateContext = Record<string, unknown>;

const PLACEHOLDER = /\{\{\s*([A-Za-z0-9_.\-]+)\s*\}\}/g;

export class TemplateError extends Error {
  constructor(
    message: string,
    readonly missing: string[],
  ) {
    super(message);
    this.name = "TemplateError";
  }
}

/** Resolve `a.b.c` against a plain object graph. Returns undefined if absent. */
export function lookup(ctx: TemplateContext, path: string): unknown {
  let current: unknown = ctx;
  for (const key of path.split(".")) {
    if (current === null || typeof current !== "object") return undefined;
    current = (current as Record<string, unknown>)[key];
    if (current === undefined) return undefined;
  }
  return current;
}

/** List the placeholders used by a template, in source order, deduplicated. */
export function placeholders(template: string): string[] {
  const found = new Set<string>();
  for (const match of template.matchAll(PLACEHOLDER)) {
    if (match[1]) found.add(match[1]);
  }
  return [...found];
}

/** Substitute every placeholder. Throws {@link TemplateError} on any miss. */
export function render(template: string, ctx: TemplateContext): string {
  const missing: string[] = [];
  const out = template.replace(PLACEHOLDER, (whole, path: string) => {
    const value = lookup(ctx, path);
    if (value === undefined || value === null) {
      missing.push(path);
      return whole;
    }
    return typeof value === "string" ? value : String(value);
  });
  if (missing.length > 0) {
    const unique = [...new Set(missing)];
    throw new TemplateError(
      `unresolved placeholder${unique.length > 1 ? "s" : ""}: ${unique.map((m) => `{{${m}}}`).join(", ")}`,
      unique,
    );
  }
  return out;
}
