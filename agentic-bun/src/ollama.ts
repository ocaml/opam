/**
 * Ollama is used for three things only — everything an agent actually says
 * goes through Claude Code, not through here:
 *
 *   1. preflight: is the server up, which models exist
 *   2. warm-up: load the model into memory before the fleet starts
 *   3. capability probe: does this Ollama have `ollama launch`
 */

export const DEFAULT_HOST = "http://127.0.0.1:11434";

export interface OllamaProbe {
  reachable: boolean;
  host: string;
  version: string;
  models: string[];
  error?: string;
}

/**
 * Models known to support tool calling in Ollama, best first. Claude Code is
 * useless without tool calling, so a fleet on a non-tool model just spins.
 */
export const TOOL_CAPABLE_HINTS = [
  "qwen3-coder",
  "qwen3",
  "qwen2.5-coder",
  "qwen2.5",
  "devstral",
  "gpt-oss",
  "llama3.3",
  "llama3.2",
  "llama3.1",
  "mistral-nemo",
  "mistral-small",
  "command-r",
  "hermes3",
  "granite3",
  "firefunction",
];

async function getJson(url: string, timeoutMs: number): Promise<unknown> {
  const response = await fetch(url, { signal: AbortSignal.timeout(timeoutMs) });
  if (!response.ok) throw new Error(`HTTP ${response.status} from ${url}`);
  return response.json();
}

/** Ask the Ollama server what it is and what it has. Never throws. */
export async function probe(host: string = DEFAULT_HOST, timeoutMs = 2500): Promise<OllamaProbe> {
  const base = host.replace(/\/+$/, "");
  try {
    const tags = (await getJson(`${base}/api/tags`, timeoutMs)) as {
      models?: { name?: string }[];
    };
    const models = (tags.models ?? [])
      .map((m) => m?.name)
      .filter((n): n is string => typeof n === "string")
      .sort();

    let version = "";
    try {
      const info = (await getJson(`${base}/api/version`, timeoutMs)) as { version?: string };
      version = info.version ?? "";
    } catch {
      // Older servers may not expose /api/version; not fatal.
    }

    return { reachable: true, host: base, version, models };
  } catch (error) {
    return {
      reachable: false,
      host: base,
      version: "",
      models: [],
      error: error instanceof Error ? error.message : String(error),
    };
  }
}

/**
 * Pick the model to run. `preferred` wins if it is installed (exact, or as a
 * `name:tag`-insensitive prefix). Otherwise prefer a known tool-capable model.
 */
export function resolveModel(preferred: string | undefined, available: string[]): string {
  if (preferred) {
    if (available.length === 0) return preferred;
    const exact = available.find((m) => m === preferred);
    if (exact) return exact;
    const tagged = available.find(
      (m) => m === `${preferred}:latest` || m.split(":")[0] === preferred,
    );
    if (tagged) return tagged;
    // Asked for something not installed: still return it, and let preflight warn.
    return preferred;
  }
  for (const hint of TOOL_CAPABLE_HINTS) {
    const match = available.find((m) => m.startsWith(hint));
    if (match) return match;
  }
  return available[0] ?? "";
}

/** Heuristic: does this model name look like one that can call tools? */
export function looksToolCapable(model: string): boolean {
  return TOOL_CAPABLE_HINTS.some((hint) => model.startsWith(hint));
}

/**
 * Load a model into memory so the first agent does not eat the cold-start.
 * An empty prompt with `keep_alive` is the cheapest way to do this.
 */
export async function warm(
  model: string,
  host: string = DEFAULT_HOST,
  keepAlive = "30m",
  timeoutMs = 120_000,
): Promise<boolean> {
  const base = host.replace(/\/+$/, "");
  try {
    const response = await fetch(`${base}/api/generate`, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ model, prompt: "", keep_alive: keepAlive }),
      signal: AbortSignal.timeout(timeoutMs),
    });
    return response.ok;
  } catch {
    return false;
  }
}

/** Run a command, capture stdout, and never throw. */
async function tryRun(cmd: string[], timeoutMs = 5000): Promise<{ ok: boolean; out: string }> {
  try {
    const proc = Bun.spawn(cmd, { stdout: "pipe", stderr: "pipe", stdin: "ignore" });
    const timer = setTimeout(() => proc.kill(), timeoutMs);
    const [out, err, code] = await Promise.all([
      new Response(proc.stdout).text(),
      new Response(proc.stderr).text(),
      proc.exited,
    ]);
    clearTimeout(timer);
    return { ok: code === 0, out: out + err };
  } catch (error) {
    return { ok: false, out: error instanceof Error ? error.message : String(error) };
  }
}

/** Is a binary on PATH? */
export async function hasBinary(name: string): Promise<boolean> {
  const { ok } = await tryRun(["which", name], 3000);
  return ok;
}

/**
 * Does this Ollama support `ollama launch claude`? Checked by inspecting the
 * help output rather than by running it, which would start an interactive CLI.
 */
export async function probeLaunchSupport(): Promise<boolean> {
  if (!(await hasBinary("ollama"))) return false;
  const help = await tryRun(["ollama", "launch", "--help"], 5000);
  if (help.ok && /claude/i.test(help.out)) return true;
  if (help.ok) return true;
  const top = await tryRun(["ollama", "--help"], 5000);
  return top.ok && /^\s*launch\b/m.test(top.out);
}

/** Full environment report used by `agentic doctor`. */
export interface Doctor {
  bunVersion: string;
  claude: boolean;
  ollamaBinary: boolean;
  launchSupport: boolean;
  probe: OllamaProbe;
  suggestedModel: string;
  toolCapable: boolean;
}

export async function doctor(host: string = DEFAULT_HOST, preferred?: string): Promise<Doctor> {
  const [claude, ollamaBinary, result] = await Promise.all([
    hasBinary("claude"),
    hasBinary("ollama"),
    probe(host),
  ]);
  const launchSupport = ollamaBinary ? await probeLaunchSupport() : false;
  const suggestedModel = resolveModel(preferred, result.models);
  return {
    bunVersion: typeof Bun !== "undefined" ? Bun.version : "",
    claude,
    ollamaBinary,
    launchSupport,
    probe: result,
    suggestedModel,
    toolCapable: suggestedModel ? looksToolCapable(suggestedModel) : false,
  };
}
