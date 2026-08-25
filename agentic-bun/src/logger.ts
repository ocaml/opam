/** Tiny leveled logger with per-agent colored prefixes. No dependencies. */

export type LogLevel = "silent" | "error" | "warn" | "info" | "debug";

const ORDER: Record<LogLevel, number> = {
  silent: 0,
  error: 1,
  warn: 2,
  info: 3,
  debug: 4,
};

const CODES = {
  reset: "\x1b[0m",
  dim: "\x1b[2m",
  bold: "\x1b[1m",
  red: "\x1b[31m",
  green: "\x1b[32m",
  yellow: "\x1b[33m",
  blue: "\x1b[34m",
  magenta: "\x1b[35m",
  cyan: "\x1b[36m",
  gray: "\x1b[90m",
} as const;

export type ColorName = keyof typeof CODES;

/** Colors cycled through when assigning a prefix color to each agent. */
const AGENT_COLORS: ColorName[] = [
  "cyan",
  "magenta",
  "green",
  "yellow",
  "blue",
  "red",
];

function colorEnabled(): boolean {
  if (process.env.NO_COLOR) return false;
  if (process.env.FORCE_COLOR) return true;
  return Boolean(process.stderr.isTTY || process.stdout.isTTY);
}

export class Logger {
  level: LogLevel;
  private readonly color: boolean;
  private readonly assigned = new Map<string, ColorName>();
  private write: (s: string) => void;

  constructor(level: LogLevel = "info", opts: { color?: boolean; write?: (s: string) => void } = {}) {
    this.level = level;
    this.color = opts.color ?? colorEnabled();
    this.write = opts.write ?? ((s) => process.stdout.write(s));
  }

  private enabled(level: Exclude<LogLevel, "silent">): boolean {
    return ORDER[this.level] >= ORDER[level];
  }

  paint(text: string, ...styles: ColorName[]): string {
    if (!this.color || styles.length === 0) return text;
    return styles.map((s) => CODES[s]).join("") + text + CODES.reset;
  }

  /** Stable color for an agent id, assigned in first-seen order. */
  agentColor(id: string): ColorName {
    const existing = this.assigned.get(id);
    if (existing) return existing;
    const next = AGENT_COLORS[this.assigned.size % AGENT_COLORS.length] ?? "cyan";
    this.assigned.set(id, next);
    return next;
  }

  line(text = ""): void {
    this.write(text + "\n");
  }

  debug(msg: string): void {
    if (this.enabled("debug")) this.line(this.paint("· " + msg, "gray"));
  }

  info(msg: string): void {
    if (this.enabled("info")) this.line(msg);
  }

  step(msg: string): void {
    if (this.enabled("info")) this.line(this.paint("▸ ", "blue") + msg);
  }

  success(msg: string): void {
    if (this.enabled("info")) this.line(this.paint("✔ ", "green") + msg);
  }

  warn(msg: string): void {
    if (this.enabled("warn")) this.line(this.paint("! ", "yellow") + msg);
  }

  error(msg: string): void {
    if (this.enabled("error")) this.line(this.paint("✖ ", "red") + msg);
  }

  /** A line attributed to one agent, e.g. `[reviewer] Bash(ls -la)`. */
  agent(id: string, msg: string, style: "text" | "tool" | "meta" = "text"): void {
    if (!this.enabled("info")) return;
    const tag = this.paint(`[${id}]`, this.agentColor(id), "bold");
    const body =
      style === "tool"
        ? this.paint(msg, "yellow")
        : style === "meta"
          ? this.paint(msg, "gray")
          : msg;
    this.line(`${tag} ${body}`);
  }
}

/** Collapse whitespace and clip, so one stream event stays on one line. */
export function oneLine(value: unknown, max = 160): string {
  let text: string;
  if (typeof value === "string") text = value;
  else {
    try {
      text = JSON.stringify(value) ?? String(value);
    } catch {
      text = String(value);
    }
  }
  text = text.replace(/\s+/g, " ").trim();
  return text.length > max ? text.slice(0, max - 1) + "…" : text;
}
