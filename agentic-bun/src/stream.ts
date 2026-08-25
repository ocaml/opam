/**
 * Claude Code emits newline-delimited JSON when run with
 * `--print --output-format stream-json --verbose`. This module turns that byte
 * stream into a small, stable set of events the rest of the framework uses.
 *
 * Everything here is defensive: an unrecognised or malformed line yields an
 * `unknown` event rather than an exception. A local model driving Claude Code
 * will occasionally produce shapes we have not seen.
 */

import { oneLine } from "./logger.ts";

export type StreamEvent =
  | { kind: "init"; sessionId: string; model: string; tools: string[] }
  | { kind: "text"; text: string }
  | { kind: "thinking"; text: string }
  | { kind: "tool-use"; name: string; summary: string }
  | { kind: "tool-result"; summary: string; isError: boolean }
  | {
      kind: "result";
      text: string;
      isError: boolean;
      numTurns: number;
      sessionId: string;
      costUsd: number;
      durationMs: number;
    }
  | { kind: "unknown"; raw: unknown };

/**
 * Incremental NDJSON splitter. Feed it arbitrary chunks; it calls back once per
 * complete line and buffers the remainder.
 */
export class NdjsonParser {
  private buffer = "";

  constructor(private readonly onValue: (value: unknown, line: string) => void) {}

  push(chunk: string): void {
    this.buffer += chunk;
    let index = this.buffer.indexOf("\n");
    while (index >= 0) {
      const line = this.buffer.slice(0, index);
      this.buffer = this.buffer.slice(index + 1);
      this.emit(line);
      index = this.buffer.indexOf("\n");
    }
  }

  /** Flush a trailing line that never got its newline (process exited). */
  flush(): void {
    if (this.buffer.length > 0) {
      const line = this.buffer;
      this.buffer = "";
      this.emit(line);
    }
  }

  private emit(rawLine: string): void {
    const line = rawLine.trim();
    if (line.length === 0) return;
    try {
      this.onValue(JSON.parse(line), line);
    } catch {
      // Not JSON: surface it as a raw string so callers can still log it.
      this.onValue({ __nonJson: line }, line);
    }
  }
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === "object" && value !== null && !Array.isArray(value);
}

function str(value: unknown, fallback = ""): string {
  return typeof value === "string" ? value : fallback;
}

function num(value: unknown, fallback = 0): number {
  return typeof value === "number" && Number.isFinite(value) ? value : fallback;
}

/** Render a tool_result payload (string, or an array of content blocks). */
function flattenContent(content: unknown): string {
  if (typeof content === "string") return content;
  if (Array.isArray(content)) {
    return content
      .map((block) => {
        if (typeof block === "string") return block;
        if (isRecord(block) && typeof block.text === "string") return block.text;
        return oneLine(block, 200);
      })
      .join(" ");
  }
  return oneLine(content, 200);
}

/**
 * Normalise one parsed NDJSON value into zero or more {@link StreamEvent}s.
 * An assistant message can carry several content blocks, hence the array.
 */
export function interpret(value: unknown): StreamEvent[] {
  if (!isRecord(value)) return [{ kind: "unknown", raw: value }];
  if (typeof value.__nonJson === "string") {
    return [{ kind: "unknown", raw: value.__nonJson }];
  }

  switch (value.type) {
    case "system": {
      if (value.subtype !== "init") return [{ kind: "unknown", raw: value }];
      const tools = Array.isArray(value.tools)
        ? value.tools.filter((t): t is string => typeof t === "string")
        : [];
      return [
        {
          kind: "init",
          sessionId: str(value.session_id),
          model: str(value.model),
          tools,
        },
      ];
    }

    case "assistant": {
      const message = isRecord(value.message) ? value.message : {};
      const content = Array.isArray(message.content) ? message.content : [];
      const events: StreamEvent[] = [];
      for (const block of content) {
        if (!isRecord(block)) continue;
        if (block.type === "text" && typeof block.text === "string") {
          if (block.text.trim().length > 0) events.push({ kind: "text", text: block.text });
        } else if (block.type === "thinking" && typeof block.thinking === "string") {
          events.push({ kind: "thinking", text: block.thinking });
        } else if (block.type === "tool_use") {
          events.push({
            kind: "tool-use",
            name: str(block.name, "tool"),
            summary: oneLine(block.input ?? {}),
          });
        }
      }
      return events.length > 0 ? events : [{ kind: "unknown", raw: value }];
    }

    case "user": {
      const message = isRecord(value.message) ? value.message : {};
      const content = Array.isArray(message.content) ? message.content : [];
      const events: StreamEvent[] = [];
      for (const block of content) {
        if (!isRecord(block) || block.type !== "tool_result") continue;
        events.push({
          kind: "tool-result",
          summary: oneLine(flattenContent(block.content)),
          isError: block.is_error === true,
        });
      }
      return events.length > 0 ? events : [{ kind: "unknown", raw: value }];
    }

    case "result": {
      // `result` holds the final text on success; on error it may be absent and
      // the useful message lives in `error`/`subtype`.
      const text = str(value.result) || str(value.error) || "";
      return [
        {
          kind: "result",
          text,
          isError: value.is_error === true || value.subtype !== "success",
          numTurns: num(value.num_turns),
          sessionId: str(value.session_id),
          costUsd: num(value.total_cost_usd),
          durationMs: num(value.duration_ms),
        },
      ];
    }

    default:
      return [{ kind: "unknown", raw: value }];
  }
}
