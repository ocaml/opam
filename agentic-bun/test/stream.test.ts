import { describe, expect, test } from "bun:test";
import { interpret, NdjsonParser, type StreamEvent } from "../src/stream.ts";
import { claudeStream } from "./helpers.ts";

function collect(chunks: string[]): unknown[] {
  const out: unknown[] = [];
  const parser = new NdjsonParser((value) => out.push(value));
  for (const chunk of chunks) parser.push(chunk);
  parser.flush();
  return out;
}

describe("NdjsonParser", () => {
  test("splits complete lines", () => {
    expect(collect(['{"a":1}\n{"b":2}\n'])).toEqual([{ a: 1 }, { b: 2 }]);
  });

  test("reassembles a value split across chunks", () => {
    expect(collect(['{"a"', ":1}", "\n"])).toEqual([{ a: 1 }]);
  });

  test("flush emits a trailing line with no newline", () => {
    expect(collect(['{"a":1}'])).toEqual([{ a: 1 }]);
  });

  test("ignores blank lines", () => {
    expect(collect(["\n\n", '{"a":1}\n', "\n"])).toEqual([{ a: 1 }]);
  });

  test("surfaces non-JSON lines instead of throwing", () => {
    expect(collect(["not json\n"])).toEqual([{ __nonJson: "not json" }]);
  });

  test("handles a byte-at-a-time stream", () => {
    const payload = '{"a":1}\n{"b":2}\n';
    expect(collect([...payload])).toEqual([{ a: 1 }, { b: 2 }]);
  });
});

describe("interpret", () => {
  const first = (value: unknown): StreamEvent => interpret(value)[0]!;

  test("init", () => {
    const event = first({
      type: "system",
      subtype: "init",
      session_id: "s1",
      model: "m",
      tools: ["Read", "Bash"],
    });
    expect(event).toEqual({ kind: "init", sessionId: "s1", model: "m", tools: ["Read", "Bash"] });
  });

  test("assistant text", () => {
    expect(
      first({ type: "assistant", message: { content: [{ type: "text", text: "hello" }] } }),
    ).toEqual({ kind: "text", text: "hello" });
  });

  test("drops empty assistant text blocks", () => {
    const events = interpret({
      type: "assistant",
      message: { content: [{ type: "text", text: "   " }] },
    });
    expect(events[0]!.kind).toBe("unknown");
  });

  test("tool_use becomes a one-line summary", () => {
    const event = first({
      type: "assistant",
      message: {
        content: [{ type: "tool_use", name: "Bash", input: { command: "ls\n-la" } }],
      },
    });
    expect(event).toMatchObject({ kind: "tool-use", name: "Bash" });
    expect((event as { summary: string }).summary).toContain("ls");
    expect((event as { summary: string }).summary).not.toContain("\n");
  });

  test("one assistant message can yield several events", () => {
    const events = interpret({
      type: "assistant",
      message: {
        content: [
          { type: "text", text: "thinking about it" },
          { type: "tool_use", name: "Read", input: { file_path: "a.ts" } },
        ],
      },
    });
    expect(events.map((e) => e.kind)).toEqual(["text", "tool-use"]);
  });

  test("tool_result, including the error flag and block arrays", () => {
    const event = first({
      type: "user",
      message: {
        content: [
          {
            type: "tool_result",
            is_error: true,
            content: [{ type: "text", text: "no such file" }],
          },
        ],
      },
    });
    expect(event).toEqual({ kind: "tool-result", summary: "no such file", isError: true });
  });

  test("result carries the final text and metadata", () => {
    const event = first({
      type: "result",
      subtype: "success",
      is_error: false,
      result: "done",
      num_turns: 3,
      session_id: "s1",
      total_cost_usd: 0.01,
      duration_ms: 500,
    });
    expect(event).toEqual({
      kind: "result",
      text: "done",
      isError: false,
      numTurns: 3,
      sessionId: "s1",
      costUsd: 0.01,
      durationMs: 500,
    });
  });

  test("a non-success result subtype counts as an error", () => {
    const event = first({ type: "result", subtype: "error_max_turns", result: "" });
    expect(event).toMatchObject({ kind: "result", isError: true });
  });

  test("unknown shapes never throw", () => {
    expect(first({ type: "totally_new" }).kind).toBe("unknown");
    expect(first(null).kind).toBe("unknown");
    expect(first("string").kind).toBe("unknown");
    expect(first({ type: "assistant" }).kind).toBe("unknown");
  });

  test("a full fixture stream parses end to end", () => {
    const kinds = claudeStream({
      text: "all done",
      tools: [{ name: "Bash", input: { command: "ls" } }],
    })
      .map((line) => interpret(JSON.parse(line)))
      .flat()
      .map((e) => e.kind);
    expect(kinds).toEqual(["init", "tool-use", "tool-result", "text", "result"]);
  });
});
