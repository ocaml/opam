import { afterEach, describe, expect, test } from "bun:test";
import { looksToolCapable, probe, resolveModel, warm } from "../src/ollama.ts";

const realFetch = globalThis.fetch;
afterEach(() => {
  globalThis.fetch = realFetch;
});

/** Serve canned responses per path, so no socket is ever opened. */
function stubFetch(routes: Record<string, unknown>, opts: { fail?: boolean } = {}): string[] {
  const calls: string[] = [];
  globalThis.fetch = (async (input: Parameters<typeof fetch>[0]) => {
    const url = String(input);
    calls.push(url);
    if (opts.fail) throw new Error("ECONNREFUSED");
    const key = Object.keys(routes).find((route) => url.endsWith(route));
    if (!key) return new Response("not found", { status: 404 });
    return new Response(JSON.stringify(routes[key]), {
      status: 200,
      headers: { "content-type": "application/json" },
    });
  }) as typeof fetch;
  return calls;
}

describe("resolveModel", () => {
  const installed = ["llama3.1:8b", "qwen2.5-coder:latest", "tinyllama:latest"];

  test("keeps an exact match", () => {
    expect(resolveModel("llama3.1:8b", installed)).toBe("llama3.1:8b");
  });

  test("resolves an untagged name to the installed tag", () => {
    expect(resolveModel("qwen2.5-coder", installed)).toBe("qwen2.5-coder:latest");
  });

  test("returns a requested model that is not installed, so preflight can warn", () => {
    expect(resolveModel("mistral-nemo", installed)).toBe("mistral-nemo");
  });

  test("with no preference, prefers a known tool-calling model over the first one", () => {
    expect(resolveModel(undefined, ["tinyllama:latest", "llama3.1:8b"])).toBe("llama3.1:8b");
  });

  test("falls back to the first installed model when none look tool-capable", () => {
    expect(resolveModel(undefined, ["tinyllama:latest", "phi:latest"])).toBe("tinyllama:latest");
  });

  test("returns empty when nothing is installed and nothing was asked for", () => {
    expect(resolveModel(undefined, [])).toBe("");
  });

  test("respects a preference even with nothing installed", () => {
    expect(resolveModel("qwen3", [])).toBe("qwen3");
  });
});

describe("looksToolCapable", () => {
  test("recognises the known families", () => {
    expect(looksToolCapable("qwen2.5-coder:7b")).toBe(true);
    expect(looksToolCapable("llama3.1:8b")).toBe(true);
    expect(looksToolCapable("tinyllama:latest")).toBe(false);
  });
});

describe("probe", () => {
  test("reports models and version from a healthy server", async () => {
    stubFetch({
      "/api/tags": { models: [{ name: "b:latest" }, { name: "a:latest" }] },
      "/api/version": { version: "0.15.2" },
    });
    const result = await probe("http://localhost:11434");
    expect(result.reachable).toBe(true);
    expect(result.models).toEqual(["a:latest", "b:latest"]); // sorted
    expect(result.version).toBe("0.15.2");
  });

  test("survives a server without /api/version", async () => {
    stubFetch({ "/api/tags": { models: [] } });
    const result = await probe("http://localhost:11434");
    expect(result.reachable).toBe(true);
    expect(result.version).toBe("");
  });

  test("reports unreachable instead of throwing", async () => {
    stubFetch({}, { fail: true });
    const result = await probe("http://localhost:11434");
    expect(result.reachable).toBe(false);
    expect(result.error).toContain("ECONNREFUSED");
    expect(result.models).toEqual([]);
  });

  test("normalises a trailing slash in the host", async () => {
    const calls = stubFetch({ "/api/tags": { models: [] } });
    const result = await probe("http://localhost:11434/");
    expect(result.host).toBe("http://localhost:11434");
    expect(calls[0]).toBe("http://localhost:11434/api/tags");
  });
});

describe("warm", () => {
  test("posts an empty generate with keep_alive", async () => {
    let body: unknown;
    globalThis.fetch = (async (_url: Parameters<typeof fetch>[0], init?: RequestInit) => {
      body = JSON.parse(String(init?.body));
      return new Response("{}", { status: 200 });
    }) as typeof fetch;
    expect(await warm("m", "http://h", "10m")).toBe(true);
    expect(body).toEqual({ model: "m", prompt: "", keep_alive: "10m" });
  });

  test("returns false rather than throwing when the server is down", async () => {
    stubFetch({}, { fail: true });
    expect(await warm("m", "http://h")).toBe(false);
  });
});
