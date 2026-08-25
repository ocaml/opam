import { describe, expect, test } from "bun:test";
import {
  buildClaudeArgs,
  buildLaunchPlan,
  directEnv,
  formatCommand,
  renderLaunchCommand,
  resolveMode,
  DEFAULT_LAUNCH_COMMAND,
} from "../src/launcher.ts";
import { resolveFleet } from "../src/plan.ts";
import type { ResolvedAgent, ResolvedFleet } from "../src/types.ts";

function fixture(overrides: Partial<ResolvedAgent> = {}): {
  agent: ResolvedAgent;
  fleet: ResolvedFleet;
} {
  const fleet = resolveFleet({
    name: "t",
    model: "qwen2.5-coder:latest",
    cwd: "/tmp",
    agents: [{ id: "a", prompt: "do the thing" }],
  });
  return { agent: { ...fleet.agents[0]!, ...overrides }, fleet };
}

describe("buildClaudeArgs", () => {
  test("always requests non-interactive streaming JSON", () => {
    const { agent } = fixture();
    const args = buildClaudeArgs(agent, null);
    expect(args.slice(0, 4)).toEqual(["--print", "--output-format", "stream-json", "--verbose"]);
  });

  test("passes through the knobs that are set", () => {
    const { agent } = fixture({
      permissionMode: "plan",
      maxTurns: 7,
      allowedTools: ["Read", "Grep"],
      disallowedTools: ["Bash"],
      addDirs: ["/srv/extra"],
      systemPrompt: "be terse",
    });
    const args = buildClaudeArgs(agent, null);
    expect(args).toContain("--permission-mode");
    expect(args[args.indexOf("--permission-mode") + 1]).toBe("plan");
    expect(args[args.indexOf("--max-turns") + 1]).toBe("7");
    expect(args[args.indexOf("--allowed-tools") + 1]).toBe("Read,Grep");
    expect(args[args.indexOf("--disallowed-tools") + 1]).toBe("Bash");
    expect(args[args.indexOf("--add-dir") + 1]).toBe("/srv/extra");
    expect(args[args.indexOf("--append-system-prompt") + 1]).toBe("be terse");
  });

  test("omits empty knobs", () => {
    const { agent } = fixture({ maxTurns: 0, systemPrompt: "  " });
    const args = buildClaudeArgs(agent, null);
    expect(args).not.toContain("--max-turns");
    expect(args).not.toContain("--append-system-prompt");
    expect(args).not.toContain("--allowed-tools");
  });

  test("appends the prompt as the last positional when asked", () => {
    const { agent } = fixture();
    const args = buildClaudeArgs(agent, "the prompt");
    expect(args.at(-1)).toBe("the prompt");
  });
});

describe("resolveMode", () => {
  test("auto follows what is installed", () => {
    expect(resolveMode("auto", true)).toBe("launch");
    expect(resolveMode("auto", false)).toBe("direct");
  });

  test("explicit modes ignore availability", () => {
    expect(resolveMode("launch", false)).toBe("launch");
    expect(resolveMode("direct", true)).toBe("direct");
  });
});

describe("buildLaunchPlan", () => {
  test("launch mode wraps claude in `ollama launch claude`", () => {
    const { agent, fleet } = fixture();
    const plan = buildLaunchPlan(agent, { mode: "launch", fleet, parentEnv: {} });
    expect(plan.cmd.slice(0, 5)).toEqual([
      "ollama",
      "launch",
      "claude",
      "--model",
      "qwen2.5-coder:latest",
    ]);
    expect(plan.cmd[5]).toBe("--");
    expect(plan.cmd[6]).toBe("--print");
  });

  test("launch mode does not leak an inherited ANTHROPIC_BASE_URL", () => {
    const { agent, fleet } = fixture();
    const plan = buildLaunchPlan(agent, {
      mode: "launch",
      fleet,
      parentEnv: { ANTHROPIC_BASE_URL: "https://api.example", PATH: "/bin" },
    });
    expect(plan.env.ANTHROPIC_BASE_URL).toBeUndefined();
    expect(plan.env.PATH).toBe("/bin");
  });

  test("direct mode runs claude itself and points it at Ollama", () => {
    const { agent, fleet } = fixture();
    const plan = buildLaunchPlan(agent, { mode: "direct", fleet, parentEnv: {} });
    expect(plan.cmd[0]).toBe("claude");
    expect(plan.env.ANTHROPIC_BASE_URL).toBe("http://127.0.0.1:11434");
    expect(plan.env.ANTHROPIC_MODEL).toBe("qwen2.5-coder:latest");
    expect(plan.env.ANTHROPIC_AUTH_TOKEN).toBe("ollama");
  });

  test("the prompt goes to stdin by default and is absent from argv", () => {
    const { agent, fleet } = fixture();
    const plan = buildLaunchPlan(agent, { mode: "direct", fleet, parentEnv: {} });
    expect(plan.stdin).toBe("do the thing");
    expect(plan.cmd).not.toContain("do the thing");
  });

  test("promptVia:arg puts the prompt on the command line instead", () => {
    const { agent, fleet } = fixture();
    const plan = buildLaunchPlan(agent, {
      mode: "direct",
      fleet: { ...fleet, promptVia: "arg" },
      parentEnv: {},
    });
    expect(plan.stdin).toBeNull();
    expect(plan.cmd.at(-1)).toBe("do the thing");
  });

  test("agent env wins over fleet env, and both are visible to the child", () => {
    const { agent, fleet } = fixture({ env: { SHARED: "agent", ONLY_AGENT: "1" } });
    const plan = buildLaunchPlan(agent, {
      mode: "direct",
      fleet: { ...fleet, env: { SHARED: "fleet", ONLY_FLEET: "1" } },
      parentEnv: {},
    });
    expect(plan.env.SHARED).toBe("agent");
    expect(plan.env.ONLY_FLEET).toBe("1");
    expect(plan.env.ONLY_AGENT).toBe("1");
    expect(plan.env.AGENTIC_AGENT_ID).toBe("a");
  });

  test("a custom launchCommand is honoured", () => {
    const { agent, fleet } = fixture();
    const plan = buildLaunchPlan(agent, {
      mode: "launch",
      fleet: { ...fleet, launchCommand: ["my-ollama", "launch", "claude", "-m", "{{model}}"] },
      parentEnv: {},
    });
    expect(plan.cmd.slice(0, 5)).toEqual([
      "my-ollama",
      "launch",
      "claude",
      "-m",
      "qwen2.5-coder:latest",
    ]);
  });
});

describe("helpers", () => {
  test("renderLaunchCommand substitutes the model", () => {
    expect(renderLaunchCommand(DEFAULT_LAUNCH_COMMAND, "m:tag")).toEqual([
      "ollama",
      "launch",
      "claude",
      "--model",
      "m:tag",
    ]);
  });

  test("directEnv strips trailing slashes and keeps the host root", () => {
    // The Anthropic client appends /v1/messages itself.
    expect(directEnv("http://localhost:11434/", "m").ANTHROPIC_BASE_URL).toBe(
      "http://localhost:11434",
    );
  });

  test("formatCommand quotes only what needs quoting", () => {
    expect(formatCommand(["claude", "--print", "hello world"])).toBe(
      'claude --print "hello world"',
    );
  });
});
