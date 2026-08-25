import { describe, expect, test } from "bun:test";
import { lookup, placeholders, render, TemplateError } from "../src/template.ts";

describe("template", () => {
  test("substitutes dotted paths", () => {
    const out = render("hi {{vars.name}} / {{outputs.a}}", {
      vars: { name: "world" },
      outputs: { a: "result-a" },
    });
    expect(out).toBe("hi world / result-a");
  });

  test("tolerates whitespace inside the braces", () => {
    expect(render("{{  vars.x  }}", { vars: { x: "1" } })).toBe("1");
  });

  test("repeats a placeholder used more than once", () => {
    expect(render("{{vars.x}}-{{vars.x}}", { vars: { x: "z" } })).toBe("z-z");
  });

  test("stringifies non-string values", () => {
    expect(render("{{n}}", { n: 42 })).toBe("42");
  });

  test("throws on an unresolved placeholder and names it", () => {
    expect(() => render("a {{outputs.missing}} b", { outputs: {} })).toThrow(TemplateError);
    try {
      render("{{outputs.one}} {{vars.two}}", {});
    } catch (error) {
      expect(error).toBeInstanceOf(TemplateError);
      expect((error as TemplateError).missing).toEqual(["outputs.one", "vars.two"]);
    }
  });

  test("leaves text without placeholders untouched", () => {
    expect(render("plain { not } a placeholder", {})).toBe("plain { not } a placeholder");
  });

  test("lookup walks objects and stops safely", () => {
    expect(lookup({ a: { b: { c: 1 } } }, "a.b.c")).toBe(1);
    expect(lookup({ a: 1 }, "a.b.c")).toBeUndefined();
    expect(lookup({}, "nope")).toBeUndefined();
  });

  test("placeholders lists each name once, in order", () => {
    expect(placeholders("{{a}} {{b}} {{a}}")).toEqual(["a", "b"]);
  });
});
