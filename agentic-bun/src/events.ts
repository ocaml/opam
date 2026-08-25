/** Minimal typed event bus. Listener errors never break a run. */

import type { AgentResult, ToolCallRecord } from "./types.ts";

export interface FleetEventMap {
  "fleet:start": { runId: string; name: string; agents: string[] };
  "fleet:end": { runId: string; ok: boolean; durationMs: number };
  "agent:queued": { id: string; role: string };
  "agent:start": { id: string; role: string; attempt: number; cmd: string[] };
  "agent:text": { id: string; text: string };
  "agent:tool": { id: string; tool: ToolCallRecord };
  "agent:tool-result": { id: string; summary: string; isError: boolean };
  "agent:retry": { id: string; attempt: number; delayMs: number; reason: string };
  "agent:end": { id: string; result: AgentResult };
  "agent:skipped": { id: string; reason: string };
}

export type FleetEvent = keyof FleetEventMap;

type Handler<E extends FleetEvent> = (payload: FleetEventMap[E]) => void;

export class EventBus {
  private readonly handlers = new Map<FleetEvent, Set<(p: never) => void>>();

  on<E extends FleetEvent>(event: E, handler: Handler<E>): () => void {
    let set = this.handlers.get(event);
    if (!set) {
      set = new Set();
      this.handlers.set(event, set);
    }
    set.add(handler as (p: never) => void);
    return () => {
      set!.delete(handler as (p: never) => void);
    };
  }

  emit<E extends FleetEvent>(event: E, payload: FleetEventMap[E]): void {
    const set = this.handlers.get(event);
    if (!set) return;
    for (const handler of set) {
      try {
        (handler as Handler<E>)(payload);
      } catch {
        // A misbehaving observer must not take the fleet down.
      }
    }
  }
}
