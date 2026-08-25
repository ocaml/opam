/**
 * The blackboard: the only channel agents use to pass work to each other.
 *
 * Agents are separate OS processes with no shared memory, so "communication"
 * here means: agent A finishes, its final text is stored, and agent B's prompt
 * template interpolates it before B is spawned.
 */

export class Blackboard {
  private readonly store = new Map<string, string>();

  set(key: string, value: string): void {
    this.store.set(key, value);
  }

  get(key: string): string | undefined {
    return this.store.get(key);
  }

  has(key: string): boolean {
    return this.store.has(key);
  }

  get size(): number {
    return this.store.size;
  }

  /** Plain-object snapshot, safe to serialise or hand to the templater. */
  snapshot(): Record<string, string> {
    return Object.fromEntries(this.store);
  }
}
