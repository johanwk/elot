// src/db/bridge.ts
//
// Step 2.3.1: read-only ElotDb bridge for the VS Code extension.
//
// Responsibilities:
//   - lazy-open an ElotDb at a path supplied by the caller
//   - watch the file for changes (debounced) and re-open on change
//   - notify listeners on every successful (re)load
//   - tolerate missing files (returns null until one appears)
//
// No vscode imports: `resolvePath()` is supplied by the caller, so
// the bridge can be unit-tested under tsx without an extension host.
// The extension wires `resolvePath()` to `resolveExtensionDbPath`
// driven by the configuration system.

import { existsSync, watch, FSWatcher } from "fs";
import { ElotDb } from "./sqljs.js";

export type BridgeListener = () => void;

export interface BridgeOptions {
  /** Resolve the current DB path; called on every (re)load. */
  resolvePath: () => string | null;
  /** Debounce delay (ms) for file-change reload.  Default 200. */
  debounceMs?: number;
  /** Logger; defaults to console.error tagged "[ElotDbBridge]". */
  onError?: (err: unknown) => void;
}

function defaultErr(err: unknown): void {
  // eslint-disable-next-line no-console
  console.error("[ElotDbBridge]", err);
}

/**
 * Read-only bridge over `ElotDb`.  The CLI is the sole writer;
 * the bridge therefore never calls `save()`.  Use `get()` to obtain
 * the current handle (opens lazily); subscribe via `onDidChange()`
 * to refresh hover/decoration caches when the file is rewritten.
 */
export class ElotDbBridge {
  private db: ElotDb | null = null;
  private watcher: FSWatcher | null = null;
  private currentPath: string | null = null;
  private listeners = new Set<BridgeListener>();
  private debounceTimer: ReturnType<typeof setTimeout> | null = null;
  private disposed = false;
  private loading: Promise<ElotDb | null> | null = null;

  constructor(private readonly opts: BridgeOptions) {}

  /** Returns the current DB instance, opening on first access. */
  async get(): Promise<ElotDb | null> {
    if (this.disposed) return null;
    if (this.db) return this.db;
    if (this.loading) return this.loading;
    return this.reload();
  }

  /**
   * Force a reload now (path may have changed, e.g. after a
   * settings update, or the file may have been replaced).
   */
  async reload(): Promise<ElotDb | null> {
    if (this.disposed) return null;
    if (this.loading) return this.loading;
    this.loading = this.doReload().finally(() => {
      this.loading = null;
    });
    return this.loading;
  }

  private async doReload(): Promise<ElotDb | null> {
    const newPath = this.opts.resolvePath();
    // Close existing.
    if (this.db) {
      try {
        this.db.close();
      } catch {
        /* ignore */
      }
      this.db = null;
    }
    this.currentPath = newPath;
    this.startWatch(newPath);
    if (!newPath || !existsSync(newPath)) {
      this.notify();
      return null;
    }
    try {
      this.db = await ElotDb.open(newPath);
    } catch (err) {
      (this.opts.onError ?? defaultErr)(err);
      this.db = null;
    }
    this.notify();
    return this.db;
  }

  /** Subscribe to (re)load events.  Returns a disposer. */
  onDidChange(listener: BridgeListener): () => void {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  }

  /** The path the bridge most recently attempted to open. */
  get path(): string | null {
    return this.currentPath;
  }

  /** True if the underlying DB is currently open. */
  get isOpen(): boolean {
    return this.db !== null;
  }

  dispose(): void {
    if (this.disposed) return;
    this.disposed = true;
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
      this.debounceTimer = null;
    }
    this.stopWatch();
    if (this.db) {
      try {
        this.db.close();
      } catch {
        /* ignore */
      }
      this.db = null;
    }
    this.listeners.clear();
  }

  private notify(): void {
    for (const l of this.listeners) {
      try {
        l();
      } catch (err) {
        (this.opts.onError ?? defaultErr)(err);
      }
    }
  }

  private stopWatch(): void {
    if (this.watcher) {
      try {
        this.watcher.close();
      } catch {
        /* ignore */
      }
      this.watcher = null;
    }
  }

  private startWatch(path: string | null): void {
    this.stopWatch();
    if (!path || !existsSync(path)) return;
    try {
      this.watcher = watch(path, { persistent: false }, () => {
        this.scheduleReload();
      });
    } catch (err) {
      (this.opts.onError ?? defaultErr)(err);
    }
  }

  private scheduleReload(): void {
    if (this.disposed) return;
    if (this.debounceTimer) clearTimeout(this.debounceTimer);
    const delay = this.opts.debounceMs ?? 200;
    this.debounceTimer = setTimeout(() => {
      this.debounceTimer = null;
      this.reload().catch((err) => (this.opts.onError ?? defaultErr)(err));
    }, delay);
  }
}
