// src/sourceCommands/reorder.ts
//
// Step 2.3.4: pure helpers for reordering and set arithmetic on the
// `elot.activeLabelSources` list.  No vscode import - tsx-testable.
//
// Identity is `(source, dataSource)`; dataSource defaults to "" when
// missing (matches `normalizeActiveSources` policy).

export interface SourceKey {
  source: string;
  dataSource: string;
}

export function keyEquals(a: SourceKey, b: SourceKey): boolean {
  return a.source === b.source && (a.dataSource ?? "") === (b.dataSource ?? "");
}

export function keyOf(item: { source: string; dataSource?: string }): string {
  return `${item.source}\u0000${item.dataSource ?? ""}`;
}

export function indexOfKey(arr: SourceKey[], k: SourceKey): number {
  for (let i = 0; i < arr.length; i++) if (keyEquals(arr[i], k)) return i;
  return -1;
}

function clone(arr: SourceKey[]): SourceKey[] {
  return arr.map((x) => ({ source: x.source, dataSource: x.dataSource ?? "" }));
}

export function moveUp(arr: SourceKey[], i: number): SourceKey[] {
  if (i <= 0 || i >= arr.length) return clone(arr);
  const out = clone(arr);
  [out[i - 1], out[i]] = [out[i], out[i - 1]];
  return out;
}

export function moveDown(arr: SourceKey[], i: number): SourceKey[] {
  if (i < 0 || i >= arr.length - 1) return clone(arr);
  const out = clone(arr);
  [out[i], out[i + 1]] = [out[i + 1], out[i]];
  return out;
}

export function moveTop(arr: SourceKey[], i: number): SourceKey[] {
  if (i <= 0 || i >= arr.length) return clone(arr);
  const out = clone(arr);
  const [item] = out.splice(i, 1);
  out.unshift(item);
  return out;
}

export function moveBottom(arr: SourceKey[], i: number): SourceKey[] {
  if (i < 0 || i >= arr.length - 1) return clone(arr);
  const out = clone(arr);
  const [item] = out.splice(i, 1);
  out.push(item);
  return out;
}

/** Append items not already present (preserves existing order). */
export function appendUnique(
  arr: SourceKey[],
  add: SourceKey[],
): SourceKey[] {
  const out = clone(arr);
  for (const item of add) {
    if (indexOfKey(out, item) === -1) {
      out.push({ source: item.source, dataSource: item.dataSource ?? "" });
    }
  }
  return out;
}

/** Remove items from arr by key. */
export function removeByKeys(
  arr: SourceKey[],
  remove: SourceKey[],
): SourceKey[] {
  return clone(arr).filter((x) => indexOfKey(remove, x) === -1);
}

/** Items in `all` that are NOT in `active` (set difference, preserves `all` order). */
export function inactiveOf(
  all: SourceKey[],
  active: SourceKey[],
): SourceKey[] {
  return clone(all).filter((x) => indexOfKey(active, x) === -1);
}
