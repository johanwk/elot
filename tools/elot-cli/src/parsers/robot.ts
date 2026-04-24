// src/parsers/robot.ts
//
// Thin wrapper around the ROBOT CLI (http://robot.obolibrary.org/).
// Mirrors elot-robot-available-p / elot-source--robot-run from
// elot-package/elot-sources.el.
//
// Resolution order (matches Elisp + plan):
//   1. $ELOT_ROBOT_JAR pointing at a robot.jar  -> java -jar <jar> ...
//   2. $ROBOT pointing at a robot executable    -> direct exec
//   3. `robot` on PATH                          -> direct exec
//
// `java` is taken from $JAVA (if set) else "java" on PATH.
// If none of the above work, robotAvailable() returns false and
// runRobotQuery() throws.  Tests that require ROBOT should skip
// themselves via robotAvailable() to keep CI green without Java.

import { spawnSync } from "child_process";
import { existsSync } from "fs";
import { delimiter, sep } from "path";

export interface RobotResolution {
  kind: "jar" | "exe";
  /** For "jar": path to robot.jar; for "exe": path to the robot binary. */
  path: string;
  /** For "jar": the `java` binary used.  Undefined for "exe". */
  java?: string;
}

function isExecutableOnPath(name: string): string | null {
  const p = process.env.PATH ?? "";
  const exts =
    process.platform === "win32"
      ? (process.env.PATHEXT ?? ".EXE;.BAT;.CMD").split(";")
      : [""];
  for (const dir of p.split(delimiter)) {
    if (!dir) continue;
    for (const ext of exts) {
      const candidate = dir.endsWith(sep) ? `${dir}${name}${ext}` : `${dir}${sep}${name}${ext}`;
      if (existsSync(candidate)) return candidate;
    }
  }
  return null;
}

export function resolveRobot(): RobotResolution | null {
  const jar = process.env.ELOT_ROBOT_JAR;
  if (jar && jar.length > 0 && existsSync(jar)) {
    const java =
      (process.env.JAVA && isExecutableOnPath(process.env.JAVA)) ||
      isExecutableOnPath("java");
    if (java) return { kind: "jar", path: jar, java };
  }
  const exeEnv = process.env.ROBOT;
  if (exeEnv && exeEnv.length > 0 && existsSync(exeEnv)) {
    return { kind: "exe", path: exeEnv };
  }
  const onPath = isExecutableOnPath("robot");
  if (onPath) return { kind: "exe", path: onPath };
  return null;
}

export function robotAvailable(): boolean {
  return resolveRobot() !== null;
}

/**
 * Run `robot query --input <input> --query <query> <output>`.
 * OUTPUT-FILE's extension determines the result format; callers pass
 * a .csv path since the downstream parser consumes CSV.  Throws on
 * non-zero exit with ROBOT's stderr captured.
 */
export function runRobotQuery(
  queryFile: string,
  inputFile: string,
  outputFile: string,
): void {
  const r = resolveRobot();
  if (!r) {
    throw new Error(
      "ROBOT not available: set $ELOT_ROBOT_JAR or $ROBOT, " +
        "or install `robot` on PATH",
    );
  }
  const baseArgs = [
    "query",
    "--input",
    inputFile,
    "--query",
    queryFile,
    outputFile,
  ];
  const { program, args } =
    r.kind === "jar"
      ? { program: r.java!, args: ["-jar", r.path, ...baseArgs] }
      : { program: r.path, args: baseArgs };
  const res = spawnSync(program, args, { encoding: "utf-8" });
  if (res.error) {
    throw new Error(`ROBOT spawn failed: ${res.error.message}`);
  }
  if (res.status !== 0) {
    const stderr = (res.stderr ?? "").trim();
    throw new Error(
      `ROBOT query failed (exit ${res.status}): ${stderr || "(no stderr)"}`,
    );
  }
}
