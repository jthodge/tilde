/**
 * Protected Paths Extension
 *
 * Substring-match guard on write / edit paths.
 *
 * Coverage (single hook, exact tool set):
 *   - Hook: `tool_call`
 *   - Tools: `write`, `edit`
 *   - Match: literal substring `path.includes(protected)` on the tool
 *     argument, checked against `.env`, `.git/`, `node_modules/`
 *
 * Out of scope:
 *   - Any other tool. This does NOT gate `bash` (`rm`, `>`, `tee`,
 *     `sed -i`, `install`, `mv`, `cp`, redirection, subshells) or
 *     extension tools that perform file I/O.
 *   - Absolute vs. relative resolution, symlink traversal, `..` escapes,
 *     case normalization on case-insensitive filesystems. A caller
 *     spelling the same target differently can bypass the substring
 *     match.
 *   - Nested writes launched from a spawned process, MCP servers, or
 *     shell tools.
 *
 * This is a Pi-side reminder for the LLM. It is not a filesystem-level
 * protection and it is not a sandbox. Rely on backups, VCS, and OS
 * permissions for real protection.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  const protectedPaths = [".env", ".git/", "node_modules/"];

  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "write" && event.toolName !== "edit") {
      return undefined;
    }

    const path = event.input.path as string;
    const isProtected = protectedPaths.some((p) => path.includes(p));

    if (isProtected) {
      if (ctx.hasUI) {
        ctx.ui.notify(`Blocked write to protected path: ${path}`, "warning");
      }
      return { block: true, reason: `Path "${path}" is protected` };
    }

    return undefined;
  });
}
