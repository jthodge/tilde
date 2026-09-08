/**
 * Permission Gate Extension
 *
 * A confirmation prompt on a small set of obviously destructive bash
 * command shapes. Does not sandbox anything.
 *
 * Coverage (single hook, single tool):
 *   - Hook: `tool_call`
 *   - Tool: `bash` only
 *   - Patterns: `rm -rf|-r|--recursive`, bare `sudo`, `chmod|chown` with `777`
 *
 * Out of scope:
 *   - Any other tool (`edit`, `write`, `powershell`, custom shell tools,
 *     extension tools that execute commands, MCP-backed tools)
 *   - Sibling shells reached through `bash -c`, `sh -c`, `env`, backticks,
 *     `$(...)`, subshells, aliases, functions, sourced scripts, cron,
 *     launchd/systemd, ssh/mosh into remote hosts
 *   - Shell operators and I/O redirection (`>`, `>>`, `|`, `&&`), pathname
 *     glob expansion, and arbitrary interpreter payloads (python -c,
 *     node -e, perl -e, awk 'BEGIN{...}')
 *   - Package I/O (npm/yarn/pnpm/pip/brew/apt install|uninstall|update)
 *   - Network egress (curl/wget/nc, git push/pull, ssh) and long-running
 *     services
 *
 * The extension is a nudge, not a sandbox. Treat it as one prompt on a
 * handful of common footguns. Do not rely on it to contain a hostile
 * model or a novel command shape. For real containment, use OS-level
 * mechanisms such as containers, seccomp, or sandbox-exec.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  const dangerousPatterns = [/\brm\s+(-rf?|--recursive)/i, /\bsudo\b/i, /\b(chmod|chown)\b.*777/i];

  pi.on("tool_call", async (event, ctx) => {
    if (event.toolName !== "bash") return undefined;

    const command = event.input.command as string;
    const isDangerous = dangerousPatterns.some((p) => p.test(command));

    if (isDangerous) {
      if (!ctx.hasUI) {
        // In non-interactive mode, block by default
        return { block: true, reason: "Dangerous command blocked (no UI for confirmation)" };
      }

      const choice = await ctx.ui.select(`⚠️ Dangerous command:\n\n  ${command}\n\nAllow?`, [
        "Yes",
        "No",
      ]);

      if (choice !== "Yes") {
        return { block: true, reason: "Blocked by user" };
      }
    }

    return undefined;
  });
}
