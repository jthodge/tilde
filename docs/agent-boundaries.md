# Agent boundaries and ownership

These extensions prevent some accidents. **They are not a sandbox.** They
cannot contain extension/package I/O, subprocesses, network access, or a tool
whose implementation differs from its name. Use independently configured OS
isolation when containment is required.

## Plan mode

`/plan` and Alt+P restrict the model to the intersection of its **currently
active** tools with `read`, `grep`, `find`, and `ls`. Missing or disabled tools
stay disabled. The `tool_call` hook blocks every other tool, including tools
registered after entry. A custom tool named `read` is still trusted by name.

- Entry snapshots active tools and the registry.
- Exit restores that snapshot, excluding removed tools. The sole addition is
  a newly registered tool that is still active at exit.
- Before session replacement, the temporary restriction is removed. New and
  forked sessions start with fresh plan state.
- Startup/resume/reload and tree navigation read only the active session
  branch. The incoming session's active tools are authoritative, not an old,
  wider persisted snapshot. Reload restores the pre-plan surface before it
  re-enters plan mode; tree navigation also releases the old branch's filter.
- `--plan` persists its entry immediately. Reload respects subsequent toggles
  rather than applying the CLI flag again.
- Execution restores the caller's tools, not a hardcoded “full access” set.

The existing numbered-plan extraction, progress widget, and `[DONE:n]` markers
remain. Hooks: `session_start`, `session_shutdown`, `session_tree`, `tool_call`,
`context`, `before_agent_start`, `turn_end`, and `agent_end`.

## Other guards

| Extension | Hook and scope | Limits |
| --- | --- | --- |
| `permission-gate.ts` | `tool_call`, `bash` only; regexes for recursive `rm`, the word `sudo`, and `chmod/chown` with `777` | Not a shell parser. Other tools, indirect execution, redirection, and unmatched commands are not contained. No UI means a matched command is blocked. |
| `protected-paths.ts` | `tool_call`, `edit`/`write` only; literal substrings `.env`, `.git/`, `node_modules/` | No path canonicalization. Shell writes, aliases, symlinks and other tools are not covered. |

Their behavior is unchanged. Tests record both actual blocks and known gaps;
they do not execute the destructive example command strings.

## Web search: one integration owner

The configured `npm:pi-web-access` package provides `web_search`,
`source_check`, `fetch_content`, and `get_search_content`. The stable
`native-web-search` skill now directs callers to those tools. Its duplicate
OAuth/credential wrapper, `search.mjs`, is removed because it repeated package
responsibilities—not because its non-use was proven. No authentication store
was read or migrated. Package configuration and credential handling remain
with the packaged integration and the APIs it uses.

## Global Git hooks: deferred composition

The existing LFS hooks remain byte-for-byte unchanged. They still require
`git-lfs`, including outside LFS repositories. Missing-LFS auto-detection was
rejected: checking current attributes alone can miss historical pointers and
can mistake failed inspection for absence. Do not skip uploads on that basis.
A future per-repository hook composition change needs explicit migration and
coverage for local/global attributes, history, worktrees, and inspection errors.
Global `core.hooksPath` still takes precedence over repository-local hooks.

## Verification

`make verify` runs the actual three extension sources after Node's built-in
TypeScript stripping (Node 22.13+ on the 22.x line, or Node 24). Only the TUI
key helper import is mocked; callbacks run against an in-memory API fixture.
No Pi installation, credential access, or network request is needed. A missing
stripper or unexpected runtime import fails the check.

This tests callback behavior, not a running Pi TUI or the full SDK type surface.
See [verification.md](verification.md). Reload Pi explicitly to load the new
extension code; this change does not restart a live agent session.
