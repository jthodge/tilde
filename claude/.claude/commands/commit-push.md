---
allowed-tools: Bash(git:*), Bash(gh:*)
description: Verify, sign, and push an authorized change to a checked branch
---

## Task

The invocation authorizes a commit and an ordinary push of the requested
changes. It does not authorize a default-branch push, force-push, or merge.

1. Read repository instructions, status, and the complete diff. Leave unrelated
   changes unstaged. Ask about ambiguous scope.
2. Check the branch, remote URL, upstream, and the remote's default branch.
   Stop on detached HEAD. If the branch is `main`, `master`, or the default
   branch, use a topic branch unless the user explicitly requests publication
   to that default branch. Never infer force-push permission.
3. Run the relevant lint, typecheck, and tests. Read the final diff. Report any
   check that is unavailable; do not describe it as passed. Stop on failure.
4. Stage only the requested files. Create a Conventional Commit with signing
   enabled through the 1Password SSH agent. If signing fails, stop and report
   the error. Never use `--no-gpg-sign` or bypass verification hooks.
5. Verify the commit signature, then push that commit to the checked remote
   and topic branch without force. Stop if the remote rejects the push.
6. Report the commit, branch, verification results, and push result.

Use the normal read and verification tools as needed. Do not combine these
steps in a way that prevents inspection of a failed check.
