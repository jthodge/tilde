# Configuration adoption decisions

## Keep Stow until a new need justifies a change

The tracked setup describes one Apple Silicon workstation. The comparison with
Mark Tran's configuration did not establish a second managed host or a need
for system generations. Stow, explicit bootstrap, ownership rules, diagnostics,
and regression tests address the demonstrated problems without a Nix migration.

Revisit Nix when at least one of these occurs:

- Two maintained hosts repeatedly drift despite a shared manifest.
- A headless host needs a stable subset of the workstation configuration.
- Repeated system rebuilds make declarative OS settings worth their cost.
- A concrete recovery requirement needs package/configuration generations.

First pilot only a headless core: shell, Git, editor, and selected agent files.
Prove deployment and removal on a disposable host before managing the laptop.
Record the boundary: checkout links, native packages, credentials, and application
data are not made immutable or reversible by a Nix generation.

## Do not migrate personal tools without a demonstrated problem

A small tracked notes setup does not prove that the existing notes/task workflow
is inadequate. Do not migrate tasks, mail, calendars, or media into Emacs merely
because the mentor's configuration integrates them. No such migration or new
notes application was installed during this work.

If decision retrieval is a real problem, try this small loop in the existing
notes location for two weeks:

1. Keep one daily `YYYY-MM-DD.md` file.
2. Capture **context, decision, reason, and next action** after important work.
3. Search by project name or decision phrase using existing search tools.
4. Review the week's files for ten minutes; resolve or move next actions into
   the existing task system.
5. Keep the loop only if it retrieves useful decisions faster than before.

Magit, Embark, project search, and test commands already supply a bounded editor
pilot. Evaluate those before adding more packages. See `emacs-workflow.md` for
keys and a hands-on trial. Their daily-use benefit remains unmeasured.

## Record OS intent, not someone else's preferences

A read-only snapshot on 2026-09-08 found these explicit Boolean values:

| Domain | Key | Value |
|---|---|---|
| `NSGlobalDomain` | `AppleShowAllExtensions` | true |
| `NSGlobalDomain` | `NSAutomaticCapitalizationEnabled` | true |
| `com.apple.dock` | `autohide` | true |
| `com.apple.dock` | `show-recents` | false |
| `com.apple.finder` | `ShowPathbar` | true |
| `com.apple.finder` | `ShowStatusBar` | true |

No explicit value was returned for global `KeyRepeat`, `InitialKeyRepeat`,
`ApplePressAndHoldEnabled`, or `NSAutomaticSpellingCorrectionEnabled`. Do not
replace those with guessed effective defaults or Mark's preferences.

Read a value and its stored type before recording a future change:

```sh
defaults read com.apple.dock autohide
defaults read-type com.apple.dock autohide
```

This is a baseline, not an automatic restore script. No OS defaults were changed
and no system process was restarted. Review values before applying them to a new
machine. Fonts, 1Password, licenses, and Accessibility permissions remain explicit
manual prerequisites in the README.

## Remove complexity based on use

Before deleting an agent extension, alternate shell, or terminal:

- Record what it does, which daily command uses it, and which other component
  overlaps it.
- Distinguish a measured lack of use from an absence of evidence in dotfiles.
- Remove one unused mechanism at a time and run `make verify` and `make check`.
- Keep signing, secret handling, and explicit publishing authority intact.

The existing permission/path guards remain heuristic accident prevention, not
an OS sandbox. Their runtime APIs are outside the new Python/Lisp/Lua test scope.
No unmeasured agent or terminal workflow was deleted during this adoption.

## Success criteria: evidence and remaining work

| Step | Result | Evidence or outstanding check |
|---|---|---|
| 1. Safety | Passed targeted checks | Policy, link preservation, cleanup confirmation/failure tests; remote deletion mocked |
| 2. Bootstrap | Implemented; not fully proven | Required tools and plugins present on this host; fresh macOS install still untested |
| 3. Ownership | Passed targeted checks | Settings preserved; private backups; repeatable seeding; deployment check passes |
| 4. Shell startup | Passed isolated checks | No provisioning; non-interactive behavior and override ordering tested |
| 5. Emacs structure | Passed mechanical check | Original reconstruction documented in `emacs-workflow.md`; later behavior changes are separate commits |
| 6. Completion/environment | Implemented; not fully proven | ERT and offline full-init smoke pass; real cold-start TS/Python/Go and GUI sessions still need use |
| 7. Editor workflow | Ready for trial | Magit/Embark installed and loadable; project/test command tests pass; context-switch reduction unmeasured |
| 8. Terminal changes | Passed targeted checks | Key collision removed; private tmux regression and live shell settings checked; remote clipboard/visual checks remain |
| 9. Regression checks | Passed locally and in CI | `make verify`, fresh-home ERT, private tmux, and macOS Actions; `make smoke` also passes locally |
| 10. Reassessment | Decision complete | Keep Stow and existing personal tools; explicit adoption gates above |

The first CI run exposed an interpreter mismatch, not a successful check: old
Pyright encountered Python 3.14 library syntax. Selecting Python 3.13 in CI and
the current test interpreter for Pyright fixed it. Local verification also passed
under Python 3.9 and Python 3.13. See `verification.md` for exact coverage.

The final doctor probe reports all required tools present, with PATH alternatives.
An agent launched by Volta can inherit its own runtime image ahead of user shims;
that probe is not a fresh-shell runtime test. The stored user defaults remain
Node 22.14.0, pnpm 10.15.0, and yarn 4.4.0. Check the selected tools in fresh
terminal, tmux, GUI-editor, and two-project sessions before claiming full runtime
consistency. No existing runtime default was changed to silence a warning.
