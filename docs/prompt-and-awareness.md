# Prompt & Situational Awareness

## Why this design

Ghostty is the sole supported terminal. The prompt is Mark Tran's small
Fish-native implementation, with no prompt framework or Nerd Font icons.
This is a low-noise preference, not a Ghostty rendering limitation. Situational
awareness that a verbose prompt used to *push* is now *pulled* on demand.
The optional `ctx` function below is a proposal, not an installed command.

- **Push (old):** every prompt redraw shows node/python/rustc/go/kubectl/aws/tf
  versions + context, each behind a Nerd Font glyph.
- **Pull (now):** the prompt shows only `pwd` + git; you query the rest with a
  command when you actually need it.

## (1) The prompt

Sources of truth:

- `fish/.config/fish/functions/fish_prompt.fish`
- `fish/.config/fish/functions/set_pwd_color.fish`
- The three `__fish_git_prompt_*` settings in `fish/.config/fish/colors.fish`

The two functions are copied verbatim from Mark Tran's `tilde` checkout at
revision `9deaa1a4f0748c8e39adff28c632916636c33186`, under
`nix/files/fish/functions/`. The Git settings match his
`nix/files/fish/config.d/git-prompt.fish`. His checkout is not a runtime or
test dependency. Fish supplies `prompt_pwd` and `__fish_git_prompt` (the latter
is a compatibility wrapper for `fish_git_prompt` in current Fish).

- One line: shortened directory, then Git branch/state, then the command.
- Directory is magenta locally and blue when `SSH_CLIENT` is nonempty.
- Branch is yellow; unstaged changes use `±`; Fish also supplies staged state.
- No frames, background blocks, extra prompt character, or right prompt.
- Untracked/stash/upstream counters and exit-status coloring are not enabled.
- `VIRTUAL_ENV_DISABLE_PROMPT=1` in `exports.fish` prevents Python activation
  from adding a venv prefix. Syntax highlighting and tool integrations are unchanged.

Example, with an unstaged change:

```text
~/s/project (master ±) git status
```

Tide's implementation, configuration, completions, and plugin entry are removed.
`fish_plugins` is intentionally empty; Fisher remains available but has no
prompt plugin to install. Do not run `fisher remove` against the replacement:
Tide's old file inventory includes `fish_prompt.fish`, which now belongs to this
repository's Fish-native implementation.

In each already-running Fish shell, run `exec fish -l` to discard the old
in-memory Tide functions and load the replacement. Merely sourcing `config.fish`
is insufficient: it does not erase Tide's generated prompt/event functions.

## (2) Pull-based awareness — tools you already have

Every signal the old prompt showed has an existing command. None of these
require new tooling.

| Signal                | Command (already installed)                          |
|-----------------------|------------------------------------------------------|
| Node version          | `node -v`  ·  `volta list node`                      |
| pnpm version          | `pnpm -v`                                             |
| Python version        | `python -V`                                           |
| Active venv           | `echo $VIRTUAL_ENV`  (name: `basename $VIRTUAL_ENV`)  |
| Rust                  | `rustc --version`  ·  `rustup show`                   |
| Go                    | `go version`                                          |
| All mise-managed vers | `mise current`  (only tools mise manages)            |
| Git branch + status   | `git status -sb`  (branch, ahead/behind, dirty)      |
| k8s context           | `kubectl config current-context`  (local, fast)      |
| k8s namespace         | `kubectl config view --minify -o 'jsonpath={..namespace}'` |
| AWS profile           | `echo $AWS_PROFILE`                                   |
| AWS identity          | `aws sts get-caller-identity`  (network call)         |
| Terraform workspace   | `terraform workspace show`                            |
| direnv state          | `direnv status`                                       |

Note: `mise current` is the closest off-the-shelf aggregator, but it only
reports mise-managed tools — node (Volta) and python (uv) won't appear, so it
isn't a complete substitute for your stack. That gap is what `ctx` (below) fills.

## (3) `ctx` — one-shot environment snapshot (optional glue)

A tiny fish function that runs the per-tool commands above and prints a compact,
**local-only (no network)** snapshot, showing only what's relevant to the
current directory. It's the only bespoke piece — pure glue over existing tools.

### Install

Save as `fish/.config/fish/functions/ctx.fish` (autoloads on first `ctx` call):

```fish
function _ctx_row
    printf '%s%-9s%s %s\n' (set_color brblack) $argv[1] (set_color normal) (string join ' ' $argv[2..-1])
end

function ctx --description "On-demand environment snapshot (local, no network)"
    _ctx_row pwd $PWD
    if git rev-parse --is-inside-work-tree 2>/dev/null >/dev/null
        _ctx_row git (git status -sb 2>/dev/null | head -1 | string replace '## ' '')
    end
    command -q node; and _ctx_row node (node -v)
    command -q pnpm; and _ctx_row pnpm (pnpm -v)
    if set -q VIRTUAL_ENV
        _ctx_row python (python -V 2>&1 | string replace 'Python ' '') "(venv:" (basename $VIRTUAL_ENV)")"
    else if command -q python
        _ctx_row python (python -V 2>&1 | string replace 'Python ' '')
    end
    command -q rustc; and _ctx_row rust (rustc --version | string split ' ')[2]
    command -q go; and _ctx_row go (go version | string match -rg 'go([0-9.]+)')
    command -q kubectl; and _ctx_row k8s (kubectl config current-context 2>/dev/null)
    set -q AWS_PROFILE; and _ctx_row aws $AWS_PROFILE
    command -q terraform; and test -d .terraform; and _ctx_row terraform (terraform workspace show 2>/dev/null)
end
```

### Example output

```
pwd       /Users/jth/conductor/workspaces/client/minnetonka-v2
git       fix/onboarding-redirect *3
node      v22.14.0
pnpm      9.15.0
python    3.13.2 (venv: base)
k8s       primitive-prod
aws       campus-staging
```

### Tuning

- **Add a signal:** add a `command -q <tool>; and _ctx_row <label> (<cmd>)` line.
- **Drop a signal:** delete its line.
- **Want network identity** (AWS account, not just profile name): add
  `command -q aws; and _ctx_row aws-id (aws sts get-caller-identity --query Account --output text 2>/dev/null)`
  — note this adds latency and is intentionally omitted from the fast default.

### Optional: surface it in tmux

Bind a key to pop `ctx` in a small split (add to `tmux/.tmux.conf`):

```tmux
bind-key i split-window -v -l 10 "fish -lc ctx; read"
```

Then `` `-i `` shows the snapshot without leaving your current pane.
