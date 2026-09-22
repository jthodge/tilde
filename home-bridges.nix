{ config, ... }:

let
  checkout = "${config.home.homeDirectory}/tilde";

  # Preserve existing deployment boundaries, including mutable state locations.
  # These are out-of-store links, never directory copies or recursive links.
  links = {
    # agents
    ".agents" = "agents/.agents";

    # claude: settings.json remains a seed-only, app-owned regular file.
    ".claude/CLAUDE.md" = "claude/.claude/CLAUDE.md";
    ".claude/commands" = "claude/.claude/commands";

    # codex: config.toml, authentication, and sessions remain unmanaged.
    ".codex/AGENTS.md" = "codex/.codex/AGENTS.md";

    # emacs: retain ignored packages, caches, and Custom state in place.
    ".emacs.d" = "emacs/.emacs.d";

    # git: preserve hook contents, including unrelated local worktree edits.
    ".config/git/hooks" = "git/.config/git/hooks";
    ".config/git/ignore" = "git/.config/git/ignore";
    ".gitconfig" = "git/.gitconfig";

    # nvim: keep the writable lockfile and lazy.nvim's existing ownership.
    ".config/nvim" = "nvim/.config/nvim";
  };
in
{
  home.file = builtins.mapAttrs (_: source: {
    source = config.lib.file.mkOutOfStoreSymlink "${checkout}/${source}";
    recursive = false;
  }) links;
}
