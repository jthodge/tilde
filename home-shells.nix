{ config, ... }:

let
  checkout = "${config.home.homeDirectory}/tilde";

  # Only the existing Bash and Zsh source files. Fish remains Stow-owned.
  files = {
    ".bash_profile" = "bash/.bash_profile";
    ".spaceshiprc.zsh" = "zsh/.spaceshiprc.zsh";
    ".zprofile" = "zsh/.zprofile";
    ".zshenv" = "zsh/.zshenv";
    ".zshrc" = "zsh/.zshrc";
  };
in
{
  # Preserve checkout editing and native shell installations.
  home.file = builtins.mapAttrs (_: source: {
    source = config.lib.file.mkOutOfStoreSymlink "${checkout}/${source}";
  }) files;
}
