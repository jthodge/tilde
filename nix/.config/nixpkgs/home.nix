{ pkgs, ... }: {
  home.username = "th"; # REPLACE ME
  home.homeDirectory = "/Users/th"; # REPLACE ME
  home.stateVersion = "22.11";
  programs.home-manager.enable = true;
}
