{ config, ... }:

let
  checkout = "${config.home.homeDirectory}/tilde";
in
{
  imports = [ ./home-shells.nix ];

  home.username = "jth";
  home.homeDirectory = "/Users/jth";

  home.stateVersion = "26.05";

  home.file.".local/share/tilde-nix/hello.txt".text =
    "Hello from Home Manager.\n";

  home.file.".config/ghostty/config".source =
    config.lib.file.mkOutOfStoreSymlink
      "${checkout}/ghostty/.config/ghostty/config";
}
