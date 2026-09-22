{ config, ... }:

let
  checkout = "${config.home.homeDirectory}/tilde";

  # Explicit files only: never link or copy whole bin/SSH directories.
  # Runtime installs, private keys, known_hosts, and local includes stay local.
  files = {
    ".local/bin/uv-python-simlink" = "bin/.local/bin/uv-python-simlink";
    ".ssh/allowed_signers" = "ssh/.ssh/allowed_signers";
    ".ssh/config" = "ssh/.ssh/config";
  };
in
{
  # Direct source links preserve executable permissions and checkout editing.
  home.file = builtins.mapAttrs (_: source: {
    source = config.lib.file.mkOutOfStoreSymlink "${checkout}/${source}";
  }) files;
}
