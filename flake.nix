{
  description = "tilde";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }: {
    packages.aarch64-darwin.hello =
      nixpkgs.legacyPackages.aarch64-darwin.hello;

    homeConfigurations.jth =
      home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.aarch64-darwin;
        modules = [ ./home.nix ];
      };
  };
}
