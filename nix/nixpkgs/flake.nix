# Build from repo root with
# nix build --no-link "path:nix/nixpkgs#homeConfigurations.amey@macos.activationPackage"
# $(nix path-info "path:nix/nixpkgs#homeConfigurations.amey@macos.activationPackage")/activate
#
# Switch generations with
# home-manager switch --flake "path:nix/nixpkgs#amey@macos"

{
  description = "Home Manager configuration of Amey Parulekar";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }:
    let
    in {
      homeConfigurations = {
        "amey@linux" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./home.nix
            ./linux.nix
          ];
        };
        "amey@macos" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.aarch64-darwin;

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./home.nix
            ./macos.nix
          ];
        };
      };
    };
}