{
  description = "flake for nixOS";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix.url = "github:Mic92/sops-nix";
  };

  outputs = inputs@{ self, home-manager, nixpkgs, sops-nix, emacs-overlay, nix-darwin, ... }:
    let
      pkg-config = {
        allowUnfree = true;
        allowInsecure = false;
      };
      common-overlays = [
        (import (builtins.fetchTarball {
          url = https://github.com/nix-community/emacs-overlay/archive/3ab303101f287c1769f0a0dc4f7ec5473e61f94f.tar.gz;
          sha256 = "12ha034fi88zshmjniwcslkxwmv66jdz5cn54sqfcf174gx85jli";
        }))
        (import ./overlays/emacs.nix)
      ];
      darwin-system = ./system-macos.nix;
      darwin-pkgs = import nixpkgs {
        system = "aarch64-darwin";
        config = pkg-config;
        overlays = common-overlays;
      };
      linux-system = ./system-linux.nix;
      linux-pkgs = import nixpkgs {
        system = "x86_64-linux";
        config = pkg-config // {
          # Enable Pulseaudio support
          pulseaudio = true;
        };
        overlays = common-overlays;
      };
    in {
      # Linux NixOS configurations
      nixosConfigurations.linux = nixpkgs.lib.nixosSystem {
        pkgs = linux-pkgs;
        system = "x86_64-linux";
        modules = [
          linux-system
          sops-nix.nixosModules.sops
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.amey = {
              imports = [
                (import ./home-linux-personal.nix)
                (import ./home-linux.nix)
                (import ./home-personal.nix)
                (import ./home.nix)
              ];
            };
          }
        ];
      };

      # macOS nix-darwin configurations
      darwinConfigurations.macos = nix-darwin.lib.darwinSystem {
        pkgs = darwin-pkgs;
        modules = [
          darwin-system
          # This should work, but complains about systemd being a missing attribute.
          home-manager.darwinModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.aparulek = {
              imports = [
                (import ./home-macos-work.nix)
                (import ./home-macos.nix)
                (import ./home-work.nix)
                (import ./home.nix)
              ];
            };
          }
        ];
      };

      # Standalone home-manager configurations
      homeConfigurations = {
        "linux" = home-manager.lib.homeManagerConfiguration {
          pkgs = linux-pkgs;

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./home-linux-personal.nix
            ./home-linux.nix
            ./home-personal.nix
            ./home.nix
          ];
        };
        "linux-work" = home-manager.lib.homeManagerConfiguration {
          pkgs = linux-pkgs;

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./home-linux-work.nix
            ./home-linux.nix
            ./home-work.nix
            ./home.nix
          ];
        };
        "macos" = home-manager.lib.homeManagerConfiguration {
          pkgs = darwin-pkgs;

          # Specify your home configuration modules here, for example,
          # the path to your home.nix.
          modules = [
            ./home-macos-work.nix
            ./home-macos.nix
            ./home-work.nix
            ./home.nix
          ];
        };
      };
    };
}
