{
  description = "flake for nixOS";

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix.url = "github:Mic92/sops-nix";
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, home-manager, nixpkgs, nixpkgs-master, sops-nix, disko, emacs-overlay, nix-darwin, ... }:
    let
      pkg-config = {
        allowUnfree = true;
        allowInsecure = false;
      };
      overlay-linux-master = final: prev: {
        master = import nixpkgs-master {
          system = "x86_64-linux";
          config.allowUnfree = true;
        };
      };
      overlay-macos-master = final: prev: {
        master = import nixpkgs-master {
          system = "aarch64-darwin";
          config.allowUnfree = true;
        };
      };
      common-overlays = [
        (import (builtins.fetchTarball {
          url = https://github.com/nix-community/emacs-overlay/archive/b8e32860b5c94c75e9efb1779b9b5a4bd4a7d655.tar.gz;
          sha256 = "0b204pshb402kdkhrsvc9wk4r5yvidycnbrfydv9qrsrljm12x2y";
        }))
        (import ./overlays/emacs.nix)
      ];
      darwin-system = ./system-macos.nix;
      darwin-pkgs = import nixpkgs {
        system = "aarch64-darwin";
        config = pkg-config;
        overlays = common-overlays ++ [ overlay-macos-master ];
      };
      linux-pkgs = import nixpkgs {
        system = "x86_64-linux";
        config = pkg-config // {
          # Enable Pulseaudio support
          pulseaudio = true;
        };
        overlays = common-overlays ++ [ overlay-linux-master ];
      };
      linux-pkgs-aarch64 = import nixpkgs {
        system = "aarch64-linux";
        config = pkg-config // {
        };
        overlays = common-overlays ++ [ overlay-linux-master ];
      };
    in {
      # Linux NixOS configurations
      nixosConfigurations.linux = nixpkgs.lib.nixosSystem {
        pkgs = linux-pkgs;
        system = "x86_64-linux";
        modules = [
          ./system-linux-common.nix
          ./system-linux-hyprland.nix
          ./system-linux-desktop.nix
          ./system-linux.nix
          sops-nix.nixosModules.sops
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.amey = {
              imports = [
                (import ./home-linux-personal.nix)
                (import ./home-linux-hyprland.nix)
                (import ./home-linux.nix)
                (import ./home-personal.nix)
                (import ./home.nix)
              ];
            };
          }
        ];
      };

      nixosConfigurations.linux-thinkpad = nixpkgs.lib.nixosSystem {
        pkgs = linux-pkgs;
        system = "x86_64-linux";
        modules = [
          ./system-linux-common.nix
          ./system-linux-hyprland.nix
          ./system-linux-thinkpad.nix
          sops-nix.nixosModules.sops
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.amey = {
              imports = [
                (import ./home-linux-personal.nix)
                (import ./home-linux-hyprland.nix)
                (import ./home-linux.nix)
                (import ./home-personal.nix)
                (import ./home.nix)
              ];
            };
          }
        ];
      };

      nixosConfigurations.linux-laptop = nixpkgs.lib.nixosSystem {
        pkgs = linux-pkgs;
        system = "x86_64-linux";
        modules = [
          ./system-linux-common.nix
          ./system-linux-hyprland.nix
          ./system-linux-laptop.nix
          sops-nix.nixosModules.sops
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.amey = {
              imports = [
                (import ./home-linux-personal.nix)
                (import ./home-linux-hyprland.nix)
                (import ./home-linux.nix)
                (import ./home-linux-laptop.nix)
                (import ./home-personal.nix)
                (import ./home.nix)
              ];
            };
          }
        ];
      };

      nixosConfigurations.linux-vm = nixpkgs.lib.nixosSystem {
        pkgs = linux-pkgs-aarch64;
        system = "aarch64-linux";
        modules = [
          ./system-linux-common.nix
          ./system-linux-hyprland.nix
          ./system-linux-vm.nix
          disko.nixosModules.disko
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.amey = {
              imports = [
                (import ./home-linux-vm.nix)
                (import ./home-linux-hyprland.nix)
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
          darwin-system {
            systemMacOs.username = "amey";
          }
          # This should work, but complains about systemd being a missing attribute.
          home-manager.darwinModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.amey = {
              imports = [
                (import ./home-macos.nix)
                (import ./home-personal.nix)
                (import ./home.nix)
              ];
            };
          }
        ];
      };

      darwinConfigurations.macos-work = nix-darwin.lib.darwinSystem {
        pkgs = darwin-pkgs;
        modules = [
          darwin-system {
            systemMacOs.username = "aparulek";
          }
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
            ./home-linux-hyprland.nix
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
            ./home-macos.nix
            ./home-personal.nix
            ./home.nix
          ];
        };
        "macos-work" = home-manager.lib.homeManagerConfiguration {
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
