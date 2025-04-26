{ config, lib, pkgs, ... }: let
  hms = "${pkgs.home-manager}/bin/home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixos#macos\"";
  nds = "darwin-rebuild switch --flake \"$HOME/.dotfiles/nix/nixos#macos\"";
in {
  home.username = lib.mkDefault "amey";
  home.homeDirectory = lib.mkDefault "/Users/${config.home.username}";

  programs.zsh = {
    shellAliases = {
      hms = lib.mkDefault hms;
      nds = lib.mkDefault nds;
    };
    envExtra = ''
      [[ -f /opt/homebrew/bin/brew ]] && export PATH=/opt/homebrew/bin:/opt/homebrew/sbin:$PATH

      # Nix
      NIX_DAEMON="/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
      if [ -e $NIX_DAEMON ]; then
        . $NIX_DAEMON
      fi
    '';
  };

  programs.fish = {
    shellAbbrs = {
      hms = hms;
      nds = nds;
    };
    shellInit = ''
      # Configure homebrew
      if test -f /opt/homebrew/bin/brew
        fish_add_path /opt/homebrew/bin
        fish_add_path /opt/homebrew/sbin
      end

      # Configure nix
      set -x NIX_DAEMON "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.fish"
      if test -f $NIX_DAEMON
        source $NIX_DAEMON
      end
    '';
  };

  services.syncthing = {
    enable = true;
    settings = {
      options = {
        relaysEnabled = false;
        urAccepted = -1;
        # Disabled until https://github.com/nix-community/home-manager/pull/6104 is merged.
        overrideDevices = false;
        overrideFolders = false;
      };
      devices = {
        "nixos44" = {
          id = "TIAM3PH-OTIFZ7P-Q5T6EMQ-AHFNE7U-H6MLRMX-EFSMRRI-PSTSUTE-PLFU4QF";
          addresses = ["tcp://syncthing.wirywolf.com:22000"];
        };
      };
      folders = {
        "/Users/amey/Denote" = {
          id = "quamc-7jxpj";
          devices = ["nixos44"];
        };
      };
    };
  };
}
