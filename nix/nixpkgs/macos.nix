{ config, pkgs, ... }:

{
  home.homeDirectory = "/Users/amey";
  home.packages = [
    # Disabled because of https://github.com/NixOS/nixpkgs/issues/127902
    # pkgs.emacsMacport
    pkgs.emacs
  ];

  programs.zsh = {
    shellAliases = {
      hms = "home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixpkgs#amey@macos\"";
    };
    envExtra = ''
      [[ -f /opt/homebrew/bin/brew ]] && export PATH=$PATH:/opt/homebrew/bin:/opt/homebrew/sbin

      # Nix
      NIX_DAEMON="/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"
      if [ -e $NIX_DAEMON ]; then
        . $NIX_DAEMON
      fi
    '';
  };
}
