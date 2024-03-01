{ config, pkgs, ... }:

{
  home.homeDirectory = "/Users/amey";
  home.packages = [
  ];

  programs.zsh = {
    shellAliases = {
      hms = "home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixpkgs#amey@macos\"";
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
}
