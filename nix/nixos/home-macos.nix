{ config, pkgs, ... }: let
  hms = "${pkgs.home-manager}/bin/home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixos#macos\"";
in {
  programs.zsh = {
    shellAliases = {
      hms = hms;
      nds = "darwin-rebuild switch --flake \"$HOME/.dotfiles/nix/nixos#macos\"";
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
    };
  };
}
