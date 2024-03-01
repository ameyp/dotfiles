{ config, pkgs, ... }: {
  programs.zsh = {
    shellAliases = {
      hms = "${pkgs.home-manager}/bin/home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixos#linux\" --extra-experimental-features \"nix-command flakes\"";
    };
  };
}
