{ config, pkgs, ... }: let
  hms = "${pkgs.home-manager}/bin/home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixos#linux-vm\" --extra-experimental-features \"nix-command flakes\"";
in {
  programs.zsh.shellAliases = {
    hms = hms;
  };

  programs.fish.shellAbbrs = {
    hms = hms;
  };
}
