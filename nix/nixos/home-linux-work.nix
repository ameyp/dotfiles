{ config, pkgs, ... }: let
  hms = "${pkgs.home-manager}/bin/home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixos#linux-work\" --extra-experimental-features \"nix-command flakes\"";
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "aparulek";
  home.homeDirectory = "/usr2/${config.home.username}";

  programs.zsh.shellAliases = {
    hms = hms;
  };

  programs.fish.shellAbbrs = {
    hms = hms;
  };
}
