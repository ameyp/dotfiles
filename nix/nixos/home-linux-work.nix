{ config, pkgs, ... }: {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "aparulek";
  home.homeDirectory = "/home/${config.home.username}";

  programs.zsh = {
    shellAliases = {
      hms = "${pkgs.home-manager}/bin/home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixos#linux-work\" --extra-experimental-features \"nix-command flakes\"";
    };
  };
}
