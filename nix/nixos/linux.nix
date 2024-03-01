{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.emacs-gtk
  ];

  programs.zsh = {
    shellAliases = {
      hms = "home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixpkgs#linux\" --extra-experimental-features \"nix-command flakes\"";
    };
  };

  fonts.fontconfig.enable = true;
  targets.genericLinux.enable = true;
  xdg.mime.enable = true;
}
