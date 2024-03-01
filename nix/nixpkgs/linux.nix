{ config, pkgs, ... }:

{
  home.homeDirectory = "/home/amey";
  home.packages = [
    pkgs.emacs-gtk
  ];

  programs.zsh = {
    shellAliases = {
      hms = "home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixpkgs#amey@linux\"";
    };
  };

  fonts.fontconfig.enable = true;
  targets.genericLinux.enable = true;
  xdg.mime.enable = true;
}
