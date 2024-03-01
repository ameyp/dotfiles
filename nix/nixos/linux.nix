{ config, pkgs, ... }:

{
  home.packages = [
    pkgs.betterlockscreen
    pkgs.emacs-gtk
    pkgs.xss-lock
  ];

  programs.zsh = {
    shellAliases = {
      hms = "home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixpkgs#linux\" --extra-experimental-features \"nix-command flakes\"";
    };
  };

  fonts.fontconfig.enable = true;
  targets.genericLinux.enable = true;
  xdg.mime.enable = true;

  # Lockscreen
  # Initialize it with images by running
  # betterlockscreen -b /path/to/folder/or/image
  services.screen-locker = {
    enable = true;
    lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen -l blur";
  };
}
