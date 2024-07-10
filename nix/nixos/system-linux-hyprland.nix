{ config, pkgs, lib, ... }:

{
  imports =
    [
      ./greetd.nix
    ];

  # Enable dconf
  # Started encountering this after adding a theme to home-manager.
  # https://github.com/nix-community/home-manager/issues/3113
  programs.dconf.enable = true;

  environment.systemPackages = with pkgs; [
    android-studio
  ];

  xdg.portal = {
    enable = true;
    config = {
      common = {
        default = [ "gtk" ];
      };
    };
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];
  };
}
