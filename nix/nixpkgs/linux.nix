{ config, pkgs, ... }:

{
  home.homeDirectory = "/home/amey";
  home.packages = [
    pkgs.emacs-gtk
  ];
}
