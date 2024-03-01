{ config, pkgs, ... }:

{
  home.homeDirectory = "/Users/amey";
  home.packages = [
    # Disabled because of https://github.com/NixOS/nixpkgs/issues/127902
    # pkgs.emacsMacport
    pkgs.emacs
  ];
}
