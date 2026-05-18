{ config, lib, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = lib.mkDefault "amey";
  home.homeDirectory = lib.mkDefault "/home/${config.home.username}";
  home.packages = [
    pkgs.emacsAmeyWithPackages
  ];
}
