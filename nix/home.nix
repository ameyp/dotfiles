{ config, pkgs, ... }:

let attrs = {
  equalsCurrentSystem = x : x == builtins.currentSystem;
  isMacOS = if builtins.any attrs.equalsCurrentSystem [ "x86_64-darwin" "aarch64-darwin" ] then true else false;
  macOSPackages = [ pkgs.emacsMacport ];
  linuxPackages = [ pkgs.emacs-gtk ];
};
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "amey";
  home.homeDirectory = if attrs.isMacOS then "/Users/amey" else "/home/amey";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Packages to install
  home.packages = [
    pkgs.curl
    pkgs.fzf
    pkgs.git
    pkgs.htop
    pkgs.ripgrep
    pkgs.starship
    pkgs.tree
    pkgs.wget
  ] ++ (if attrs.isMacOS then attrs.macOSPackages else attrs.linuxPackages);
}