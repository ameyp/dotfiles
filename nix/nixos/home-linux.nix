{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "amey";
  home.homeDirectory = "/home/${config.home.username}";

  targets.genericLinux.enable = true;

  # Only supported on linux.
  # TODO delete once I'm happy with my age setup.
  # services.gpg-agent = {
  #   enable = true;
  #   pinentryFlavor = "curses";
  #   extraConfig = ''
  #     allow-emacs-pinentry
  #     allow-loopback-pinentry
  #   '';
  # };
}
