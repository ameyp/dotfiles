{ config, lib, ... }: {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "aparulek";
  home.homeDirectory = lib.mkForce "/Users/${config.home.username}";
}
