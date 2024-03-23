{ config, lib, ... }: {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "aparulek";
  home.homeDirectory = lib.mkForce "/Users/${config.home.username}";

  programs.kitty.settings = {
    # I need bigger fonts on macOS.
    font_size = "14.0";
    macos_option_as_alt = "yes";
  }
}
