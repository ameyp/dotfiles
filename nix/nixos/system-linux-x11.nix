{ config, pkgs, lib, ... }:

{
  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    # Set display resolution and refresh rate
    # displayManager.setupCommands = ''
    #   xrandr --output DP-4 --mode 2560x1440 --rate 180
    # '';

    # Use Awesome
    displayManager = {
      sddm.enable = true;
      defaultSession = "none+xmonad";
    };

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      # Don't want to rebuild nixos everytime I want to update xmonad.
      # config = builtins.readFile ../../xmonad/xmonad.hs;
      extraPackages = haskellPackages: [ haskellPackages.dbus ];
    };

    # Configure keymap in X11
    layout = "us";
    xkbVariant = "";
  };

  # Enable picom compositor
  services.picom = {
    enable = true;
    # inactiveOpacity = 0.8;
    fade = true;
    fadeDelta = 5;
    backend = "glx";
    settings = {
      corner-radius = 8;
    };
  };

  environment.systemPackages = with pkgs; [
    xorg.xrandr

    # For XMonad
    # for displaying the wallpaper
    feh
    # launcher
    rofi
    # status bar
    polybar
  ];
}
