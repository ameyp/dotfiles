{ config, pkgs, lib, ... }:
{
  # Without this, no mouse cursor is visible with an Nvidia GPU.
  environment.sessionVariables.WLR_NO_HARDWARE_CURSORS = "1";

  # Login greeter
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --remember --remember-session --sessions ${pkgs.hyprland}/share/wayland-sessions";
        user = "greeter";
      };
    };
  };

  # Custom session file for setting environment variables
  systemd.services.greetd.serviceConfig = {
    Type = "idle";
    StandardInput = "tty";
    StandardOutput = "tty";
    # Without this, errors will spam on screen
    StandardError = "journal";
    # Without these, bootlogs will spam on screen
    TTYReset = true;
    TTYVHangup = true;
    TTYVTDisallocate = true;
  };
}
