{ config, pkgs, lib, disko, ... }:

{
  imports =
    [
      ./hardware-configuration-thinkpad.nix
      ./disko-config-thinkpad.nix
    ];

  networking.hostName = "nixos-thinkpad"; # Define your hostname.

  services.tailscale = {
    enable = true;
  };

  environment.systemPackages = with pkgs; [
  ];

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  services.syncthing = {
    enable = true;
    relay.enable = false;
    systemService = true;
    user = "amey";
  };

  services.safeeyes.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
