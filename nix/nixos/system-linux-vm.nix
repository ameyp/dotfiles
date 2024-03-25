# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, disko, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration-vm.nix
      ./disko-config-vm.nix
    ];

  networking.hostName = "nixos-vm"; # Define your hostname.

  # Enable docker daemon
  virtualisation.docker.enable = true;
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };

  virtualisation.vmware.guest.enable = true;

  services.tailscale = {
    enable = true;
  };

  services.syncthing = {
    enable = true;
    relay.enable = false;
    systemService = true;
    user = "amey";
    dataDir = "/home/amey";
  };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Without this, no mouse cursor is visible with an Nvidia GPU.
  environment.sessionVariables.WLR_NO_HARDWARE_CURSORS = "1";

  # For now, we need this since hardware acceleration does not work.
  environment.variables.LIBGL_ALWAYS_SOFTWARE = "1";

  hardware.opengl = {
    # enable = lib.mkForce false;
    # driSupport = lib.mkForce false;
    # 32-bit doesn't make sense on an aarch64 machine.
    driSupport32Bit = lib.mkForce false;
  };


  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
