# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  networking.hostName = "nixos-desktop"; # Define your hostname.

  # Add wirywolf-media user and group so that I can access NFS shares from my desktop.
  users.groups.wirywolf-media = {
    gid = 5001;
  };
  users.users.wirywolf-media = {
    uid = 5001;
    isNormalUser = true;
    description = "wirywolf-media";
    extraGroups = [ "wirywolf-media" ];
    shell = pkgs.zsh;
  };

  # Enable docker daemon
  virtualisation.docker.enable = true;
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };

  # Enable avahi for local network name resolution
  services.avahi = {
    enable = true;
    nssmdns4 = true;
  };

  services.tailscale = {
    enable = true;
    extraUpFlags = [
      "--hostname=nixos-desktop"
    ];
  };

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    browsing = true;
    drivers = [ pkgs.brlaser ];
  };

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    devenv
    discord
    jellyfin-media-player
    lens
    pavucontrol
  ];

  # List services that you want to enable:

  services.devmon.enable = true; # Automount devices
  services.udisks2.enable = true; # DBus service that allows applications to query and manipulate storage devices
  services.gvfs.enable = true; # Mount, trash and other functionalities
  services.tumbler.enable = true; # Thumbnail support for images

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {
    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    # powerManagement.enable = false;

    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of
    # supported GPUs is at:
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    # Only available from driver 515.43.04+
    # Currently alpha-quality/buggy, so false is currently the recommended setting.
    open = false;

    # Enable the Nvidia settings menu,
    # accessible via `nvidia-settings`.
    nvidiaSettings = true;

    # Optionally, you may need to select the appropriate driver version for your specific GPU.
    package = config.boot.kernelPackages.nvidiaPackages.beta;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
