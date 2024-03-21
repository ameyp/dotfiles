{ config, pkgs, lib, ... }:

{
  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable experimental features
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager = {
    enable = true;
  };
  # Enabling tailscale triggers https://github.com/NixOS/nixpkgs/issues/180175
  # Adding the tailscale interface to unmanaged does nothing, so I'm just disabling
  # the service that probably waits for all interfaces to be up.
  systemd.services.NetworkManager-wait-online.enable = lib.mkForce false;

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable zsh
  programs.zsh.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.amey = {
    isNormalUser = true;
    description = "amey";
    extraGroups = [ "networkmanager" "wheel" "audio" "docker" ];
    packages = with pkgs; [
    ];
    shell = pkgs.zsh;
  };

  xdg.mime.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    chromium
    cryptsetup
    dig
    emacsAmeyWithPackages
    firefox
    git
    mullvad-vpn
    nfs-utils
    tmux
    unzip
    usbutils
    vim

    # For Thunar
    xfce.thunar
    xfce.thunar-volman
  ];

  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
    thunar-volman
  ];

  # To safe preferences for thunar since I'm not using xfce as a desktop.
  programs.xfconf.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  services.devmon.enable = true; # Automount devices
  services.udisks2.enable = true; # DBus service that allows applications to query and manipulate storage devices
  services.gvfs.enable = true; # Mount, trash and other functionalities
  services.tumbler.enable = true; # Thumbnail support for images

  services.syncthing = {
    enable = true;
    relay.enable = false;
    systemService = true;
    user = "amey";
  };

  services.safeeyes.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable OpenGL
  hardware.opengl = {
    enable = true;
    driSupport = true;
    driSupport32Bit = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}
