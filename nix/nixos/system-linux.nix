# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable experimental features
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  networking.hostName = "nixos"; # Define your hostname.
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
  };

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

    # windowManager.awesome = {
    #   enable = true;
    #   luaModules = with pkgs.luaPackages; [
    #     luarocks
    #     luadbi-mysql
    #   ];
    # };

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

  # Enable CUPS to print documents.
  services.printing = {
    enable = true;
    browsing = true;
    drivers = [ pkgs.hplipWithPlugin ];
  };

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    support32Bit = true;
  };

  # Enable sound with pipewire.
  # sound.enable = false;
  # hardware.pulseaudio.enable = false;
  # security.rtkit.enable = true;
  # services.pipewire = {
  #   enable = true;
  #   alsa.enable = true;
  #   alsa.support32Bit = true;
  #   pulse.enable = true;
  #   # If you want to use JACK applications, uncomment this
  #   jack.enable = true;

  #   # use the example session manager (no others are packaged yet so this is enabled by default,
  #   # no need to redefine it in your config for now)
  #   #media-session.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Enable zsh
  programs.zsh.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.amey = {
    isNormalUser = true;
    description = "amey";
    extraGroups = [ "networkmanager" "wheel" "audio" "docker" ];
    packages = with pkgs; [
      chromium
      firefox
      jellyfin-media-player
      lens
      pavucontrol
      tmux
    ];
    shell = pkgs.zsh;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    cryptsetup
    dig
    emacsAmeyWithPackages
    git
    kitty
    nfs-utils
    usbutils
    vim
    xorg.xrandr
    unzip

    # For Thunar
    xfce.thunar
    xfce.thunar-volman

    # For XMonad
    # launcher
    rofi
    # for displaying the wallpaper
    feh
    # status bar
    polybar

    mullvad-vpn
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

  # Load nvidia driver for Xorg and Wayland
  services.xserver.videoDrivers = ["nvidia"];

  hardware.nvidia = {

    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    powerManagement.enable = false;
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
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };

  # containers.test = {
  #   autoStart = true;
  #   privateNetwork = true;

  #   config = { config, pkgs, ... }: {
  #     system.stateVersion = "23.11";

  #     fileSystems."/mnt/cgroup_net_cls" = {
  #       device = "net_cls";
  #       fsType = "cgroup";
  #       options = [ "net_cls" ];
  #     };
  #   };
  # };

  # fileSystems."/mnt/backups" = {
  #   device = "192.168.1.52:/mnt/nas/backups";
  #   fsType = "nfs";
  # };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?
}