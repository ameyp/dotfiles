{ pkgs, config, lib, ... }: let
  username = "aparulek";
in {

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # ((emacsPackagesFor emacs).emacsWithPackages (
    #   epkgs: with epkgs; [
    #   ]
    # ))
    emacsAmeyWithPackages
  ];

  # In order to add custom DNS servers using scutil, create a file with the following content:
  # open
  # d.init
  # d.add ServerAddresses * 192.168.1.1 8.8.8.8
  # quit
  #
  # Then run sudo scutil < filename

  homebrew = {
    enable = true;
    casks = [
      # sudo xattr -r -d com.apple.quarantine /Applications/Stretchly.app
      "alfred"
      "iterm2"
      "stretchly"
    ];
    # Did not work for stretchly
    # caskArgs = {
    #   no_quarantine = true;
    # };
    taps = [
    ];
    brews = [
    ];
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Necessary for netskope.
  # nix.settings.ssl-cert-file = "/Library/Application Support/Netskope/STAgent/download/nscacert_combined.pem";

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina

  # Set Git commit hash for darwin-version.
  system.configurationRevision = config.rev or config.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # Nix-darwin does not link installed applications to the user environment. This means apps will not show up
  # in spotlight, and when launched through the dock they come with a terminal window. This is a workaround.
  # Upstream issue: https://github.com/LnL7/nix-darwin/issues/214
  system.activationScripts.applications.text = lib.mkForce ''
    echo "setting up ~/Applications..." >&2
    applications="/Users/${username}/Applications"
    nix_apps="$applications/Nix Apps"

    # Needs to be writable by the user so that home-manager can symlink into it
    if ! test -d "$applications"; then
        mkdir -p "$applications"
        chown ${username}: "$applications"
        chmod u+w "$applications"
    fi

    # Delete the directory to remove old links
    rm -rf "$nix_apps"
    mkdir -p "$nix_apps"
    find ${config.system.build.applications}/Applications -maxdepth 1 -type l -exec readlink '{}' + |
        while read src; do
            ln -sf "$src" "$nix_apps"
        done
  '';

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
