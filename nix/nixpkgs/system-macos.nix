{ pkgs, config, ... }: {

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages =
    [
    ];

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
      "d12frosted/emacs-plus"
    ];
    brews = [
      {
        # Must run manually after installation:
        # ln -s /opt/homebrew/Cellar/emacs-plus@30/30.1/Emacs.app /Applications
        name = "emacs-plus@29";
        args = [ "with-imagemagick" "with-native-comp" ];
      }
    ];
  };

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;
  # nix.package = pkgs.nix;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Create /etc/zshrc that loads the nix-darwin environment.
  programs.zsh.enable = true;  # default shell on catalina

  # Set Git commit hash for darwin-version.
  system.configurationRevision = config.rev or config.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";
}
