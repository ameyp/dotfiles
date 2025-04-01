{ config, lib, pkgs, ... }: let
  hms = "${pkgs.home-manager}/bin/home-manager switch --flake \"path:$HOME/.dotfiles/nix/nixos#macos-work\"";
  nds = "darwin-rebuild switch --flake \"$HOME/.dotfiles/nix/nixos#macos-work\"";
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = lib.mkForce "aparulek";
  home.homeDirectory = lib.mkForce "/Users/${config.home.username}";

  services.syncthing = {
    enable = lib.mkForce false;
  };

  programs.zsh = {
    shellAliases = {
      hms = lib.mkForce hms;
      nds = lib.mkForce nds;
    };
  };

  programs.fish = {
    shellAbbrs = {
      hms = lib.mkForce hms;
      nds = lib.mkForce nds;
    };
  };

  programs.kitty.settings = {
    # I need bigger fonts on macOS.
    font_size = "14.0";
    macos_option_as_alt = "yes";
  };
}
