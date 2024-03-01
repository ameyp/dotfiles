#!/bin/bash

set -euo pipefail

# Install nix

if ! [[ -x $(command -v nix) ]]; then
    curl -L https://nixos.org/nix/install | sh
    . $HOME/.nix-profile/etc/profile.d/nix.sh
fi

. /home/amey/.nix-profile/etc/profile.d/nix.sh

if ! [[ -x $(command -v home-manager) ]]; then
    # Install Home Manager
    nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
    nix-channel --update

    nix run home-manager/master -- init
fi

# Fix for https://github.com/nix-community/home-manager/issues/3734
mkdir -m 0755 -p /nix/var/nix/{profiles,gcroots}/per-user/$USER

# Fix for a conflict for zsh that's been around forever
sudo rm -rf sudo rm -rf /nix/store/ny9r65799s7xhp605bc2753sjvzkxrrs-nix-2.15.1/share/zsh/site-functions/_nix

rm -f $HOME/.bashrc $HOME/.profile

if [[ $(uname) == "linux" ]]; then
    home-manager switch --flake "path:$HOME/.dotfiles/nix/nixpkgs#amey@linux" --extra-experimental-features "nix-command flakes"
else
    home-manager switch --flake "path:$HOME/.dotfiles/nix/nixpkgs#amey@macos" --extra-experimental-features "nix-command flakes"
fi
