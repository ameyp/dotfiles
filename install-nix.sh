#!/bin/bash

set -euo pipefail
set -x

# Install nix

if ! [[ -x $(command -v nix) ]]; then
    curl -L https://nixos.org/nix/install | sh
    . $HOME/.nix-profile/etc/profile.d/nix.sh
fi

if ! [[ -x $(command -v home-manager) ]]; then
    # Install Home Manager
    nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
    nix-channel --update

    nix run home-manager/master --extra-experimental-features "nix-command flakes" -- init
fi

# Fix for a conflict for zsh that's been around forever
# The path changes based on nix version.
# sudo rm -rf /nix/store/ny9r65799s7xhp605bc2753sjvzkxrrs-nix-2.15.1/share/zsh/site-functions/_nix

rm -f $HOME/.bashrc $HOME/.profile

if [[ $(uname) == "linux" || $(uname) == "Linux" ]]; then
    nix run home-manager --extra-experimental-features "nix-command flakes" -- \
        switch --flake "path:$HOME/.dotfiles/nix/nixos#linux" \
        --extra-experimental-features "nix-command flakes"
else
    nix run home-manager --extra-experimental-features "nix-command flakes" -- \
        switch --flake "path:$HOME/.dotfiles/nix/nixos#macos" \
        --extra-experimental-features "nix-command flakes"
fi
