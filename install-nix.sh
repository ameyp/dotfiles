#!/bin/bash

set -euo pipefail
set -x

# Install nix

if ! [[ -x $(command -v nix) ]]; then
    curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install --determinate
    . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
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
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
    nix run nix-darwin --extra-experimental-features "nix-command flakes" -- switch --flake "path:$HOME/.dotfiles/nix/nixos#macos"
    nix run home-manager --extra-experimental-features "nix-command flakes" -- \
        switch --flake "path:$HOME/.dotfiles/nix/nixos#macos" \
        --extra-experimental-features "nix-command flakes"
fi
