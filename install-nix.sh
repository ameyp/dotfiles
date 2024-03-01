#!/bin/bash

set -euo pipefail

# Install nix

if ! [[ -x $(command -v nix) ]]; then
    curl -L https://nixos.org/nix/install | sh
fi

if ! [[ -x $(command -v home-manager) ]]; then
    # Install Home Manager
    nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
    nix-channel --update

    export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels${NIX_PATH:+:$NIX_PATH}
    nix-shell '<home-manager>' -A install
fi

home-manager switch
