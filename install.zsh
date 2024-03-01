#!/usr/bin/env zsh

set -euo pipefail

source ./os-detect.sh

if isdarwin; then
  # Install homebrew
  if ! [[ -x $(command -v brew) ]]; then
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
  fi

  brew doctor
fi

# Create symlinks

function __mkdir { if [[ ! -d $1 ]]; then mkdir -p $1; fi }

function link-file { __mkdir "${2:h}"; rm -rf "$2"; ln -s "$PWD/$1" "$2" }

link-file emacs.d ~/.emacs.d
link-file xmonad ~/.xmonad
link-file ruby/gemrc ~/.gemrc
link-file zsh/zshrc ~/.zshrc
link-file zsh/plugins ~/.zsh
link-file nix/config ~/.config/nix/nix.conf
link-file nix/home.nix ~/.config/nixpkgs/home.nix

# Download iterm2 shell integration
curl -L https://iterm2.com/shell_integration/install_shell_integration.sh | bash
