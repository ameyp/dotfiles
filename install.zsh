#!/usr/bin/env zsh

set -euo pipefail

source ./os-detect.sh

if isdarwin; then
  INSTALL='brew install'
  GUI_INSTALL='brew install --cask'
fi

if islinux; then
  INSTALL='sudo apt-get -y install'
  GUI_INSTALL=$INSTALL
fi

if isdarwin; then
  # Install homebrew
  if ! [[ -x $(command -v brew) ]]; then
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    eval "$(/opt/homebrew/bin/brew shellenv)"
  fi

  brew doctor
fi

if islinux; then
  sudo apt-get update
fi

# Install common tools
eval $INSTALL \
     curl \
     git \
     htop \
     tree \
     wget

if isdarwin; then
  eval $INSTALL \
       coreutils \
       ack \
       fzf \
       the_silver_searcher

  brew tap homebrew/cask-fonts
  brew install font-hack
fi

if islinux; then
  eval $INSTALL -f

  eval $INSTALL \
       ack-grep \
       ripgrep

  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install --no-update-rc
fi


# Install fzf
isdarwin && /usr/local/opt/fzf/install

# Create symlinks

function __mkdir { if [[ ! -d $1 ]]; then mkdir -p $1; fi }

function link-file { __mkdir "${2:h}"; rm -rf "$2"; ln -s "$PWD/$1" "$2" }

link-file emacs.d ~/.emacs.d
link-file git/gitconfig ~/.gitconfig
link-file git/gitexcludes ~/.gitexcludes
link-file xmonad ~/.xmonad
link-file ruby/gemrc ~/.gemrc
link-file zsh/zshrc ~/.zshrc
link-file zsh/plugins ~/.zsh

# Download iterm2 shell integration
curl -L https://iterm2.com/shell_integration/install_shell_integration.sh | bash
