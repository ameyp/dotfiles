#!/bin/bash

if [[ ! -x $(command -v apt-get) ]]; then
  echo "Only debian systems are supported, this script will only create symlinks."
fi

if [[ -f $HOME/.zshrc ]]; then
  mv $HOME/.zshrc $HOME/.zshrc.preinstall
fi

if [[ -f $HOME/.gitconfig ]]; then
  mv $HOME/.gitconfig $HOME/.gitconfig.preinstall
fi

pushd $HOME/.dotfiles

source $HOME/.dotfiles/os-detect.sh

# Install zsh if needed
if ! [[ -x $(command -v zsh) ]]; then
  if isdarwin; then
    "Couldn't find zsh, this shouldn't be possible."
    exit 1
  fi
fi

$INSTALL zsh

zsh ./install.zsh

popd
