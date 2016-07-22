#!/usr/bin/env zsh

set -e
set -u

function puts () {
  echo "\n-- [$1] $2"
}

function error () {
  echo "$1"
  exit 1
}

function missing () {
  error "\n-- ERROR: $1 is missing, please run OS-specific script"
}

if ! [[ -x $(command -v stow) ]]; then
  missing 'stow'
fi

if ! [[ -d $HOME/.cache/ssh/sockets ]]; then
  mkdir -p $HOME/.cache/ssh/sockets
  puts 'Created' '~/.cache/ssh/sockets'
fi

if ! [[ -d $HOME/.zgen ]]; then
  puts 'Installing' 'zgen'
  command -v git >/dev/null 2>&1 && \
    env git clone https://github.com/tarjoilija/zgen.git $HOME/.zgen >/dev/null 2>&1
fi

if [[ -d $HOME/.zgen ]]; then
  puts 'Installed' 'zgen'
fi

if [[ -f $HOME/.zshrc ]]; then
  mv $HOME/.zshrc $HOME/.zshrc.preinstall
fi

echo $PWD

stow emacs -t $HOME
stow git -t $HOME
stow zsh -t $HOME
stow iterm2 -t $HOME
exit


if [ -d $HOME/.zgen.zsh ]; then
  echo -e "\033[32m  ✔ Found         ❰ zgen ❱   \033[0m"
else
  echo -e "  ➤ Installing    ❰ zgen ❱   \033[0m"


  echo -e "\033[32m    ✔ Installed   ❰ zgen ❱   \033[0m"
fi
