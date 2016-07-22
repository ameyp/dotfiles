#!/usr/bin/env zsh

echo -e "\033[32m➤ Installing!   \033[0m"

command -v git >/dev/null 2>&1 \
  && echo -e "\033[32m  ✔ Found         ❰ Git ❱   \033[0m" \
  || {
    echo -e "\033[31m  ✘ Missing       ❰ Git ❱   \033[0m"
    echo -e "\033[31m✘ Install failed!"
    exit 1
  }

if [ -d $HOME/.dotfiles ]; then
  echo -e "\033[32m  ✔ Found         ❰ dotfiles ❱   \033[0m"
else
  echo -e "  ➤ Installing    ❰ dotfiles ❱   \033[0m"

  command -v git >/dev/null 2>&1 && \
    env git clone https://github.com/ameyp/dotfiles $HOME/.dotfiles >/dev/null 2>&1

  echo -e "\033[32m    ✔ Installed   ❰ dotfiles ❱   \033[0m"
fi

pushd $HOME/.dotfiles
source ./bootstrap.zsh
popd
