#!/usr/bin/env zsh

source ./os-detect.sh

if isdarwin; then
  INSTALL='brew install'
  GUI_INSTALL='brew cask install'
fi

if islinux; then
  INSTALL='sudo apt-get -y install'
  GUI_INSTALL=$INSTALL
fi

if isdarwin; then
  # Install homebrew
  if ! [[ -x $(command -v brew) ]]; then
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
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
fi

if islinux; then
  # For google chrome
  wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo apt-key -y add -

  # Dropbox
  wget https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_2015.10.28_amd64.deb && \
      sudo dpkg -i ./dropbox_2015.10.28_amd64.deb && \
      rm ./dropbox_2015.10.28_amd64.deb

  sudo apt-get update

  eval $INSTALL -f

  eval $INSTALL \
       ack-grep \
       google-chrome-stable \
       silversearcher-ag

  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install --no-update-rc
fi


# Install fzf
isdarwin && /usr/local/opt/fzf/install
islinux && /home/linuxbrew/.linuxbrew/opt/fzf/install

# Create symlinks

function __mkdir { if [[ ! -d $1 ]]; then mkdir -p $1; fi }

function link-file { __mkdir "${2:h}"; rm -rf "$2"; ln -s "$PWD/$1" "$2" }

link-file emacs.d ~/.emacs.d
link-file git/gitconfig ~/.gitconfig
link-file git/gitexcludes ~/.gitexcludes
link-file xmonad ~/.xmonad
link-file ruby/gemrc ~/.gemrc
link-file slate/slate.js ~/.slate.js
link-file zsh/zshrc ~/.zshrc
link-file zsh/plugins ~/.zsh

# Download iterm2 shell integration
curl -L https://iterm2.com/shell_integration/install_shell_integration.sh | bash
