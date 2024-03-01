#!/usr/bin/env zsh

source ./os-detect.sh

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

eval $GUI_INSTALL \
     emacs \
     firefox \
     vlc

if isdarwin; then
  eval $INSTALL \
       ack \
       fzf \
       the_silver_searcher

  eval $GUI_INSTALL \
       dropbox \
       flux \
       google--chrome \
       iterm2 \
       slate \
       spotify
fi

if islinux; then
  # For spotify
  sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
       --recv-keys 0DF731E45CE24F27EEEB1450EFDC8610341D9410
  echo deb http://repository.spotify.com stable non-free | sudo tee /etc/apt/sources.list.d/spotify.list

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
       silversearcher-ag \
       spotify-client

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
link-file ruby/gemrc ~/.gemrc
link-file slate/slate.js ~/.slate.js
link-file zsh/zshrc ~/.zshrc
link-file zsh/plugins ~/.zsh

# Download iterm2 shell integration
curl -L https://iterm2.com/shell_integration/zsh \
-o ~/.iterm2_shell_integration.zsh
