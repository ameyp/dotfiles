if ! [[ -x $(command -v brew) ]]; then
  echo "Installing homebrew"
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

brew doctor
brew install stow
brew install zsh
brew install ack
brew install the_silver_searcher
brew install fzf

brew cask install emacs
brew cask install google-chrome
brew cask install hammerspoon
brew cask install iterm2
brew cask install flux
brew cask install dropbox
brew cask install vlc
brew cask install spotify
brew linkapps

# /usr/local/opt/fzf/install
