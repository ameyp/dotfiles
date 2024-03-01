if ! [[ -d $HOME/.linuxbrew ]]; then
  echo "Installing homebrew"
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Linuxbrew/install/master/install)"
fi

brew doctor
brew install stow