# Install homebrew
ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"

# Install brew packages from Brewfile
brew bundle

# Get oh-my-zsh
git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.zsh

# Set up softlinks
ln -s $PWD/zshrc ~/.zshrc
ln -s $PWD/zsh-custom ~/.zsh-custom
ln -s $PWD/gitconfig ~/.gitconfig
ln -s $PWD/emacs.d ~/.emacs.d
