# Fetch submodules if any
git submodule update --init --recursive

# Check if zsh is installed
which zsh
if [ $? -ne 0 ]; then
    echo "## Install zsh and try again ##"
    exit
fi

# Check if homebrew is installed
if [ `uname` = "Darwin" ]; then
    which brew
    if [ $? -ne 0 ]; then
	echo "## Intalling homebrew ##"
	ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
    fi

    echo "## Installing brew packages from Brewfile ##"
    brew bundle
fi

echo "## Fetching oh-my-zsh ##"
git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.zsh

echo "## Setting zsh as default shell ##"
chsh -s `which zsh`

if [ `uname` = "Darwin" ]; then
    echo "## Downloading Solarized themes ##"
    curl https://raw.githubusercontent.com/altercation/solarized/master/iterm2-colors-solarized/Solarized%20Light.itermcolors -o "/tmp/Solarized Light.itermcolors"
    curl https://raw.githubusercontent.com/altercation/solarized/master/iterm2-colors-solarized/Solarized%20Dark.itermcolors -o "/tmp/Solarized Dark.itermcolors"
    echo "## Installing Solarized themes for iTerm2 ##"
    open "/tmp/Solarized Light.itermcolors" "/tmp/Solarized Dark.itermcolors"
fi

echo "## Setting up soft-links ##"
ln -s $PWD/zshrc ~/.zshrc
ln -s $PWD/zsh-custom ~/.zsh-custom
ln -s $PWD/gitconfig ~/.gitconfig
ln -s $PWD/emacs.d ~/.emacs.d
ln -s $PWD/coffeelintrc ~/.coffeelintrc

echo "## Instructions for necessary manual configuration (if any)##"
if [ `uname` = "Darwin" ]; then
    echo "iTerm2 - Preferences -> Profiles -> Colors -> Load Presets... -> Solarized Dark"
fi

echo "## Done! ##"
