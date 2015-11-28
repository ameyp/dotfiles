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
	ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    fi

    echo “## Running doctor ##”
    brew doctor
    echo "## Installing brew packages ##"
    # brew install emacs --HEAD --use-git-head --cocoa --with-gnutls
    brew install graphicsmagick
    brew install ack

    brew install caskroom/cask/brew-cask

    brew cask install emacs
    brew cask install google-chrome
    brew cask install iterm2
    brew cask install flux
    brew cask install dropbox
    brew cask install vlc
    brew linkapps
fi

echo "## Fetching oh-my-zsh ##"
git clone git://github.com/robbyrussell/oh-my-zsh.git ~/.zsh

echo "## Setting zsh as default shell ##"
chsh -s `which zsh`

if [ `uname` = "Darwin" ]; then
    echo "## Installing Blazer theme for iTerm2 ##"
    open $PWD/iterm2-themes/Blazer.itermcolors

    echo "## Downloading Inconsolata Powerline font ##"
    curl "https://github.com/powerline/fonts/blob/master/InconsolataDz/Inconsolata-dz%20for%20Powerline.otf?raw=true" -o ~/Downloads/Inconsolata-dz-Powerline.otf

    echo "## Installing Inconsolata Powerline font ##"
    open ~/Downloads/Inconsolata-dz-Powerline.otf
    rm ~/Downloads/Inconsolata-dz-Powerline.otf
fi

echo "## Setting up soft-links ##"
ln -s $PWD/zshrc ~/.zshrc
ln -s $PWD/zsh-custom ~/.zsh-custom
ln -s $PWD/gitconfig ~/.gitconfig
ln -s $PWD/emacs.d ~/.emacs.d
ln -s $PWD/coffeelintrc.json ~/.coffeelintrc

echo "## Instructions for necessary manual configuration (if any)##"
if [ `uname` = "Darwin" ]; then
    echo "iTerm2 - Preferences -> Profiles -> Colors -> Load Presets... -> Solarized Dark"
    echo "iTerm2 - Preferences -> Profiles -> Text -> Regular Font -> Change Font -> Inconsolata dz for Powerline (size 13)"
    echo "iTerm2 - Preferences -> Profiles -> Text -> Non-ASCII Font -> Change Font -> Inconsolata dz for Powerline (size 13)"
fi

echo "## Done! ##"
