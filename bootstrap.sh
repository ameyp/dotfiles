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
    brew install the_silver_searcher
    brew install fzf

    # Supposedly not required anymore
    # brew install caskroom/cask/brew-cask

    brew cask install emacs
    brew cask install google-chrome
    brew cask install iterm2
    brew cask install flux
    brew cask install dropbox
    brew cask install vlc
    brew linkapps

    /usr/local/opt/fzf/install
elif [ `uname` = "Linux" ]; then
    if hash apt-get 2>/dev/null; then
        # Install common stuff
        sudo apt-get install build-essential
        sudo apt-get install zsh git

        # Install SciPy dependencies
        sudo apt-get install libblas-dev liblapack-dev libatlas-base-dev gfortran

        # Install pyenv
        curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash

        # Install python 3.5.0
        env PYTHON_CONFIGURE_OPTS="--enable-shared" pyenv install 3.5.0

        # Install emacs
        sudo apt-get build-dep emacs24
        pushd /tmp
        wget http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.xz
        tar -xf emacs-24.5.tar.xz
        pushd emacs-24.5
        ./configure
        make
        sudo make install
        popd
        
        # Install cuda
        wget http://developer.download.nvidia.com/compute/cuda/7.5/Prod/local_installers/cuda-repo-ubuntu1404-7-5-local_7.5-18_amd64.deb
        sudo dpkg -i cuda-repo-ubuntu1404-7-5-local_7.5-18_amd64.deb
        sudo apt-get update
        sudo apt-get install cuda
        popd
    fi
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
elif [ `uname` = "Linux" ]; then
    echo "## Install Solarized Dark ##"
    pushd /tmp
    git clone https://github.com/Anthony25/gnome-terminal-colors-solarized
    pushd gnome-terminal-colors-solarized
    ./install.sh
    popd
    popd

    echo "## Downloading Inconsolata Powerline font ##"
    mkdir ~/.fonts
    pushd ~/.fonts
    wget "https://github.com/powerline/fonts/blob/master/InconsolataDz/Inconsolata-dz%20for%20Powerline.otf?raw=true"
    fc-cache -f -v
    popd
fi

echo "## Setting up soft-links ##"
ln -s $PWD/zshrc ~/.zshrc
ln -s $PWD/zsh-custom ~/.zsh-custom
ln -s $PWD/gitconfig ~/.gitconfig
ln -s $PWD/gitexcludes ~/.gitexcludes
ln -s $PWD/emacs.d ~/.emacs.d
ln -s $PWD/scripts ~/bin

echo "## Instructions for necessary manual configuration (if any)##"
if [ `uname` = "Darwin" ]; then
    echo "iTerm2 - Preferences -> Profiles -> Colors -> Load Presets... -> Solarized Dark"
    echo "iTerm2 - Preferences -> Profiles -> Text -> Regular Font -> Change Font -> Inconsolata dz for Powerline (size 13)"
    echo "iTerm2 - Preferences -> Profiles -> Text -> Non-ASCII Font -> Change Font -> Inconsolata dz for Powerline (size 13)"
fi

echo "## Done! ##"
