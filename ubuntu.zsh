# Install common stuff
sudo apt-get install build-essential
sudo apt-get install zsh git

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
