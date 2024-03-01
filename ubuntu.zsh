# Install common stuff
sudo apt-get update
sudo apt-get install -y git-core curl zlib1g-dev build-essential libssl-dev libreadline-dev libyaml-dev libsqlite3-dev sqlite3 libxml2-dev libxslt1-dev libcurl4-openssl-dev python-software-properties libffi-dev

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

# Install ruby
git clone git://github.com/sstephenson/rbenv.git $HOME/.rbenv
git clone git://github.com/sstephenson/ruby-build.git $HOME/.rbenv/plugins/ruby-build
rbenv install 2.2.0
rbenv global 2.2.0
