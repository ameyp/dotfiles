echo "Creating symlinks"

if [[ -f $HOME/.zshrc ]]; then
  mv $HOME/.zshrc $HOME/.zshrc.preinstall
fi

if [[ -f $HOME/.gitconfig ]]; then
  mv $HOME/.gitconfig $HOME/.gitconfig.preinstall
fi

pushd dotfiles
for dir in `ls`; do
  stow $dir -t $HOME
done
popd
