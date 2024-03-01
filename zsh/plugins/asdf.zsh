if [[ -e $HOME/.asdf/asdf.sh ]]; then
  source $HOME/.asdf/asdf.sh
fi

if [[ -f $HOMEBREW_BINARY ]] && [[ -e $(brew --prefix asdf)/libexec/asdf.sh ]]; then
  source $(brew --prefix asdf)/libexec/asdf.sh
fi
