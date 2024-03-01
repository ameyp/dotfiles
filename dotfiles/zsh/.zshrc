if [[ -f $HOME/.zshrc.preinstall ]]; then
  source $HOME/.zshrc.preinstall
fi

source "${HOME}/.zgen/zgen.zsh"

if ! zgen saved; then
  echo "Creating a zgen save".

  # Clone Base16 Shell.
  zgen clone chriskempson/base16-shell master

  # Load Prezto.
  zgen prezto

  # Load a theme
  zgen prezto prompt theme 'peepcode'

  # Prezto plugins to load.
  prezto_plugins=(
    completion
    git
    node
    python
    syntax-highlighting
  )

  # Load Prezto plugins.
  for plugin in $prezto_plugins; do
    zgen prezto $plugin
  done

  # Load this plugin.
  zgen pmodule ameyp/zshrc master

  zgen save
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

if [[ -f $HOME/.zsh-extra ]]; then
  source $HOME/.zsh-extra
fi
