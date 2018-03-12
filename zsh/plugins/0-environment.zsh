# Set editor.
export EDITOR='emacsclient -c'

# Set git editor.
export GIT_EDITOR='emacsclient -c'

# Set Base16 Shell path.
if [[ -z $BASE16_SHELL ]]; then
  export BASE16_SHELL=$HOME/.zgen/chriskempson/base16-shell-master
fi

# Set Base16 theme.
if [[ -z $BASE16_THEME ]]; then
  export BASE16_THEME=default
fi

# Set Base16 background type.
if [[ -z $BASE16_TYPE ]]; then
  export BASE16_TYPE=dark
fi

# XDG Base Directory Specification.
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
export XDG_CONFIG_DIRS=/etc/xdg
export XDG_CACHE_HOME=$HOME/.cache

# Set fzf default command.
if [[ -x $(command -v fzf) && \
      -x $(command -v ag) ]]; then
  export FZF_DEFAULT_COMMAND='(ag -g "")'
fi

# Set ssh-agent socket.
if [[ -S $XDG_RUNTIME_DIR/ssh-agent.socket ]]; then
  export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
fi

# Add local binaries to path.
if [[ -d $HOME/.local/bin ]]; then
  PATH=$HOME/.local/bin:$PATH
fi

# Add npm installed binaries to path.
if [[ -d $HOME/.npm/bin ]]; then
  PATH=$HOME/.npm/bin:$PATH
fi

# Add emacs installed binaries to path.
if [[ -d $HOME/Applications/Emacs.app ]]; then
  PATH=$HOME/Applications/Emacs.app/Contents/MacOS/bin:$PATH
fi

# Activate torch if available.
if [[ -d $HOME/Applications/torch ]]; then
  . $HOME/Applications/torch/install/bin/torch-activate
fi

# Add cuda installed binaries and libraries to paths.
if [[ -d /usr/local/cuda-7.5 ]]; then
  export CUDA_ROOT=/usr/local/cuda-7.5
  export CUDA_HOME=/usr/local/cuda-7.5
  PATH=$CUDA_ROOT:/bin/:$PATH
  export LD_LIBRARY_PATH=$CUDA_ROOT/lib64
fi

# Enable iTerm2 shell integration.
if [[ -d $HOME/.iterm2_shell_integration ]]; then
  source ~/.iterm2_shell_integration.`basename $SHELL`
fi
