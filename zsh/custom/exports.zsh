export PATH=$HOME/bin:~/Applications/Emacs.app/Contents/MacOS/bin:/usr/local/smlnj/bin:/usr/local/bin:$PATH
export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Editor for git commits
export GIT_EDITOR='emacsclient -c'

# Preferred editor for local and remote sessions
export EDITOR='emacsclient -c'

# Pyenv shims
if [[ -d $HOME/.pyenv ]]; then
    export PATH="$HOME/.pyenv/bin:$PATH"
fi
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# Activate fzf
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# Use the_silver_searcher with fzf
_fzf_compgen_path() {
  ag -g "" "$1"
}

# Activate torch
if [[ -a /Users/amey/Applications/torch/install/bin/torch-activate ]]; then
    . /Users/amey/Applications/torch/install/bin/torch-activate
fi

# Paths for CUDA
if [[ -d /usr/local/cuda-7.5 ]]; then
    export PATH="/usr/local/cuda-7.5/bin/:$PATH"
    export LD_LIBRARY_PATH=/usr/local/cuda-7.5/lib64
    export CUDA_ROOT=/usr/local/cuda-7.5
fi

# Enable iterm2 shell integration
source ~/.iterm2_shell_integration.`basename $SHELL`