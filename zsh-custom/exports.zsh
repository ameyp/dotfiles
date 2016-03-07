export PATH=$HOME/bin:~/Applications/Emacs.app/Contents/MacOS/bin:/usr/local/smlnj/bin:/usr/local/bin:$PATH
export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Editor for git commits
export GIT_EDITOR='emacsclient -c'

# Preferred editor for local and remote sessions
export EDITOR='emacsclient -c'

# Pyenv shims
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# Use the_silver_searcher with fzf
_fzf_compgen_path() {
  ag -g "" "$1"
}