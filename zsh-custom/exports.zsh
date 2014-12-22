export PATH=$HOME/bin:/Applications/Emacs.app/Contents/MacOS/bin:/usr/local/smlnj/bin:/usr/local/bin:$PATH
export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Editor for git commits
export GIT_EDITOR='emacsclient -c'

# Preferred editor for local and remote sessions
export EDITOR='emacsclient -c'

# Add GHC 7.8.3 to the PATH, via http://ghcformacosx.github.io/
export GHC_DOT_APP="/Applications/GHC.app"
if [[ ! -z $GHC_DOT_APP ]] then
    export PATH="${HOME}/.cabal/bin:${GHC_DOT_APP}/Contents/bin:${PATH}"
fi
