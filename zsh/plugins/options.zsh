# Create cache directory.
mkdir -p $XDG_CACHE_HOME/zsh

# Set history file.
HISTSIZE=2000
HISTFILE=$XDG_CACHE_HOME/zsh/history

# Do not add commands with leading space to history.
setopt histignorespace
unsetopt CORRECT

# append history list to the history file; this is the default but we make sure
# because it's required for share_history.
setopt inc_append_history

# import new commands from the history file also in other zsh-session
setopt share_history

# save each command's beginning timestamp and the duration to the history file
setopt extended_history

# extended globbing patterns
setopt extendedglob

# If a new command line being added to the history list duplicates an older
# one, the older command is removed from the list
setopt histignorealldups

# not just at the end
setopt completeinword

# Don't send SIGHUP to background processes when the shell exits.
setopt nohup

# make cd push the old directory onto the directory stack.
setopt auto_pushd

# avoid "beep"ing
setopt nobeep

# don't push the same dir twice.
setopt pushd_ignore_dups

# use zsh style word splitting
setopt noshwordsplit

# Autocompletion
autoload compinit
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
compinit

# What zsh identifies as a word delimiter
autoload -U select-word-style
select-word-style bash
