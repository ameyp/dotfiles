autoload -Uz add-zsh-hook
autoload -U colors && colors
autoload -Uz vcs_info

zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git*' stagedstr '%f'
zstyle ':vcs_info:git*' unstagedstr '%f'
zstyle ':vcs_info:git*' formats '%b (%a) %m%u%c'

fn_hostname () {
    full_hostname=$(hostname)
    psvar[1]=$full_hostname[(ws:.:)1]
}

fn_prompt_color () {
    psvar[1]=%(?.$fg[green].$fg[red])
}

fn_prompt_vcsinfo () {
    vcs_info
    psvar[1]=${vcs_info_msg_0_}
}

add-zsh-hook precmd fn_prompt_vcsinfo
setopt prompt_subst

# %m is the first part of the hostname.
# %~ uses bookmarks if available, $HOME otherwise.
PROMPT="
[%m] %~ %v
%(?.%{$fg[green]%}.%{$fg[red]%})>%{$reset_color%} "
