if type "pyenv" > /dev/null; then
    export PATH=$(pyenv root)/shims:$PATH

    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi
