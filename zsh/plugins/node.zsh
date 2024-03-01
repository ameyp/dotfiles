if [[ -f ~/.config/nvm.sh ]]; then
    function init_nvm() {
        export NVM_DIR="$HOME/.config"
        source $NVM_DIR/nvm.sh
    }
fi
