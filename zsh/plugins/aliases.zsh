# Always display colors.
alias ls='ls -G'

# ack is ack-grep on some systems.
[[ -x $(command -v ack-grep) ]] && alias ack='ack-grep'

# Alias for emacsclient
alias ec='emacsclient -c'

# Copy to clipboard with xclip.
[[ -x $(command -v xclip ) ]] && alias cb='xclip -selection clipboard'

# npm aliases.
if [[ -x $(command -v npm) ]]; then
  alias npmi='npm install --save'
  alias npmid='npm install --save-dev'
  alias npmu='npm uninstall --save'
  alias npmud='npm uninstall --save-dev'
  alias npmsw='npm shrinkwrap'
  alias npmswd='npm shrinkwrap -dev'
  alias npmrm='rm -rf node_modules'
fi

# Convenient alias to lock the mac.
[[ -x $(command -v pmset) ]] && alias lock='pmset displaysleepnow'

# tail on steroids!
# Ctrl-C to pause scrolling and navigate
# F while paused to resume
alias trail="less +F -n"
