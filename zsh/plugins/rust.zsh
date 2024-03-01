# Add cargo to path if available.
if [[ -d $HOME/.cargo/bin ]]; then
  PATH=$HOME/.cargo/bin:$PATH
fi
