# Add ruby-build to path if available.
if [[ -d $HOME/.rbenv/plugins/ruby-build ]]; then
  PATH=$HOME/.rbenv/plugins/ruby-build/bin:$PATH
fi