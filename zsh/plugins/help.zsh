function zshrc () {
  echo 'Quick reference for zshrc
  [General]
  - Leading any command with a space will not record it to history.

  [Binds]
  - ^A and ^E move to beginning or end of line.
  - ^N and ^P navigate command history.

  [Aliases]
  - cb will xclip a file to the clipboard.
  - gfup is git fetch --no-tags upstream.

  [Functions]
  - Shortcuts to find:
    - ff finds by name
    - fa finds by *name*
    - fs finds by $name*
    - fe finds by *$name
  - tarz will create a tar archive.
  - Shortcuts for git:
    - ghcl user/repo clones from GitHub
    - gitsed runs a sed command on all tracked files
    - mknew will setup a new makenew project
  - sslgen-cert-test generates a dummy SSL certificate.
  - zshupg upgrades zshrc.
  '
}
