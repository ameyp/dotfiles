## Setup

Clone the repo:
    git clone https://github.com/ameyp/dotfiles

Change directory to where you cloned the repo
    cd dotfiles

Run the bootstrap script
    sh install.sh

## Restore GPG private key

```bash
# Restore the key
gpg --import-options restore --import ~/Downloads/secret-backup.gpg
# Trust the key
gpg --edit-key amey@wirywolf.com
trust
# Ultimate trust
5
quit
gpgconf --kill all
```

## Todo

- Implement `command_not_found_handler` to auto-install (whitelisted? all?) commands via the system's package manager on first use.
