{ config, pkgs, ... }:

let attrs = {
};
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "amey";
  home.homeDirectory = "/home/amey";

  # Enable font management
  fonts.fontconfig.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  programs.home-manager.path = "$HOME/Applications/home-manager";

  # Packages to install
  home.packages = [
    (pkgs.nerdfonts.override { fonts = [ "Hack" ]; })
    pkgs.curl
    pkgs.direnv
    pkgs.fzf
    pkgs.git
    pkgs.htop
    pkgs.ripgrep
    pkgs.starship
    pkgs.tree
    pkgs.wget
  ];

  # Enable direnv
  # https://github.com/nix-community/nix-direnv
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  # Git config
  programs.git = {
    enable = true;
    userName = "Amey Parulekar";
    userEmail = "amey@wirywolf.com";
    extraConfig = {
      core = {
        fileMode = false;
        pager = "less -FMRiX";
        editor = "emacsclient -c";
      };
      push = {
        default = "current";
      };
      remote = {
        pushDefault = "origin";
      };
      color = {
        ui = "auto";
      };
      credential = {
        helper = "cache";
      };
      pull = {
        rebase = true;
      };
      init = {
        defaultBranch = "main";
      };
    };
    aliases = {
      dag = "log --graph --format='format:%C(yellow)%h%C(reset) %C(blue)\\\"%an\\\" <%ae>%C(reset) %C(magenta)%ar%C(reset)%C(auto)%d%C(reset)%n%s' --date-order";
      co = "checkout";
      st = "status";
      rb = "rebase";
      br = "branch";
    };
    ignores = [
      "*.~undo-tree~"
      "*.iml"
      "*.pyc"
      "*.class"
      "*.bak"
      "build"
      ".DS_Store"
      ".rakeTasks"
      "._.DS_Store"
      ".classpath"
      ".gradle"
      ".project"
      "eclipse-bin"
      ".bemol"
      ".settings"
      ".solargraph.yml"
      ".direnv"
      ".idea"
    ];
  };

  # ZSH
  # Disabled due to https://github.com/nix-community/home-manager/issues/2995
  # https://nix-community.github.io/home-manager/options.html#opt-programs.zsh.enable

  programs.zsh = let zsh_attrs = {
    linuxEnv = ''
    '';
    macOSEnv = ''
      HOMEBREW_BINARY=/opt/homebrew/bin/brew
      [[ -f $HOMEBREW_BINARY ]] && eval "$($HOMEBREW_BINARY shellenv)"

    '';
  }; in {
    enable = true;
    defaultKeymap = "emacs";
    history = {
      extended = true;
      ignoreDups = true;
      ignoreSpace = true;
      path = "$XDG_CACHE_HOME/zsh/history";
      save = 10000;
      size = 10000;
      share = true;
    };
    completionInit = ''
      autoload compinit
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
      compinit
    '';
    envExtra = ''
      export JAVA_TOOLS_OPTIONS="-Dlog4j2.formatMsgNoLookups=true"

      # Nix
      NIX_PROFILE="$HOME/.nix-profile/etc/profile.d/nix.sh"
      [ -f "$NIX_PROFILE" ] && source "$NIX_PROFILE"

      export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels

      # XDG Base Directory Specification.
      export XDG_DATA_HOME=$HOME/.local/share
      export XDG_CONFIG_HOME=$HOME/.config
      export XDG_DATA_DIRS=/usr/local/share/:/usr/share/
      export XDG_CONFIG_DIRS=/etc/xdg
      export XDG_CACHE_HOME=$HOME/.cache
      ! [ -d $XDG_CACHE_HOME/zsh ] && mkdir -p $XDG_CACHE_HOME/zsh

      # Set ssh-agent socket.
      if [[ -S $XDG_RUNTIME_DIR/ssh-agent.socket ]]; then
        export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
      fi

      # Set editor.
      export EDITOR='emacsclient -c'

      # Set git editor.
      export GIT_EDITOR='emacsclient -c'

      # Set fzf default command.
      if [[ -x $(command -v fzf) && \
            -x $(command -v rg) ]]; then
        export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
        export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
      fi

      # Add local binaries to path.

      if [[ -d $HOME/.local ]]; then
        PATH=$HOME/.local:$PATH
      fi

      if [[ -d $HOME/.local/bin ]]; then
        PATH=$HOME/.local/bin:$PATH
      fi

      # Add npm installed binaries to path.
      if [[ -d $HOME/.npm/bin ]]; then
        PATH=$HOME/.npm/bin:$PATH
      fi

      # Disable fancy prompts for tramp to work.
      if [[ "$TERM" == "dumb" ]]; then
          unsetopt zle
          PS1='$ '
      fi

      if [[ -d $HOME/Applications/android-sdk ]]; then
          export ANDROID_HOME=$HOME/Applications/android-sdk
          export PATH=$ANDROID_HOME/platform-tools:$PATH
      fi

      if [[ -d $HOME/Developer && -d $HOME/Developer/go ]]; then
          export GOPATH=$HOME/Developer/go
          export PATH=$GOPATH/bin:$PATH
      fi
          '';
    initExtra = ''
      ## Late-init environment variables
      # Direnv
      [ -x $(command -v direnv) ] && eval "$(direnv hook zsh)"

      # Starship
      [ -x $(command -v starship) ] && eval "$(starship init zsh)"

      ## Options

      # extended globbing patterns
      setopt extendedglob

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

      # What zsh identifies as a word delimiter
      autoload -U select-word-style
      select-word-style bash

      ## Functions

      # Find: [f]ile by name.
      function ff () {
        if ! [[ -n "$1" ]]; then
          echo 'Must specify a search string.'
          return 1
        fi

        find . -name "$1"
      }

      # Find: name [a]ll.
      function fa () { ff "*$1*" }

      # Find: name [s]tarts with.
      function fs () { ff "$1*" }

      # Find name [e]nds with.
      function fe () { ff "*$1" }

      # Create a new tar archive.
      function tarz () {
        if ! [[ -n "$1" ]]; then
          echo 'Must specify directory to use.'
          return 1
        fi

        tar -czf $1.tar.gz $1
      }
    '';
  };

  # Starship
  home.file.".config/starship.toml".source = ../../starship.toml;
}
