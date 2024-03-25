{ config, lib, pkgs, ... }: {
  news.display = "silent";

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
  home.stateVersion = "23.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Packages to install
  home.packages = [
    (pkgs.nerdfonts.override { fonts = [ "Hack" ]; })
    pkgs.curl
    pkgs.direnv
    pkgs.fd
    pkgs.fzf
    pkgs.git
    pkgs.gopls
    pkgs.grc
    pkgs.htop
    pkgs.lsd
    pkgs.pandoc
    pkgs.pyenv
    pkgs.ripgrep
    pkgs.starship
    pkgs.wget

    pkgs.age
    pkgs.rage
    pkgs.pinentry-gtk2

    # PDF viewer that can read from stdin
    pkgs.zathura

    # lsp
    pkgs.tailwindcss-language-server
  ];

  # Enable direnv
  # https://github.com/nix-community/nix-direnv
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  # Git config
  programs.git = {
    enable = true;
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
    # https://difftastic.wilfred.me.uk/git.html
    difftastic = {
      enable = true;
      background = "dark";
    };
    aliases = {
      dag = "log --graph --format='format:%C(yellow)%h%C(reset) %C(blue)\\\"%an\\\" <%ae>%C(reset) %C(magenta)%ar%C(reset)%C(auto)%d%C(reset)%n%s' --date-order";
      co = "checkout";
      st = "status";
      rb = "rebase";
      br = "branch";
      ec = "emacsclient";
      dft = "difftool";
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
      ".envrc"
      ".venv.*"
    ];
  };

  # ZSH
  programs.zsh = {
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
    shellAliases = {
      gca = "${pkgs.git}/bin/git commit -a --amend --no-edit";
      ec = "${pkgs.emacsAmeyWithPackages}/bin/emacsclient -c";
      ls = "${pkgs.lsd}/bin/lsd";
    };
    envExtra = ''
      export JAVA_TOOLS_OPTIONS="-Dlog4j2.formatMsgNoLookups=true"

      # Nix
      [ -f /etc/profile ] && source /etc/profile

      # XDG Base Directory Specification.
      export XDG_DATA_HOME=$HOME/.local/share
      export XDG_CONFIG_HOME=$HOME/.config
      export XDG_DATA_DIRS=$HOME/.nix-profile/share/applications:/usr/local/share/:/usr/share/
      export XDG_CONFIG_DIRS=/etc/xdg
      export XDG_CACHE_HOME=$HOME/.cache
      ! [ -d $XDG_CACHE_HOME/zsh ] && mkdir -p $XDG_CACHE_HOME/zsh

      # Set ssh-agent socket.
      if [[ -S $XDG_RUNTIME_DIR/ssh-agent.socket ]]; then
        export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
      fi

      # Set editor.
      export EDITOR='${pkgs.emacsAmeyWithPackages}/bin/emacsclient -c'

      # Set git editor.
      export GIT_EDITOR='${pkgs.emacsAmeyWithPackages}/bin/emacsclient -c'

      # Configure ripgrep defaults
      export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

      # Set fzf default command.
      export FZF_DEFAULT_COMMAND='${pkgs.ripgrep}/bin/rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
      export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

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

      if [[ -d $HOME/.pyenv ]]; then
          export PATH=$HOME/.pyenv/bin:$PATH
      fi

      if [[ -d $HOME/.config/nvm ]]; then
          export NVM_DIR="$HOME/.config/nvm"
          . "$NVM_DIR/nvm.sh"
      fi
          '';
    initExtra = ''
      ## Late-init environment variables
      # Direnv
      eval "$(${pkgs.direnv}/bin/direnv hook zsh)"

      # Starship
      eval "$(${pkgs.starship}/bin/starship init zsh)"

      # Pyenv
      eval "$(${pkgs.pyenv}/bin/pyenv init -)"

      [ -f ~/.zsh-extra ] && . ~/.zsh-extra

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

      # Delete local branches that have been merged to main.
      function gitprune () {
        ${pkgs.git}/bin/git checkout -q main && ${pkgs.git}/bin/git for-each-ref refs/heads/ "--format=%(refname:short)" | while read branch; do mergeBase=$(${pkgs.git}/bin/git merge-base main $branch) && [[ $(${pkgs.git}/bin/git cherry main $(${pkgs.git}/bin/git commit-tree $(${pkgs.git}/bin/git rev-parse "$branch^{tree}") -p $mergeBase -m _)) == "-"* ]] && ${pkgs.git}/bin/git branch -D $branch; done
      }
    '';
  };

  # Bash
  programs.bash = let bash_attrs = {
  }; in {
    enable = true;
  };

  # Fish
  programs.fish = {
    enable = true;
    shellAbbrs = {
      gca = "${pkgs.git}/bin/git commit -a --amend --no-edit";
      ec = "${pkgs.emacsAmeyWithPackages}/bin/emacsclient -c";
    };
    shellAliases = {
      ls = "${pkgs.lsd}/bin/lsd";
    };
    shellInit = ''
      set -x XDG_DATA_HOME $HOME/.local/share
      set -x XDG_CONFIG_HOME $HOME/.config
      set -x XDG_DATA_DIRS $HOME/.nix-profile/share/applications:/usr/local/share/:/usr/share/
      set -x XDG_CONFIG_DIRS /etc/xdg
      set -x XDG_CACHE_HOME $HOME/.cache

      # Set editor.
      set -x EDITOR '${pkgs.emacsAmeyWithPackages}/bin/emacsclient -c'

      # Set git editor.
      set -x GIT_EDITOR '${pkgs.emacsAmeyWithPackages}/bin/emacsclient -c'

      # Configure ripgrep defaults
      set -x RIPGREP_CONFIG_PATH $HOME/.ripgreprc

      # Set fzf default command.
      set -x FZF_DEFAULT_COMMAND '${pkgs.ripgrep}/bin/rg --files --no-ignore --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'
      set -x FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"

      if test -d "$HOME/.local"
        fish_add_path "$HOME/.local"
      end

      if test -d "$HOME/.local/bin"
        fish_add_path "$HOME/.local/bin"
      end

      if test -d "$HOME/.npm/bin"
        fish_add_path "$HOME/.npm/bin"
      end

      eval (${pkgs.direnv}/bin/direnv hook fish)

      if test -f "$HOME/.fish-extra"
        source "$HOME/.fish-extra"
      end
    '';
    interactiveShellInit = ''
      set fish_greeting # Disable greeting

      # https://github.com/pyenv/pyenv#set-up-your-shell-environment-for-pyenv
      # The documentation says to execute this interactively.
      if test -d "$HOME/.pyenv"
        set -Ux PYENV_ROOT "$HOME/.pyenv"
        fish_add_path "$PYENV_ROOT/bin"
      end

      # Fish executes a fish_prompt function whenever it needs to show the prompt.
      function fish_prompt --description 'Write out the prompt'
          set -l last_status $status
          set -l normal (set_color normal)
          set -l status_color (set_color brgreen)
          set -l cwd_color (set_color $fish_color_cwd)
          set -l vcs_color (set_color brpurple)
          set -l prompt_status ""

          # Since we display the prompt on a new line allow the directory names to be longer.
          set -q fish_prompt_pwd_dir_length
          or set -lx fish_prompt_pwd_dir_length 0

          # Color the prompt differently when we're root
          set -l suffix '❯'
          if functions -q fish_is_root_user; and fish_is_root_user
              if set -q fish_color_cwd_root
                  set cwd_color (set_color $fish_color_cwd_root)
              end
              set suffix '#'
          end

          # Color the prompt in red on error
          if test $last_status -ne 0
              set status_color (set_color $fish_color_error)
              set prompt_status $status_color "[" $last_status "]" $normal
          end

          echo -s (prompt_login) ' ' $cwd_color (prompt_pwd) $vcs_color (fish_vcs_prompt) $normal ' ' $prompt_status
          echo -n -s $status_color $suffix ' ' $normal
      end
    '';
    plugins = [
      # Enable a plugin (here grc for colorized command output) from nixpkgs
      { name = "grc"; src = pkgs.fishPlugins.grc.src; }
      {
        name = "nvm";
        src = pkgs.fetchFromGitHub {
          owner = "jorgebucaran";
          repo = "nvm.fish";
          rev = "c69e5d1017b21bcfca8f42c93c7e89fff6141a8a";
          sha256 = "LV5NiHfg4JOrcjW7hAasUSukT43UBNXGPi1oZWPbnCA=";
        };
      }
    ];
  };

  # Kitty
  programs.kitty = {
    enable = true;
    shellIntegration.enableZshIntegration = true;
    theme = "Modus Vivendi";
    settings = {
      # https://sw.kovidgoyal.net/kitty/conf
      font_family = "Hack Nerd Font Mono";
      font_size = lib.mkDefault "12.0";
      scrollback_lines = "-1";
      cursor_blink_interval = "0";
      shell = "${pkgs.fish}/bin/fish --interactive --login";
    };
  };

  # Starship
  home.file.".config/starship.toml".source = ./starship.toml;

  # Ripgrep
  home.file.".ripgreprc".source = ./ripgreprc;

  # xdg.configFile."xmonad".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.dotfiles/xmonad";
  # Lockscreen
  # Initialize it with images by running
  # betterlockscreen -b /path/to/folder/or/image
  # services.screen-locker = {
  #   enable = true;
  #   lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen -l blur";
  #   xautolock = {
  #     # Use xidlehook instead of xautolook because it supports detecting when audio is playing.
  #     # Otherwise, use the "-corners" option (see man xautolook).
  #     extraOptions = ["-corners 000-"];
  #   };
  # };
}
