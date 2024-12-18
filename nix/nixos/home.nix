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
    pkgs.git
    pkgs.gopls
    pkgs.grc
    pkgs.htop
    pkgs.lsd
    pkgs.pandoc
    pkgs.pyenv
    pkgs.ripgrep
    pkgs.wget

    pkgs.age
    pkgs.rage
    pkgs.pinentry-gtk2

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

      if test -n "$GHOSTTY_RESOURCES_DIR"
        source $GHOSTTY_RESOURCES_DIR/shell-integration/fish/vendor_conf.d/ghostty-shell-integration.fish
      end

      # https://github.com/pyenv/pyenv#set-up-your-shell-environment-for-pyenv
      # The documentation says to execute this interactively.
      if test -d "$HOME/.pyenv"
        set -Ux PYENV_ROOT "$HOME/.pyenv"
        fish_add_path "$PYENV_ROOT/bin"
        pyenv init - | source
        fish_add_path "$PYENV_ROOT/shims"
      end

      # https://github.com/atuinsh/atuin?tab=readme-ov-file#fish
      ${pkgs.atuin}/bin/atuin init fish | source

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

          echo ""
          echo -s (prompt_login) ' ' $cwd_color (prompt_pwd) $vcs_color (fish_vcs_prompt) $normal ' ' $prompt_status
          echo -n -s $status_color $suffix ' ' $normal
      end

      # Necessary for emacs tramp to work.
      if test "$TERM" = "dumb"
        function fish_prompt
          echo "\$ "
        end

        function fish_right_prompt; end
        function fish_greeting; end
        function fish_title; end
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

  programs.ghostty = {
    enable = true;
    settings = {
      font-family = "Iosevka";
      font-style = "Heavy";
      font-size = 14;
      font-thicken = false;
      cursor-style-blink = false;
      shell-integration = "fish";
      command = "${pkgs.fish}/bin/fish --interactive --login";
      # theme = Adventure
      theme = "Banana Blueberry";
      macos-option-as-alt = true;
    };
    keybindings = {
      "ctrl+z" = "reload_config";
    };
  };

  # Ripgrep
  home.file.".ripgreprc".source = ./ripgreprc;

  # Zathura, PDF viewer
  programs.zathura = {
    enable = true;
    options = {
      database = "sqlite";
    };
  };

  # Atuin https://github.com/atuinsh/atuin
  programs.atuin = {
    enable = true;
    settings = {
      auto_sync = false;
    };
  };
  
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
