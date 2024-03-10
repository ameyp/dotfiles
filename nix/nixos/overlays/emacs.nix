# Copied from https://github.com/noctuid/dotfiles/blob/master/nix/overlays/emacs.nix#L23

# my custom Emacs
# use emacs-plus patches on osx
# (eventually) use lucid on linux

# relevant links:
# https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/editors/emacs/generic.nix
# https://github.com/nix-community/emacs-overlay/blob/master/overlays/emacs.nix
# https://github.com/d12frosted/homebrew-emacs-plus/tree/master/patches/emacs-30

self: super: rec {
  # configuration shared for all systems
  emacsGitAmeyGeneric = super.emacs-unstable.override {
    withSQLite3 = true;
    withWebP = true;
    withImageMagick = true;
    # have to force this; lib.version check wrong or because emacsGit?
    withTreeSitter = true;
    withNativeCompilation = true;
  };
  emacsAmey =
    if super.stdenv.isDarwin
    then
      emacsGitAmeyGeneric.overrideAttrs (old: {
        patches =
          (old.patches or []);
      })
    else
      (emacsGitAmeyGeneric.override {
        withPgtk = true;
        withXinput2 = true;
      });
  # On macOS, remember to do a fresh launch of emacs, not from the dock icon
  # after adding to this list.
  emacsAmeyWithPackages =
    ((super.emacsPackagesFor emacsAmey).emacsWithPackages (epkgs: with epkgs; [
      # necessary to install through nix to get libenchant integration working
      jinx

      use-package
      vterm
      multi-vterm
      exec-path-from-shell
      package-build
      ag
      markdown-mode
      clojure-mode
      dockerfile-mode
      swift-mode
      eglot
      projectile
      terraform-mode
      jsonnet-mode
      company
      undo-tree
      modus-themes
      spacious-padding
      pulsar
      counsel-projectile
      ivy
      counsel
      rustic
      cmake-mode
      protobuf-mode
      yaml-mode

      org
      org-roam
      ox-hugo
      ox-pandoc
      zig-mode
      go-mode
      haskell-mode
      magit
      nix-mode
      tree-sitter
      treesit-grammars.with-all-grammars
      direnv
      vertico
      orderless
      consult
      marginalia
      embark
      embark-consult
      consult-projectile
      python
      pyenv-mode
      auto-virtualenv
      python-pytest
      # whitespace
      eldoc
      expand-region
      smartparens
      rainbow-delimiters
      typescript-mode
      # tsi
      apheleia
      jest-test-mode
      eglot-java
      lsp-tailwindcss

      # For gpg
      pinentry

      age

      # For icons
      nerd-icons
      nerd-icons-dired
    ]));
}
