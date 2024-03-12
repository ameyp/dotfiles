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
      ag
      age
      apheleia
      auto-virtualenv
      clojure-mode
      cmake-mode
      company
      consult
      consult-projectile
      counsel
      counsel-projectile
      denote
      direnv
      dockerfile-mode
      eglot
      eglot-java
      eldoc
      embark
      embark-consult
      exec-path-from-shell
      expand-region
      go-mode
      haskell-mode
      ivy
      jest-test-mode
      jinx
      jsonnet-mode
      lsp-tailwindcss
      magit
      marginalia
      markdown-mode
      modus-themes
      multi-vterm
      nerd-icons
      nerd-icons-dired
      nix-mode
      orderless
      org
      org-roam
      ox-hugo
      ox-pandoc
      package-build
      pinentry
      projectile
      protobuf-mode
      pulsar
      pyenv-mode
      python
      python-pytest
      rainbow-delimiters
      rustic
      smartparens
      spacious-padding
      swift-mode
      terraform-mode
      tree-sitter
      treesit-grammars.with-all-grammars
      typescript-mode
      undo-tree
      use-package
      vertico
      vterm
      yaml-mode
      zig-mode
    ]));
}
