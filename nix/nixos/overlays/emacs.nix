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
  };
  emacsAmey =
    if super.stdenv.isDarwin
    then
      emacsGitAmeyGeneric.overrideAttrs (old: {
        patches =
          (old.patches or [])
          ++ [
            # Don't raise another frame when closing a frame
            # (super.fetchpatch {
            #   url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/no-frame-refocus-cocoa.patch";
            #   sha256 = "QLGplGoRpM4qgrIAJIbVJJsa4xj34axwT3LiWt++j/c=";
            # })
            # # Fix OS window role so that yabai can pick up Emacs
            # (super.fetchpatch {
            #   url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
            #   sha256 = "+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
            # })
            # # Use poll instead of select to get file descriptors
            # (super.fetchpatch {
            #   url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-29/poll.patch";
            #   sha256 = "jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
            # })
            # # Add setting to enable rounded window with no decoration (still
            # # have to alter default-frame-alist)
            # (super.fetchpatch {
            #   url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/round-undecorated-frame.patch";
            #   sha256 = "uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
            # })
            # # Make Emacs aware of OS-level light/dark mode
            # # https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
            # (super.fetchpatch {
            #   url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/system-appearance.patch";
            #   sha256 = "oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
            # })
          ];
      })
    else
      # TODO nix's lucid reports the wrong mm-size (breaks textsize package):
      # (frame-monitor-attribute 'mm-size (selected-frame))
      (emacsGitAmeyGeneric.override {
        withX = true;
        # lucid
        # withGTK2 = false;
        withGTK3 = true;
        withXinput2 = true;
      }).overrideAttrs(_: {
        # for full control/testing (e.g. can't do lucid without cairo using
        # builtin withs)
        configureFlags = [
          # for a (more) reproducible build
          "--disable-build-details"
          "--with-modules"
          "--with-x-toolkit=gtk3"
          "--with-xft"
          "--with-cairo"
          "--with-xaw3d"
          "--with-native-compilation"
          "--with-imagemagick"
          "--with-xinput2"
        ];
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
