;(package-initialize)

;; For native compilation
(setq comp-speed 2)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(server-start)

;; Add custom paths to 'load-path
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/themes")

;; Disable those god-awful documentation warnings
;(eval-after-load 'flycheck (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Print complete messages for C-x C-e
(setq eval-expression-print-length 10000)

;; Echo keystrokes faster in minibuffer
(setq echo-keystrokes 0.01)

;; Enable minibuffer-in-minibuffer
(setq enable-recursive-minibuffers t)

;; Load config files
(mapcar 'require '(;; load the essential packages first
                   ameyp-packages
                   ameyp-direnv
                   ameyp-text
                   ameyp-gui
                   ameyp-company
                   ameyp-eglot
                   ameyp-flycheck

                   ;; load language-specific packages next
                   ameyp-clojure
                   ameyp-cmake
                   ameyp-coffee
                   ameyp-docker
                   ameyp-elisp
                   ameyp-git
                   ameyp-go
                   ameyp-haskell
                   ameyp-java
                   ameyp-jsonnet
                   ameyp-markdown
                   ameyp-nix
                   ameyp-org
                   ameyp-protobuf
                   ameyp-projectile
                   ameyp-python
                   ameyp-ruby
                   ameyp-rust
                   ameyp-swift
                   ameyp-syntax
                   ameyp-terraform
                   ameyp-typescript
                   ameyp-vertico
                   ameyp-warnings
                   ameyp-web
                   ameyp-yaml
                   ameyp-zig
                   ))

;; Load modules that live outside of my dotfiles.
(if (file-readable-p "~/.emacs-extra/init.el")
    (load "~/.emacs-extra/init.el"))
(put 'set-goal-column 'disabled nil)
