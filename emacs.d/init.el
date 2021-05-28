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

;; Load config files
(mapcar 'require '(;; load the essential packages first
                   ameyp-packages
                   ameyp-text
                   ameyp-gui
                   ameyp-company
                   ameyp-flycheck
                   ameyp-ivy

                   ;; load language-specific packages next
                   ameyp-clojure
                   ameyp-cmake
                   ameyp-coffee
                   ameyp-elisp
                   ameyp-git
                   ameyp-haskell
                   ameyp-java
                   ameyp-lsp
                   ameyp-markdown
                   ameyp-org
                   ameyp-python
                   ameyp-ruby
                   ameyp-rust
                   ameyp-swift
                   ameyp-typescript
                   ameyp-warnings
                   ameyp-web
                   ameyp-yaml
                   ameyp-zig))

;; Set path
(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path (append exec-path '("%HOME%/Apps/bin")))
      (setenv "PATH" (concat (getenv "PATH") ";%HOME%\\Apps\\bin")))
  (progn
    (setq exec-path (append exec-path '("/usr/local/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("~/Apps/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":~/Apps/bin"))
    (setq exec-path (append exec-path '("~/.cargo/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))))
