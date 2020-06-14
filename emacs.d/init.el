;(package-initialize)

(server-start)

;; Add custom paths to 'load-path
(add-to-list 'load-path "~/.emacs.d/modules")

;; Disable those god-awful documentation warnings
;(eval-after-load 'flycheck (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Load config files
(mapcar 'require '(;; load the essential packages first
                   ameyp-packages
                   ameyp-text
                   ameyp-gui
                   ameyp-company
                   ameyp-helm
                   ameyp-flycheck

                   ;; load language-specific packages next
                   ameyp-clojure
                   ameyp-cmake
                   ameyp-coffee
                   ameyp-elisp
                   ameyp-haskell
                   ameyp-markdown
                   ameyp-org
                   ameyp-python
                   ameyp-ruby
                   ameyp-rust
                   ameyp-swift
                   ameyp-warnings
                   ameyp-web
                   ameyp-yaml))

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


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
