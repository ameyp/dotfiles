(require 'package)
(setq package-enable-at-startup nil)
(setq package-user-dir "~/.emacs.d/.elpa")

(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(defvar ameyp-package-list)
(setq ameyp-package-list
      (quote (
              browse-kill-ring
              cider
              cmake-mode
              coffee-mode
              company
              company-go
              company-lsp
              company-tern
              dash
              enh-ruby-mode
              epl
              expand-region
              find-file-in-project
              flycheck
              flycheck-lsp
              git-commit-mode
              git-rebase-mode
              glsl-mode
              go-mode
              haskell-mode
              haxe-mode
              helm
              helm-cmd-t
              helm-company
              helm-dash
              helm-projectile
              inf-ruby
              jade-mode
              js2-mode
              lsp-mode
              lsp-ui
              lua-mode
              markdown-mode
              multiple-cursors
              package-build
              projectile
              rainbow-delimiters
              robe
              rust-mode
              smartparens
              tern
              web-mode
              yaml-mode
              yard-mode
              )))

(dolist (package ameyp-package-list)
  (when (and (not (package-installed-p package))
           (assoc package package-archive-contents))
    (package-install package)))

(provide 'ameyp-packages)
