(require 'package)
(setq package-user-dir "~/.emacs.d/.elpa")

(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package '(
		   browse-kill-ring
		   cider
		   cmake-mode
		   coffee-mode
		   company
		   dash
		   enh-ruby-mode
		   epl
		   find-file-in-project
		   flycheck
		   git-commit-mode
		   git-rebase-mode
		   glsl-mode
		   haskell-mode
		   haxe-mode
		   helm
		   helm-cmd-t
		   helm-company
		   helm-dash
		   inf-ruby
		   jade-mode
		   js2-mode
		   lua-mode
		   magit
		   markdown-mode
		   package-build
		   rainbow-delimiters
		   robe
		   rust-mode
		   smartparens
		   yaml-mode
		   yard-mode))
  (unless (package-installed-p package)
    (package-install package)))
