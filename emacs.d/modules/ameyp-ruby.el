;; Install:
; gem install pry pry-nav pry-stack_explorer termios

(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (setq tab-width 4)
  :hook (enh-ruby-mode . eldoc-mode))

(use-package inf-ruby)

(use-package robe)

(use-package yard-mode)

(provide 'ameyp-ruby)
