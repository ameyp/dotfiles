;; Install:
; gem install pry pry-nav pry-stack_explorer termios

(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :ensure
  :config
  (setq helm-dash-docsets '("Ruby")
        tab-width 4)
  :hook (enh-ruby-mode . eldoc-mode))

(provide 'ameyp-ruby)
