;; Install:
; If on Mac, `rbenv install <latest stable>'
; gem install solargraph

(use-package enh-ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (setq tab-width 4)
  :hook (enh-ruby-mode . (lambda ()
                                        ;(require 'lsp-solargraph)
                           (lsp)
                           (eldoc-mode))))

(use-package inf-ruby)

(use-package yard-mode)

(provide 'ameyp-ruby)
