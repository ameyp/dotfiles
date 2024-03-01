(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure))

(provide 'ameyp-go)
