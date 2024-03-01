(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred))

(provide 'ameyp-go)
