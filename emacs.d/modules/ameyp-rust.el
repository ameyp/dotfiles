(use-package lsp-mode
  :ensure t
  :commands lsp)

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . lsp))

(use-package flycheck
  :hook (rust-mode . flycheck-mode))

(provide 'ameyp-rust)
