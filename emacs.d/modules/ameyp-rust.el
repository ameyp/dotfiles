(use-package lsp-mode
  :commands lsp
  :config (require 'lsp-clients))

(use-package lsp-ui)

(use-package company-lsp)

(use-package rust-mode
  :ensure
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . lsp)
  :config
  (setq helm-dash-docsets '("Rust")))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (setq lsp-enable-snippet nil))

(provide 'ameyp-rust)
