(use-package lsp-mode
  :commands lsp
  :config
  (require 'lsp-clients)
  (setq lsp-enable-snippet nil))

(use-package company-lsp)

(use-package rust-mode
  :ensure
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . lsp)
  :config
  (setq helm-dash-docsets '("Rust")))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'ameyp-rust)
