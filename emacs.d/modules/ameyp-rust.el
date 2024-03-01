(use-package lsp-mode
  :commands lsp
  :config
  (require 'lsp-clients))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . lsp)
  :config
  (setq helm-dash-docsets '("Rust")))
  ;(setq indent-tabs-mode nil))

(use-package flycheck
  :hook (rust-mode . flycheck-mode))

(provide 'ameyp-rust)
