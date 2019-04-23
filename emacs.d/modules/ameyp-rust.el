(use-package rust-mode
  :ensure
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (setq helm-dash-docsets '("Rust")))

(use-package flymake-rust
  :ensure
  :hook (rust-mode . flymake-rust-load))

(provide 'ameyp-rust)
