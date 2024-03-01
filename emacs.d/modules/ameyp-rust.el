;; Install rust-analyzer for this to work.
;; https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary
;; Use the cargo method.

(use-package rustic
  :config
  (push 'rustic-clippy flycheck-checkers)
  ;; Disable function signature documentation windows from popping up automatically.
  ;; lsp-signature-activate pops it up.
  (setq lsp-signature-auto-activate nil)
  )

(provide 'ameyp-rust)
