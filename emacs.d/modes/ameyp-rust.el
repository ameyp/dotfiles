(autoload 'rust-mode "rust-mode" "Major mode for editing rust files" t)
(autoload 'flymake-rust-load "flymake-rust" "Major mode for editing rust files" t)

(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.rs$" . flymake-rust-load))

(add-hook 'rust-mode-hook 'flymake-rust-load)
          ;; (lambda ()
	  ;;   ;; Activate helm-dash docset
	  ;;   (setq-local helm-dash-docsets '("Rust"))

	  ;;   ;; Activate flymake-rust
	  ;;   (require 'flymake-rust)
	  ;;   (flymake-rust-load)))

(provide 'ameyp-rust)
