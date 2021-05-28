(use-package flycheck
  :init (global-flycheck-mode)
  :delight flycheck-mode
  :config
  (setq flycheck-display-errors-function nil))

(provide 'ameyp-flycheck)
