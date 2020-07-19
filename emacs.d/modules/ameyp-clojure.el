(use-package clojure-mode
  :mode "\\.cljs?\\'"
  :hook (clojure-mode . cider-mode))

(use-package smartparens)

(use-package cider-mode
  :ensure cider
  :after (clojure-mode rainbow-delimiters smartparens)
  :config
  (setq nrepl-hide-special-buffers t)
  :hook
  (cider-repl-mode . 'rainbow-delimiters-mode)
  (cider-repl-mode . 'smartparens-strict-mode))

(use-package cider-eldoc
  :ensure cider
  :after (cider-mode)
  :hook (cider-mode . eldoc-mode))

(provide 'ameyp-clojure)
