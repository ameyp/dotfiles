(use-package clojure-mode
  :mode "\\.cljs?\\'"
  :hook (clojure-mode . cider-mode))

(use-package cider
  :after (clojure-mode rainbow-delimiters smartparens)
  :config
  (setq nrepl-hide-special-buffers t)
  :hook
  (cider-repl-mode . 'rainbow-delimiters-mode)
  (cider-repl-mode . 'smartparens-strict-mode))

(provide 'ameyp-clojure)
