(use-package clojure-mode
  :mode "\\.cljs?\\'"
  :hook (clojure-mode . cider-mode))

(use-package cider-mode
  :after (clojure-mode)
  :config
  (setq nrepl-hide-special-buffers t)
  :hook
  (cider-repl-mode . 'rainbow-delimiters-mode)
  (cider-repl-mode . 'smartparens-strict-mode))

(use-package cider-eldoc
  :after (cider-mode)
  :hook (cider-mode . eldoc-mode))

(provide 'ameyp-clojure)
