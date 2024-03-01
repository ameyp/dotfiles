(use-package clojure-mode
  :mode "\\.cljs?\\'"
  :ensure t
  :hook ((clojure-mode . lsp-deferred)
         (clojurec-mode . lsp-deferred)
         (clojurescript-mode . lsp-deferred))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
     (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
     ;(setq lsp-clojure-server-command '("/path/to/clojure-lsp"))) ;; Optional: In case `clojure-lsp` is not in your $PATH
  )

(provide 'ameyp-clojure)
