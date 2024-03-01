(use-package clojure-mode
  :mode "\\.cljs?\\'"
  :ensure t
  :hook ((clojure-mode . eglot-ensure)
         (clojurec-mode . eglot-ensure)
         (clojurescript-mode . eglot-ensure))
  )

(provide 'ameyp-clojure)
