(use-package haskell-mode
  :mode "\\.hs\\'"
  :config
  (haskell-indentation-mode)
  :bind (:map haskell-mode-map
         ("C-," . haskell-move-nested-left)
         ("C-." . haskell-move-nested-right)
         ("C-c C-l" . haskell-process-load-or-reload)
         ("C-`" . haskell-interactive-bring)
         ("C-c C-t" . haskell-process-do-type)
         ("C-c C-i" . haskell-process-do-info)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c c" . haskell-process-cabal)
         :map haskell-cabal-mode-map
         ("C-`" . haskell-interactive-bring)
         ("C-c C-k" . haskell-interactive-mode-clear)
         ("C-c C-c" . haskell-process-cabal-build)
         ("C-c c" . haskell-process-cabal))
  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t))

(provide 'ameyp-haskell)
