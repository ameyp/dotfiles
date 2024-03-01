(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/home/amey/Applications/swift-5.2.4-RELEASE-ubuntu20.04/usr/bin/sourcekit-lsp"))

(use-package swift-mode)
  ;; Disabling lsp unless activated because it hogs memory.
  ;:hook (swift-mode . (lambda () (lsp))))

(provide 'ameyp-swift)
