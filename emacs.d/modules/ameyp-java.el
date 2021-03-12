(use-package lsp-java
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-java-enable-file-watch nil)
  (add-hook 'java-mode-hook #'lsp))

(provide 'ameyp-java)
