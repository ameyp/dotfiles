(use-package lsp-java
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-java-enable-file-watch nil
        lsp-java-vmargs
        '("-noverify"
        "-Xmx1G"
        "-XX:+UseG1GC"
        "-XX:+UseStringDeduplication"
        ))
  (add-hook 'java-mode-hook #'lsp))

(provide 'ameyp-java)
