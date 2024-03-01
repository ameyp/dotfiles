(use-package eglot-java
  :config
  (setq eglot-java-eclipse-jdt-args
        '("-noverify"
        "-Xmx1G"
        "-XX:+UseG1GC"
        "-XX:+UseStringDeduplication"
        ))
  (add-hook 'java-mode-hook #'eglot-ensure))

(provide 'ameyp-java)
