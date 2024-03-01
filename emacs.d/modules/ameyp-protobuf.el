(use-package protobuf-mode
  :hook (protobuf-mode-hook . (lambda ()
                                (setq c-basic-offset 4))
                            ))

(provide 'ameyp-protobuf)
