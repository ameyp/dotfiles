(use-package protobuf-mode
  :hook (protobuf-mode . (lambda ()
                                (setq-local c-basic-offset 4))
                            ))

(provide 'ameyp-protobuf)
