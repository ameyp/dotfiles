(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . (lambda ()
		     (set (make-local-variable 'company-backends) '(company-go))
		     (company-mode))))

(use-package company-go
  :after (company go-mode)
  :ensure)

(provide 'ameyp-go)
