;; Activate company-mode
(add-hook 'after-init-hook
	  (lambda ()
	    (global-company-mode)
	    (define-key global-map (kbd "C-<tab>") 'company-complete)
	    (define-key company-active-map (kbd "C-n") 'company-select-next)
	    (define-key company-active-map (kbd "C-p") 'company-select-previous)
	    (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
	    (define-key company-active-map (kbd "<tab>") 'company-complete)))

(provide 'ameyp-company)
