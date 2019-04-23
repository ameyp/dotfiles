;; Activate company-mode
(use-package company
  :config
  (setq company-dabbrev-downcase nil)
  ;(global-company-mode)
  :bind (("C-<tab>" . 'company-complete)
	 :map company-active-map
	 ("C-n" . 'company-select-next)
	 ("C-p" . 'company-select-previous)
	 ("C-d" . 'company-show-doc-buffer)
	 ("<tab>" . 'company-complete)))

(provide 'ameyp-company)
