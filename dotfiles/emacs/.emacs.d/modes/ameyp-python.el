(autoload 'python-mode "python-mode" "Major mode for editing python files" t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-hook 'python-mode-hook
	  (lambda ()
	    ;; Activate helm-dash docset
	    (setq-local helm-dash-docsets '("Python 3"))))

(provide 'ameyp-python)
