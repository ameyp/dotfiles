;; Markdown mode
(autoload 'markdown-mode "markdown-mode" "Major mode for Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (define-key markdown-mode-map (kbd "RET") 'newline)))

(provide 'ameyp-markdown)
