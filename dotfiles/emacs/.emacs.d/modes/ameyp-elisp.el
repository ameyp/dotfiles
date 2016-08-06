(defun popup-documentation-at-point ()
  (interactive)
  (let* ((position (point))
         (string-under-cursor (buffer-substring-no-properties
                         (progn (skip-syntax-backward "w_") (point))
                         (progn (skip-syntax-forward "w_") (point)))))
    (goto-char position)
    (popup-tip (ac-symbol-documentation (intern string-under-cursor)))))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (define-key emacs-lisp-mode-map (kbd "C-c d") 'popup-documentation-at-point)
	    (setq-local company-backends '(company-capf company-elisp))
	    (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

(provide 'ameyp-elisp)
