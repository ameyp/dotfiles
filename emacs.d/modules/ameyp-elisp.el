(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq company-backends '(company-capf company-elisp))
            ;; (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            (setq indent-tabs-mode nil)))

(defun advice-unadvice (sym)
  "Remove all advices from symbol SYM.
Use with (advice-unadvice 'function-with-advice)"
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(provide 'ameyp-elisp)
