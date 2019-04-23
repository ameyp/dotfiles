(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq company-backends '(company-capf company-elisp))
            (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            (setq indent-tabs-mode nil)))

(provide 'ameyp-elisp)
