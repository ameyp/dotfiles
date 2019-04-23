(use-package emacs-lisp-mode
  :mode "\\.el\\'"
  :config
  (setq company-backends '(company-capf company-elisp)
	flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :bind (:map emacs-lisp-mode-map
	      ("C-c d" . ameyp-elisp/popup-documentation-at-point)))

(use-package pos-tip
  :ensure
  :config
  (defun ameyp-elisp/popup-documentation-at-point (function)
   "Display the full documentation of FUNCTION (a symbol) in tooltip."
   (interactive (list (function-called-at-point)))
   (if (null function)
       (pos-tip-show
        "** You didn't specify a function! **" '("red"))
     (pos-tip-show
      (with-temp-buffer
        (let ((standard-output (current-buffer))
              (help-xref-following t))
          (prin1 function)
          (princ " is ")
          (describe-function-1 function)
          (buffer-string)))
      nil nil nil 0))))

(provide 'ameyp-elisp)
