(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

(defun ameyp-dired-do-encrypt ()
  "Encrypt marked files."
  (interactive)
  (let ((context (epg-make-context))
        (recipient-key (epg-list-keys (epg-make-context) "amey@wirywolf.com" t)))
    (dolist (file (dired-get-marked-files))
      (epa-encrypt-file
       (expand-file-name file) recipient-key))
    )
  (revert-buffer))

(ameyp-emacs-keybind dired-mode-map
  ": e" #'ameyp-dired-do-encrypt)

(provide 'ameyp-dired)
