(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

(setq amey-age-public-key "age1vwx5jpt3remv9l0yvvj5v4qzkp9jfr42kds3uv9ynecntzlrgezqdj7zth")

(use-package age
  :custom
  (age-default-identity "~/.age/key-encrypted.key")
  (age-program "rage")
  (age-default-recipient `(,amey-age-public-key))
  (age-debug t)
  (age-pinentry-mode 'ask)
  :config
  (age-file-enable))

(defun ameyp-dired-do-encrypt ()
  "Encrypt marked files."
  (interactive)
  (let ((context (age-make-context))
        (recipients
         `(,amey-age-public-key)))
    (dolist (file (dired-get-marked-files))
      (let ((source-file (expand-file-name file)))
        (age-encrypt-file
         context source-file recipients (concat source-file ".age"))
        ))
    (revert-buffer)))

(defun ameyp-dired-open-age-pdf ()
  "Decrypt an encrypted PDF and open it in zathura."
  (interactive)
  (let ((context (age-make-context)))
    (ameyp-spawn-process-with-stdin
     "zathura" ; process name
     "zathura" '("-") ; process and arguments
     (age-decrypt-file (age-make-context) (dired-get-file-for-visit) nil)))
  )

(ameyp-emacs-keybind dired-mode-map
  ": e" #'ameyp-dired-do-encrypt
  ": o" #'ameyp-dired-open-age-pdf)

(provide 'ameyp-dired)
