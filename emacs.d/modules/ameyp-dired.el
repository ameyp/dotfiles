(use-package dired
  :ensure t
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; List for what external programs are invoked when ! or & is pressed.
(setq dired-guess-shell-alist-user
      (list
       ;; (list "\\.pdf$" "zathura")  ; fixed rule
       ;; possibly more rules...
       (list "\\.pdf$"  ; rule with condition test
             '(cond ((eq system-type 'darwin) "open")
                    ((eq system-type 'gnu/linux) "zathura"))
             )))


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

;; Quickly move files when two different windows are open.
(setq dired-dwim-target t)

(provide 'ameyp-dired)
