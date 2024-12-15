;; ---- org-mode ----------------------------

;; C-c , assigns a priority to a TODO
;; C-c C-t cycles between states (keywords)
;; C-c a t shows complete todo list

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-hide-emphasis-markers nil
        org-log-done t
        org-startup-truncated nil
        ;; TODO Look up how to set these per-file.
        org-todo-keywords '((sequence "TODO" "REVIEW" "|" "DONE"))
        org-todo-keywords '((sequence "TODO(t)" "REVIEW(r)" "|" "DONE(d)")))

  ;; (let* ((variable-tuple
  ;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
  ;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  ;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  ;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
  ;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  ;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  ;;        (base-font-color     (face-foreground 'default nil 'default))
  ;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  ;;   (custom-theme-set-faces
  ;;    'user
  ;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
  ;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
  ;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.6))))
  ;;    `(ivy-org ((t (:default))))
  ;;    ;; My org-mode capture titles are appearing as bookmarks for some reason.
  ;;    `(bookmark-face ((t (:default))))
  ;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
  ;;    `(org-block-begin-line ((t (:height 140 :weight thin :foreground "#888888"))))
  ;;    `(org-block-end-line ((t (:height 140 :weight thin :foreground "#888888"))))
  ;;    `(variable-pitch ((t (:family "Verdana" :height 160 :weight thin))))
  ;;    ))

  (defconst ameyp/org-tasks-personal-file "~/Sync/Documents/org-mode/tasks.org")
  (defconst ameyp/org-tasks-work-file "~/Documents/org-mode/tasks.org")
  (defconst ameyp/org-journal-personal-file "~/Sync/Documents/org-mode/journal.org")
  (defconst ameyp/org-journal-work-file "~/OneDrive - Qualcomm/Documents/journal.org")

  (setq org-agenda-files (list ameyp/org-tasks-personal-file ameyp/org-tasks-work-file))
  ;; Set this to t, otherwise org-mode complains when agenda files are missing.
  (setq org-agenda-skip-unavailable-files t)
  (setq org-default-notes-file "~/Sync/Documents/org-mode/tasks.org")

  (defcustom hugo-post-root nil "Path to the folder in which you want Hugo posts to be stored.")

  (defun org-hugo-get-all-posts ()
    (if (not hugo-post-root)
        (customize-set-variable 'hugo-post-root
                                (read-string "Path to Hugo posts root: ")))
    (format "%s/%s" hugo-post-root "all-posts.org"))

  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title))
           (all-posts (format "%s/%s" hugo-post-root "all-posts.org"))
           )
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (setq org-capture-templates
        '(
          ("t" "Todo"
           entry (file+headline "~/Sync/Documents/org-mode/tasks.org" "Tasks")
           "* TODO %? \nCREATED: %U\n")
          ("j" "Journal Entry"
           entry (file+datetree ameyp/org-journal-personal-file)
           "* %(format-time-string \"%H:%M:%S\") %?\n"
           :empty-lines 1)
          ("w" "Work Journal Entry"
           entry (file+datetree ameyp/org-journal-work-file)
           "* %(format-time-string \"%H:%M:%S\") %?\n"
           :empty-lines 1)
          ("h" "Hugo post"
           entry
           (file+olp org-hugo-get-all-posts "Blog Ideas")
           (function org-hugo-new-subtree-post-capture-template))

          ;; ... other templates
          ))

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("src" . "src"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  ;; Convenient function for copying the content of cells.
  (bind-key (kbd "C-c y")
            (lambda()(interactive)
              (when (org-at-table-p)
                (kill-new
                 (string-trim
                  (substring-no-properties(org-table-get-field))))
                (message "copied cell: @%d$%d"
                         (org-table-current-line)
                         (org-table-current-column) )))
            org-mode-map)

  (require 'org-tempo)

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         (:map org-mode-map
               ("C-," . nil))
         ("C-," . consult-projectile-find-file)
         )
  :hook
  (org-mode . visual-line-mode))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Sync/Documents/org-mode/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-<tab>" . completion-at-point))
  )

(use-package ox-hugo
  :after ox)

(use-package ox-pandoc)

(provide 'ameyp-org)
