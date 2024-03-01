;; ---- org-mode ----------------------------

;; C-c , assigns a priority to a TODO
;; C-c C-t cycles between states (keywords)
;; C-c a t shows complete todo list

(use-package org
  :straight t
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-log-done t
        org-startup-truncated nil
        org-todo-keywords '((sequence "TODO" "REVIEW" "VERIFY" "|" "DONE" "DELEGATED"))
        org-todo-keywords '((sequence "TODO(t)" "REVIEW(r)" "VERIFY(v)" "|" "DONE(d)" "DELEGATED(l)")))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.6))))
     `(ivy-org ((t (:default))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
     `(org-block-begin-line ((t (:height 140 :weight thin :foreground "#888888"))))
     `(org-block-end-line ((t (:height 140 :weight thin :foreground "#888888"))))
     `(variable-pitch ((t (:family "Verdana" :height 160 :weight thin))))
     ))

  (setq org-agenda-files (list "~/Dropbox/org-mode/tasks.org"))
  (setq org-default-notes-file "~/Dropbox/org-mode/tasks.org")

  (defcustom hugo-post-root nil "Path to the folder in which you want Hugo posts to be stored.")

  (defun org-capture-hugo-post ()
    (if (not hugo-post-root)
        (customize-set-variable 'hugo-post-root (read-string "Path to Hugo posts root: ")))
    (let* ((title (read-string "Title: "))
           (date (format-time-string "%Y-%m-%d"))
           (slug (downcase
                  ;; Replace any whitespace in the title with -,
                  ;; and then remove any non-alphanumeric characters.
                  (replace-regexp-in-string
                   "[^a-z0-9\-]" ""
                   (replace-regexp-in-string "\s" "-" title))))

           ;; Format is YYYY-MM-DD-title-in-lower-case-with-dashes-separating-words.org
           (file-name (format "%s/%s-%s.org" hugo-post-root date slug))
           (post-buffer (find-file-noselect file-name)))
      (with-current-buffer post-buffer
        (insert "#+draft: true\n")
        (insert "#+type: post\n")
        (insert (format "#+title: %s\n" title))
        (insert (format "#+date: %s\n" date))
        (insert "#+tags: []\n")
        )
      (switch-to-buffer post-buffer)))

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
           entry (file+headline "~/Dropbox/org-mode/tasks.org" "Tasks")
           "* TODO %? \nCREATED: %U")
          ("j" "Journal Entry"
           entry (file+datetree "~/Dropbox/org-mode/journal.org")
           "* %(format-time-string \"%H:%M:%S\") %?"
           :empty-lines 1)
          ("h" "Hugo post"
           entry
           (file+olp org-hugo-get-all-posts "Blog Ideas")
           (function org-hugo-new-subtree-post-capture-template))

          ;; ... other templates
          ))

  (setq org-capture-templates (list))

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook
  (org-mode . visual-line-mode))

(use-package ox-hugo
  :after ox)

(provide 'ameyp-org)
