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
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))
     `(variable-pitch ((t (:family "Geneva" :height 160 :weight thin))))
     ))

  (setq org-agenda-files (list "~/Dropbox/org-mode/tasks.org"))
  (setq org-default-notes-file "~/Dropbox/org-mode/tasks.org")

  (setq org-capture-templates
        '(
          ("t" "Todo"
           entry (file+headline "~/Dropbox/org-mode/tasks.org" "Tasks")
           "* TODO %? \nCREATED: %U")
          ("j" "Journal Entry"
           entry (file+datetree "~/Dropbox/org-mode/journal.org")
           "* %(format-time-string \"%H:%M:%S\") %?"
           :empty-lines 1)

          ;; ... other templates
          ))

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook
  (org-mode . visual-line-mode))

(provide 'ameyp-org)
