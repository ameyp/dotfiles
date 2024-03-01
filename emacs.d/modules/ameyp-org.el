;; ---- org-mode ----------------------------

;; C-c , assigns a priority to a TODO
;; C-c C-t cycles between states (keywords)
;; C-c a t shows complete todo list

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq org-log-done t
        org-startup-truncated nil
        org-todo-keywords '((sequence "TODO" "REVIEW" "VERIFY" "|" "DONE" "DELEGATED"))
        org-todo-keywords '((sequence "TODO(t)" "REVIEW(r)" "VERIFY(v)" "|" "DONE(d)" "DELEGATED(l)")))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)))

(provide 'ameyp-org)
