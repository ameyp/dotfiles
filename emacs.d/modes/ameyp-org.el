;; ---- org-mode ----------------------------

;; C-c , assigns a priority to a TODO
;; C-c C-t cycles between states (keywords)
;; C-c a t shows complete todo list

(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

(require 'org-capture)
(setq org-default-notes-file "~/.emacs.d/org/journal.org")
(define-key global-map "\C-cc" 'org-capture)

(setq org-todo-keywords
      '((sequence "TODO" "REVIEW" "VERIFY" "|" "DONE" "DELEGATED")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "REVIEW(r)" "VERIFY(v)" "|" "DONE(d)" "DELEGATED(l)")))

(setq org-capture-templates
      '(("t" "Todo" entry (file "~/.emacs.d/org/newgtd.org")
             "* TODO %^{Brief Description} %^g\n%?\nAdded: %U\n %i \n%")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-agenda-files '("~/Documents/agenda"))

(provide 'ameyp-org)
