;; Awesome guide:
;; http://tuhdo.github.io/helm-intro.html
;; http://tuhdo.github.io/helm-projectile.html

;; Initialize helm and helm-cmd-t
(require 'helm-config)
(require 'helm-cmd-t)

;; Initialize projectile
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Other cool things:
;; helm-top
;; helm-surfraw
;; helm-color

;; Default helm-command-prefix key
(global-unset-key (kbd "C-x c"))
(define-key global-map (kbd "C-c h") 'helm-command-prefix)

(setq helm-split-window-in-side-p     nil ; don't open helm buffer inside current window
      helm-buffers-fuzzy-matching     t ; fuzzy matching buffer names when non--nil
      helm-ff-search-library-in-sexp  t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount              8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t) ; use recent history

;; Find files fast (somewhat)
(define-key global-map (kbd "C-,") 'helm-cmd-t)

;; Helm kill ring
(define-key global-map (kbd "C-c k") 'helm-show-kill-ring)

;; Use helm for listing symbols
(define-key global-map (kbd "C-c i") 'helm-semantic-or-imenu)

;; Use helm for finding files
(define-key global-map (kbd "C-x C-f") 'helm-find-files)

;; Use helm for M-x
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x C-m") 'helm-M-x)

;; Use helm for switching buffers
(define-key global-map (kbd "C-x b") 'helm-mini)
(define-key global-map (kbd "C-x C-b") 'helm-buffers-list)

;; Better keybinding for helm occur
(define-key global-map (kbd "C-c h o") 'helm-occur)

;; Global mark rings
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

;; Helm resume - Resume with previous helm command and patterns
;; helm-command-prefix o

;; Keybindings while helm is active
(define-key helm-map (kbd "C-w") 'backward-kill-word)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-buffer-map (kbd "M-d") 'helm-buffer-run-kill-buffers)

;; Use ack-grep instead of grep
(setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
      helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f")

;; Enable man page at point with helm-man-woman (C-c h m)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;; Remove additional sources from helm-projectile
(setq helm-projectile-sources-list '(helm-source-projectile-projects
                                     helm-source-projectile-files-list))

;; Executed after switching to a project
(setq projectile-switch-project-action 'helm-projectile)

;; Enable caching for projectile
;; However, once this is enabled only the currently-cached files are shown.
; (setq projectile-enable-caching t)

;; Useful projectile commands
;; C-c p p helm-projectile-switch-project
;; C-c p f helm-projectile-find-file
;; C-c p i projectile-invalidate-cache
;; C-c p z projectile-cache-current-file,
;;         projectile-purge-file-from-cache
;;         projectile-purge-dir-from-cache
;; C-c p s a helm-projectile-ack

;; Virtual directories

;(ad-remove-advice 'helm-cmd-t-root-data 'after 'adv-expand-file-name)

(defadvice helm-cmd-t-root-data (after ad-expand-file-name)
  "expand the repo-root returned"
  (unless (eq ad-return-value nil)
    (setq ad-return-value
	  `(,(car ad-return-value) . ,(expand-file-name (cdr ad-return-value))))))

(ad-activate 'helm-cmd-t-root-data)

(provide 'ameyp-helm)
