;; Awesome guide:
;; http://tuhdo.github.io/helm-intro.html
;; Other cool things:
;; helm-top
;; helm-color

(use-package helm
  :init
  (custom-set-variables '(helm-command-prefix-key "C-c h"))
  :config
  (setq helm-split-window-in-side-p           nil ; don't open helm buffer inside current window
	helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	helm-ff-file-name-history-use-recentf t ; use recent history
	)
  :bind (("C-c k" . 'helm-show-kill-ring) ;; Helm kill ring
	 ("C-c i" . 'helm-semantic-or-imenu) ;; Use helm for listing symbols
	 ("C-x C-f" . 'helm-find-files) ;; Use helm for finding files
	 ("M-x" . 'helm-M-x) ;; Use helm for M-x
	 ("C-x C-m" . 'helm-M-x) ;; Use helm for M-x
	 ("C-x b" . 'helm-mini) ;; Use helm for mini buffer switching
	 ("C-c h o" . 'helm-occur) ;; Better keybinding for helm occur
	 ("C-h SPC" . 'helm-all-mark-rings) ;; Global mark rings
	 :map helm-map
	 ("C-w" . 'backward-kill-word)
	 ("C-z" . 'helm-select-action)
	 ("<tab>" . 'helm-execute-persistent-action)
	 ("C-i" . 'helm-execute-persistent-action)  ;; make TAB work in terminal
	 ))

(use-package helm-buffers
  :config
  (setq helm-buffers-fuzzy-matching t) ; fuzzy matching buffer names when non--nil
  :bind (("C-x C-b" . 'helm-buffers-list) ;; Use helm for big buffer switching
	 :map helm-buffer-map
	 ("M-d" . 'helm-buffer-run-kill-buffers)))

(use-package helm-grep
  :after (helm)
  :config
  ;; Use ack-grep instead of grep
  (setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
	helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))

(use-package helm-man
  :after (helm)
  :config
  ;; Enable man page at point with helm-man-woman (C-c h m)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages))

;; Useful projectile commands
;; C-c p p helm-projectile-switch-project
;; C-c p f helm-projectile-find-file
;; C-c p i projectile-invalidate-cache
;; C-c p z projectile-cache-current-file,
;;         projectile-purge-file-from-cache
;;         projectile-purge-dir-from-cache
;; C-c p s a helm-projectile-ack
(use-package projectile
  :after (helm)
  :config
  (projectile-mode)
  (setq projectile-enable-caching nil
	projectile-completion-system 'helm
	projectile-switch-project-action 'helm-projectile))

(use-package helm-projectile
  :after (projectile)
  :ensure
  :bind ("C-," . helm-projectile-find-file)
  :config
  (helm-projectile-on)
  (setq helm-projectile-sources-list '(helm-source-projectile-projects
				       helm-source-projectile-files-list)))

(provide 'ameyp-helm)
