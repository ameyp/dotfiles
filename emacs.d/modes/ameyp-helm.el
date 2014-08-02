;; Initialize helm and helm-cmd-t
(require 'helm-config)
(require 'helm-cmd-t)
(define-key global-map (kbd "C-,") 'helm-cmd-t)

;; Use helm for listing symbols
(define-key global-map (kbd "C-c i") 'helm-imenu)

;; Use helm for finding files
(define-key global-map (kbd "C-x C-f") 'helm-find-files)

;; Use helm for switching buffers
(define-key global-map (kbd "C-x b") 'helm-buffers-list)
(define-key global-map (kbd "C-x C-b") 'helm-buffers-list)

;; Keybindings while helm is active
(define-key helm-map (kbd "C-w") 'backward-kill-word)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-buffer-map (kbd "M-d") 'helm-buffer-run-kill-buffers)

;(ad-remove-advice 'helm-cmd-t-root-data 'after 'adv-expand-file-name)

(defadvice helm-cmd-t-root-data (after ad-expand-file-name)
  "expand the repo-root returned"
  (unless (eq ad-return-value nil)
    (setq ad-return-value
	  `(,(car ad-return-value) . ,(expand-file-name (cdr ad-return-value))))))

(ad-activate 'helm-cmd-t-root-data)

(provide 'ameyp-helm)
