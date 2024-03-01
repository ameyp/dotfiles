(put 'set-goal-column 'disabled nil)

;; Disable autosave
(setq auto-save-default nil)

;; Disable backup
(setq backup-inhibited t)

;; ---- unzip files in-line from dired ------
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.txt.gz\\'")))

;; Close completions buffer when done with it
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
