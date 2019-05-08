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
 '(helm-command-prefix-key "C-c h")
 '(package-selected-packages
   (quote
    (terraform-mode pry whitespace-toggle-options whitespace-mode company-go use-package json-snatcher json-mode go-mode magit yard-mode yaml-mode web-mode smartparens rust-mode robe rainbow-delimiters package-build multiple-cursors markdown-mode lua-mode js2-mode jade-mode inf-ruby helm-projectile helm-dash helm-company helm-cmd-t helm haxe-mode haskell-mode glsl-mode flycheck find-file-in-project expand-region enh-ruby-mode company-tern company coffee-mode cmake-mode cider browse-kill-ring))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
