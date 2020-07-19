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
   (quote
    ("10222cb6df1b8ba78c39183e90e9d8b952164454b44ff919cfeab624f0049b6c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "5ed25f51c2ed06fc63ada02d3af8ed860d62707e96efc826f4a88fd511f45a1d" "de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" default)))
 '(haskell-process-auto-import-loaded-modules t t)
 '(haskell-process-log t t)
 '(haskell-process-suggest-remove-import-lines t t)
 '(helm-command-prefix-key "C-c h")
 '(package-selected-packages
   (quote
    (eldoc-mode delight git-rebase-mode git-commit-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
