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
    (cider-eldoc cider-mode helm-man helm-grep helm-buffers brutalist-theme tern zig-mode org-ref solarized-theme doom-modeline all-the-icons lsp-java swift-mode lsp-sourcekit lsp-ui flycheck-rust company-lsp terraform-mode pry whitespace-toggle-options whitespace-mode company-go use-package json-snatcher json-mode go-mode magit yard-mode yaml-mode web-mode smartparens rust-mode robe rainbow-delimiters package-build multiple-cursors markdown-mode lua-mode js2-mode jade-mode inf-ruby helm-projectile helm-dash helm-company helm-cmd-t helm haxe-mode haskell-mode glsl-mode flycheck find-file-in-project expand-region enh-ruby-mode company-tern company coffee-mode cmake-mode cider browse-kill-ring))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
