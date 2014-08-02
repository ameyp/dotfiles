(autoload 'haskell-mode "haskell-mode" "Major mode for haskell files" t)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

(add-hook 'haskell-mode-hook
          (lambda ()
	    ;; Activate helm-dash docset
	    (setq-local helm-dash-docsets '("Haskell"))

	    ;; Enable indentation
					;(turn-on-haskell-simple-indent)
					;(turn-on-haskell-indent)
	    (turn-on-haskell-indentation)

	    ;; Set up interactive mode
	    (custom-set-variables
	     '(haskell-process-suggest-remove-import-lines t)
	     '(haskell-process-auto-import-loaded-modules t)
	     '(haskell-process-log t))

	    ;; Haskell keybindings
	    (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
	    (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
	    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
	    (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
	    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
	    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
	    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
	    (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
	    (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
	    (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

	    ;; Cabal keybindings
	    ;(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
	    ;(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
	    ;(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
	    ;(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
	    ))

(provide 'ameyp-haskell)
