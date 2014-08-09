;; (add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.clj$" . cider-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs$" . cider-mode))

;; (autoload 'clojure-mode "clojure-mode" "Major mode for editing clojure files" t)
;; (autoload 'cider-mode "cider-mode" "Minor mode for clojure repl" t)

;(add-hook 'clojure-mode-hook 'cider-mode)

;; Turn on eldoc mode
(autoload 'cider-eldoc "cider-eldoc" "Eldoc mode for clojure files" t)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Enable smartparens
;; (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)

;; Enable rainbow delimiters
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(add-hook 'cider-mode-hook
          (lambda ()
	    ;; hide nrepl buffers in cider-mode
            (setq nrepl-hide-special-buffers t)))

(provide 'ameyp-clojure)
