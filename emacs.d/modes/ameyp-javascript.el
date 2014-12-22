(autoload 'js2-mode "js2-mode" "Major mode for javascript files" t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook
	  (lambda ()
	    ;; Scan the file for nested code blocks
	    (imenu-add-menubar-index)

	    ;; Activate the folding mode
	    (hs-minor-mode t)

	    ;; Activate helm-dash docsets
	    (setq-local helm-dash-docsets '("JavaScript" "jQuery" "NodeJS"))

	    ;; indentation
	    (setq js2-consistent-level-indent-inner-bracket-p t)
	    (setq js2-pretty-multiline-decl-indentation-p t)

	    (setq js2-cleanup-whitespace t)
	    (setq js2-enter-indents-newline t)
	    (setq js2-indent-on-enter-key t)
	    (setq js2-mirror-mode nil)
	    (setq js2-mode-show-parse-errors t)))

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (let ((btext (replace-regexp-in-string
                          ": *true" " "
                          (replace-regexp-in-string "[\n\t ]+" " "
						    (buffer-substring-no-properties 1 (buffer-size)) t t))))
              (setq js2-additional-externs
                    (split-string
                     (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext)
			 (match-string-no-properties 1 btext) "") " *, *" t)))))

(provide 'ameyp-javascript)
