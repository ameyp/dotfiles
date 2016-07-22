(autoload 'coffee-mode "coffee-mode" "Major mode for coffeescript files" t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(add-hook 'coffee-mode-hook
          (lambda ()
	    ;; Activate helm-dash docset
	    (setq-local helm-dash-docsets '("CoffeeScript" "JavaScript" "NodeJS"))

	    ;; Set tab-width
            (set (make-local-variable 'tab-width) 4)
            (set 'coffee-tab-width 4)))

(provide 'ameyp-coffee)
