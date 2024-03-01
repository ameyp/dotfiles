;; Install:
; gem install pry pry-nav pry-stack_explorer termios

(add-to-list 'load-path "~/.emacs.d/modes/emacs-pry")

;;; Enhanced ruby mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;(setq enh-ruby-program "(path-to-ruby1.9)/bin/ruby")

(autoload 'robe-mode "robe" "Minor mode for ruby files" t)
(autoload 'robe-ac-setup "robe-ac" "Minor mode for ruby files" t)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

;; Auto completion
;; Company-mode: http://company-mode.github.com/
; (push 'company-robe company-backends)

;;; Yard mode, alternative documentation tool
;; Commented for use with enh-ruby-mode
; (add-hook 'ruby-mode-hook 'yard-mode)
(autoload 'yard-mode "yard-mode" "Minor mode for ruby files" t)
(add-hook 'enh-ruby-mode-hook 'yard-mode)

;; Eldoc support, so that the expected syntax for the tag beneath the cursor
;; is displayed in the minibuffer
; (add-hook 'ruby-mode-hook 'eldoc-mode)

;;; For running pry from inside emacs
(add-hook 'enh-ruby-mode-hook
	  (lambda ()
	    ;; Activate helm-dash docsets
	    (setq-local helm-dash-docsets '("Ruby"))

	    ;; Enable pry
	    (require 'pry)
	    (define-key enh-ruby-mode-map (kbd "S-<f9>") 'pry-intercept)
	    (define-key enh-ruby-mode-map (kbd "<f9>") 'pry-intercept-rerun)))

(provide 'ameyp-ruby)
