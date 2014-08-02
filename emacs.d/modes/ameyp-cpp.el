(add-to-list 'load-path "~/.emacs.d/packages/ascope")
(add-to-list 'load-path "~/.emacs.d/packages/clancs")

;; Ascope configuration
(require 'ascope)
(setq ascope-name-line-width -24)

(add-to-list 'auto-mode-alist '("\\.h" . c++-mode))

;; Add cscope.files to helm-cmd-t's repo types
(if (eq system-type 'windows-nt)
    (add-to-list 'helm-cmd-t-repo-types '("cscope" "cscope.files" "cd %d && type cscope.files"))
  (add-to-list 'helm-cmd-t-repo-types '("cscope" "cscope.files" "cd %d && cat cscope.files")))

;; Clancs!
;(require 'clancs)
;(clancs-init)

(defun create-cscope-files (dir-name)
  (interactive "DDirectory: ")

  (call-process-shell-command
   (format "cd %s; rm cscope.*" dir-name)
   nil " *Cscope Generation Output*" t)

  (create-file-list dir-name '(".") '("*.cpp" "*.[chCH]" "*.cxx") "cscope.files")
  (ascope-create-database dir-name)
  (kill-buffer " *Cscope Generation Output*"))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Javadoc-style comments
	    (setq-local c-block-comment-prefix "* ")

	    ;; Activate helm-dash docset
	    (setq-local helm-dash-docsets '("C++"))

	    ;; Use helm for filtering cscope.files
	    (setq-local helm-for-files-preferred-list '(helm-source-files-in-cscope))

	    ;; Ascope bindings
	    (define-key c++-mode-map (kbd "M-.") 'cscope-find-global-definition)
	    (define-key c++-mode-map (kbd "M-p") 'cscope-find-functions-calling-this-function)
	    ;(define-key c++-mode-map (kbd "RET") 'c-indent-new-comment-line)

	    ;; Disable company-clang for now
	    (setq-local company-backends '(company-capf))

	    ;; ---- Indentation style
	    (c-set-style "bsd")
	    (setq-local tab-width 4)
	    (setq-local c-basic-offset 4)

	    ;; indent using spaces
	    (setq-local indent-tabs-mode nil)))

(provide 'ameyp-cpp)
