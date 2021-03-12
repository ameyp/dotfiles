;; ---- whitespace-mode ---------------------
(use-package whitespace)

(use-package eldoc
  :delight eldoc-mode)

;; expand-region
(use-package expand-region
  :bind (("C-=" . 'er/expand-region)
         ("C-+" . 'er/contract-region)))

(require 'bind-key)
(bind-keys* ("C-w" . backward-kill-word)
            ("C-x C-k" . kill-region)
            ("M-s" . isearch-forward-regexp)
            ("M-r" . isearch-backward-regexp))

;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; Show matching paren
  (show-paren-mode))

;; Replace selected text while mark is active
(delete-selection-mode 1)

;; Tabs are evil
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; Before saving:
;; 1. Untabify if indent-tabs-mode is off
;; 2. Delete trailing whitespace
;; 3. Set line-endings to unix
(setq delete-trailing-lines nil)
(add-hook 'before-save-hook
          (lambda ()
            (set-buffer-file-coding-system 'utf-8-unix)
            (delete-trailing-whitespace)
            (if (not indent-tabs-mode)
                (untabify (point-min) (point-max)))))

;; Delete duplicate lines
(defun ameyp-text/uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun ameyp-text/uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (ameyp-text/uniquify-all-lines-region (point-min) (point-max)))

(provide 'ameyp-text)
