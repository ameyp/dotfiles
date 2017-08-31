(require 'web-mode)
(require 'flycheck)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ftl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

(setq web-mode-engines-alist
      '(("freemarker"    . "\\.html\\'")))

(add-hook 'web-mode-hook
          (lambda ()
            ;; Indentation settings
            ;; Enough with the tabs
            (setq-default indent-tabs-mode nil)
            ;; Markup
            (setq web-mode-markup-indent-offset 2)
            ;; CSS
            (setq web-mode-css-indent-offset 2)
            ;; Script
            (setq web-mode-code-indent-offset 4)
            ;; JSX
            (setq jsx-indent-level 2)
            ;; Enable flycheck
            (add-hook 'javascript-mode-hook 'flycheck-mode)))

(flycheck-def-args-var flycheck-javascript-flow-args javascript-flow)
(customize-set-variable 'flycheck-javascript-flow-args '("status"))

(flycheck-define-checker javascript-flow
    "A JavaScript syntax and style checker using Flow.
See URL `http://flowtype.org/'."
    :command (
              "flow"
              (eval flycheck-javascript-flow-args)
              "--old-output-format"
              "--color=never"
              source-original)
    :error-patterns
    ((error line-start
            (file-name)
            ":"
            line
            ":"
            (minimal-match (one-or-more not-newline))
            ": "
            (message (minimal-match (and (one-or-more anything) "\n")))
            line-end))
    :modes (web-mode js2-mode))

(add-to-list 'flycheck-checkers 'javascript-flow t)

(defun ameyp-web/html-encode ()
  (interactive)
  (let* ((regionp (region-active-p))
         (beg (and regionp (region-beginning)))
         (end (and regionp (region-end)))
         (buf (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer) nil t)
      (rename-buffer " *html encode*" t)
      (insert-buffer-substring buf beg end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      ;(goto-char (point-min))
      ;(replace-string "\"" "&quot;")
      (clipboard-kill-region (point-min) (point-max)))))

(defun ameyp-web/html-decode ()
  (interactive)
  (let* ((regionp (region-active-p))
         (beg (and regionp (region-beginning)))
         (end (and regionp (region-end)))
         (buf (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer) nil t)
      (rename-buffer " *html encode*" t)
      (insert-buffer-substring buf beg end)
      (goto-char (point-min))
      (replace-string "&amp;" "&")
      (goto-char (point-min))
      (replace-string "&lt;" "<")
      (goto-char (point-min))
      (replace-string "&gt;" ">")
      (goto-char (point-min))
      (replace-string "&quot;" "\"")
      (clipboard-kill-region (point-min) (point-max)))))

(provide 'ameyp-web)
