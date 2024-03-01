(use-package web-mode
  :mode ("\\.phtml\\'"
         "\\.php\\'"
         "\\.[gj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.ftl\\'"
         "\\.html?\\'"
         "\\.jsx?$"
         "\\.json$"
         "\\.ejs$")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))
        web-mode-engines-alist '(("freemarker"    . "\\.html\\'"))
        indent-tabs-mode nil
        web-mode-markup-indent-offset 4 ;; Markup
        web-mode-css-indent-offset 4  ;; CSS
        web-mode-code-indent-offset 4  ;; Script tags
        web-mode-attr-indent-offset 4 ;; So that HTML/JSX attributes aren't aligned at their names
        web-mode-enable-auto-quoting nil ;; So that attributes don't get an automatic quote after =
        jsx-indent-level 4)
  :hook
  (javascript-mode . flycheck-mode))

(use-package lsp-mode
  :hook (web-mode . lsp-deferred))

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
      (rename-buffer " *html decode*" t)
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
