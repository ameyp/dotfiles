(require 'web-mode)

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
(add-to-list 'auto-mode-alist '("\\.ejs$" . web-mode))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("freemarker"    . "\\.html\\'")))

;(defadvice web-mode-highlight-part (around tweak-jsx activate)
;  (if (equal web-mode-content-type "jsx")
;      (let ((web-mode-enable-part-face nil))
;        ad-do-it)
;    ad-do-it))

;; Indentation settings
;; Markup
(setq web-mode-markup-indent-offset 2)
;; CSS
(setq web-mode-css-indent-offset 2)
;; Script
(setq web-mode-code-indent-offset 2)
;; JSX
(setq jsx-indent-level 2)

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
