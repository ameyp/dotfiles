(autoload 'sws-mode "sws-mode" "Major mode for significant white-space files" t)
(autoload 'jade-mode "jade-mode" "Major mode for jade files" t)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(provide 'ameyp-jade)
