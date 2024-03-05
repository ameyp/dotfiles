;; Automatic formatting on save
(use-package apheleia
  :ensure t
  :config
  ;; Respect the prettier config, not whatever flags are set in emacs.
  (setq apheleia-formatters-respect-indent-level nil)
  (apheleia-global-mode +1)
  ;; Don't explicitly specify the parser as html when prettifying html.
  (setf (alist-get 'prettier-html apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath))
  )

;; https://github.com/rymndhng/jest-test-mode
(use-package jest-test-mode
  :ensure t
  :commands jest-test-mode
  :hook (typescript-mode js-mode typescript-tsx-mode))

;; Not working, getting an internal server error.
;; (use-package lsp-tailwindcss
;;   :config
;;   (setq lsp-tailwindcss-add-on-mode t))

(provide 'ameyp-typescript)
