(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package pyenv-mode)

(use-package eglot
  :ensure t
  :hook (python-mode . (lambda ()
                         (eglot-ensure)
                         (company-mode))))

(use-package auto-virtualenv
  :ensure t
  :init
  (use-package pyvenv
    :ensure t)
  :config
  (auto-virtualenv-setup)
  )

(use-package python-pytest)

(provide 'ameyp-python)
