(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :ensure
  :config
  (setq helm-dash-docsets '("Python 3")))

(provide 'ameyp-python)
