(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setq helm-dash-docsets '("Python 3")))

(provide 'ameyp-python)
