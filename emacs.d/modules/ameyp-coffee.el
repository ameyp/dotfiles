(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  (setq helm-dash-docsets '("CoffeeScript" "JavaScript" "NodeJS")
	tab-width 4
	coffee-tab-width 4))

(provide 'ameyp-coffee)
