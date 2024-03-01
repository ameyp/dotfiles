(add-hook 'rst-mode-hook
          (lambda ()
            (progn (turn-on-auto-fill)
                   (setq-local fill-column 100))))
