;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  :bind (
         ("M-<up>" . vertico-previous-group)
         ("M-<down>" . vertico-next-group)
         :map minibuffer-local-map
         ("C-w" . backward-kill-word))
  )

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Use Consult
(use-package consult
  :bind (
         ("C-x b" . consult-buffer)
         ("C-c p s" . consult-ripgrep)
         ("C-c k" . consult-yank-from-kill-ring)
         )
  :config
  (setq consult-ripgrep-args
        (concat "rg "
                "--null "
                "--line-buffered "
                "--color=never "
                "--max-columns=1000 "
                "--path-separator / "
                "--smart-case "
                "--no-heading "
                "--with-filename "
                "--line-number "
                "--search-zip "
                "--hidden "
                "--glob=!.git/"))

  (use-package marginalia
    ;; Bind `marginalia-cycle' locally in the minibuffer.
    :bind
    (:map minibuffer-local-map
          ("M-A" . marginalia-cycle))
    ;; To make the binding available in the *Completions* buffer,
    ;; add it to the `completion-list-mode-map'.
    (:map completion-list-mode-map
          ("M-A" . marginalia-cycle))


    ;; The :init section is always executed.
    :init

    ;; Marginalia must be activated in the :init section of use-package such that
    ;; the mode gets enabled right away. Note that this forces loading the
    ;; package.
    (marginalia-mode))

  (use-package embark)

  (use-package embark-consult
    :bind (
           ("C-c C-o" . embark-export)))

  (use-package consult-projectile
    :bind (
           ("C-," . consult-projectile-find-file)
           ))

  (provide 'ameyp-vertico)
