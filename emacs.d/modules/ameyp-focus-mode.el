;; Adapted from https://protesilaos.com/codelog/2020-07-16-emacs-focused-editing/, seems like prot no longer uses this.

(use-package olivetti
  :ensure
  :defer
  :diminish
  :config
  (setq olivetti-body-width 80)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)

  (define-minor-mode ameyp/olivetti-mode
    "Toggle buffer-local `olivetti-mode' with additional parameters.

Fringes are disabled.  The modeline is hidden, except for
`prog-mode' buffers (see `ameyp/hidden-mode-line-mode').  The
default typeface is set to a proportionately-spaced family.
The cursor becomes a blinking bar, per `ameyp/cursor-type-mode'."
    :init-value nil
    :global nil
    (if ameyp/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
          (setq-local line-spacing 0.4)
          (ameyp/cursor-type-mode 1)
          (ameyp/scroll-centre-cursor-mode 1)
          (focus-mode 1)
          (advice-add 'bounds-of-thing-at-point :around #'ameyp/focus-mode-markdown-bounds-advice)
          (unless (derived-mode-p 'prog-mode)
            (ameyp/hidden-mode-line-mode 1)))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
      (setq-local line-spacing 3) ; This appears to be the default value
      (ameyp/cursor-type-mode -1)
      (ameyp/scroll-centre-cursor-mode -1)
      (focus-mode -1)
      (advice-remove 'bounds-of-thing-at-point #'ameyp/focus-mode-markdown-bounds-advice)
      (unless (derived-mode-p 'prog-mode)
        (ameyp/hidden-mode-line-mode -1))))
  :bind ("C-c o" . ameyp/olivetti-mode))

(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode ameyp/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if ameyp/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local)))))



;; Line numbers disabled by default.
(use-package display-line-numbers
  :ensure nil
  :bind
  ("<f7>" . ameyp/display-line-numbers-mode)
  :config
  ;; Set absolute line numbers.  A value of "relative" is also useful.
  (setq-default display-line-numbers-type t)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t)
  (setq-default hl-line-mode -1)

  (define-minor-mode ameyp/display-line-numbers-mode
    "Toggle `display-line-numbers-mode' and `hl-line-mode'."
    :init-value nil
    :global nil
    (if ameyp/display-line-numbers-mode
        (display-line-numbers-mode 1)
      (display-line-numbers-mode -1))))


(use-package frame
  :commands ameyp/cursor-type-mode
  :config
  (setq-default cursor-type 'box)
  (setq-default cursor-in-non-selected-windows '(bar . 2))
  (setq-default blink-cursor-blinks 50)
  (setq-default blink-cursor-interval nil) ; 0.75 would be my choice
  (setq-default blink-cursor-delay 0.2)

  (blink-cursor-mode -1)

  (define-minor-mode ameyp/cursor-type-mode
    "Toggle between static block and pulsing bar cursor."
    :init-value nil
    :global t
    (if ameyp/cursor-type-mode
        (progn
          (setq-local blink-cursor-interval 0.75
                      cursor-type '(bar . 2)
                      cursor-in-non-selected-windows 'hollow)
          (blink-cursor-mode 1))
      (dolist (local '(blink-cursor-interval
                       cursor-type
                       cursor-in-non-selected-windows))
        (kill-local-variable `,local))
      (blink-cursor-mode -1))))


(defun ameyp/focus-mode-markdown-bounds-advice (orig-fun &rest args)
  "Provide custom behavior for bounds-of-thing-at-point in modes like markdown-mode where
I want a sentence to be defined differently, by accounting for things like list items."
  (if (derived-mode-p 'markdown-mode)
      (let ((thing (car args)))
        (cond
         ((eq thing 'sentence)
          (let ((current-element
                 (save-excursion
                   (beginning-of-line)
                   (cond
                    ;; Header case
                    ((looking-at "^#+ ")
                     'header)
                    ;; List item case
                    ((looking-at "^\\s-*[-*+] \\|^\\s-*[0-9]+\\. ")
                     'list-item)
                    ;; Add any future extensions here.
                    ))))
            (cond
             ((eq current-element 'header)
              (cons (save-excursion
                      (beginning-of-line)
                      (point))
                    (save-excursion
                      (forward-line)
                      (point))))
             ((eq current-element 'list-item)
              (cons (save-excursion
                      (beginning-of-line)
                      (point))
                    (save-excursion
                      (forward-line)
                      (while (looking-at "^\\s-+\\(?:[^-*+.]\\|$\\)")
                        (forward-line))
                      (point))))
             ;; Regular sentence case - fall back to default behavior
             (t
              (apply orig-fun args)
              ))))
         ;; Fall back to default behavior for other things
         (t
          (apply orig-fun args))))
    ;; Default behavior for non-markdown modes
    (apply orig-fun args)))


(use-package focus
  :ensure t
  :config
  (set-face-attribute 'focus-focused nil :family "iA Writer Duospace" :height 190)
  (set-face-attribute 'focus-unfocused nil :family "iA Writer Duospace" :height 190 :foreground "#666666")
  (setq focus-mode-to-thing '((text-mode . sentence)
                              (markdown-mode . sentence))))

(provide 'ameyp-focus-mode)
