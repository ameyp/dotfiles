;; Remove toolbar, menubar and scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; Instead of an annoying ding as a bell, flash the modeline instead.
;; Disabled because it still causes the rest of the window to flash on windows.
;; (defun flash-mode-line ()
;;   (invert-face 'mode-line)
;;   (run-with-timer 0.5 nil #'invert-face 'mode-line))
;;
;; (setq visible-bell nil
;;       ring-bell-function 'flash-mode-line)

;; https://www.emacswiki.org/emacs/UndoTree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; Stop undo-tree files from polluting all directories.
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;; Stop backups from polluting all directories
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;; Make file-visiting buffers automatically save to their files after
;; an interval of idleness instead of saving such buffers to a separate file.
(setq auto-save-visited-interval 60) ; Interval is in seconds
(setq remote-file-name-inhibit-auto-save-visited t)
(auto-save-visited-mode 1)

(setq visible-bell nil
      ring-bell-function 'ignore)

;; Resizes windows on splitting and unsplitting
(defadvice split-window-right (after balance-out-windows activate)
  (balance-windows))
(defadvice split-window-below (after balance-out-windows activate)
  (balance-windows))
(defadvice find-file-other-window (after balance-out-windows activate)
  (balance-windows))
(defadvice delete-window (after balance-out-windows activate)
  (balance-windows))

;; TODO Use this for centering text based on window size.
;; (set-window-margins (frame-selected-window) 50 50)

;; Deleted selected text upon text insertion
(use-package delsel
  :ensure nil ; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; The order of the next three segments is *extremely* important.

;; 1. Load theme
(use-package ef-themes
  :preface
  (setq ef-themes-org-blocks 'gray-background)
  :config
  ;; Convenience function for quickly using my currently-chose light theme.
  (defun ameyp-use-dark-theme ()
    (interactive)
    (load-theme 'ef-night :no-confirm))

  ;; Convenience function for quickly using my currently-chose dark theme.
  (defun ameyp-use-light-theme ()
    (interactive)
    (load-theme 'ef-day :no-confirm))

  ;; Load dark theme by default.
  (ameyp-use-dark-theme)
  )

;; 2. Set font family, font size and line spacing
(let ((mono-spaced-font "NotoSansM Nerd Font Mono")
      (proportionately-spaced-font "NotoSansM Nerd Font Propo")
      (face-height
       (if (eq system-type 'darwin)
           ;; macOS
           150
         ;; Default (linux/windows)
         115)))
  (progn (set-face-attribute 'default nil :family mono-spaced-font :height face-height)
         (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
         (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0)
         (set-face-attribute 'mode-line nil :family mono-spaced-font :height 1.0)
         (set-face-attribute 'mode-line-inactive nil :family mono-spaced-font :height 1.0)
         (set-face-attribute 'font-lock-type-face nil :family mono-spaced-font :underline nil)
         (set-face-attribute 'font-lock-comment-face nil :family mono-spaced-font :italic nil)
         ))

(setq-default line-spacing 3)

;; Ediff settings
(setq ediff-keep-variants nil)
(setq ediff-make-buffers-readonly-at-startup nil)
(setq ediff-merge-revisions-with-ancestor t)
(setq ediff-show-clashes-only t)

(setq ediff-split-window-function 'split-window-horizontally)
;; instead of creating a new frame for ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; spacious-padding
(use-package spacious-padding
  :config
  ;; These is the default value, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 20
           :scroll-bar-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line nil)

  (spacious-padding-mode 1)
  )

(use-package pulsar
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (pulsar-global-mode 1)

  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  (add-hook 'next-error-hook #'pulsar-pulse-line-red)
  (add-hook 'next-error-hook #'pulsar-recenter-top)
  (add-hook 'next-error-hook #'pulsar-reveal-entry)

  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-red))

;; 4. Modeline
(use-package ameyp-modeline
  :ensure t
  :commands ameyp/hidden-mode-line-mode
  :config
  (setq mode-line-compact nil) ; Emacs 28
  (setq-default mode-line-format
                '("%e"
                  ameyp-modeline-kbd-macro
                  ameyp-modeline-narrow
                  ameyp-modeline-buffer-status
                  ameyp-modeline-input-method
                  ameyp-modeline-buffer-identification
                  "  "
                  ameyp-modeline-major-mode
                  ameyp-modeline-process
                  "  "
                  ameyp-modeline-vc-branch
                  "  "
                  ameyp-modeline-eglot
                  "  "
                  ameyp-modeline-flymake))

  (define-minor-mode ameyp/hidden-mode-line-mode
    "Toggle modeline visibility in the current buffer."
    :init-value nil
    :global nil
    (if ameyp/hidden-mode-line-mode
        (setq-local mode-line-format nil)
      (kill-local-variable 'mode-line-format)
      (force-mode-line-update))))

;; Set up icons
(use-package nerd-icons :ensure t)

(provide 'ameyp-gui)

