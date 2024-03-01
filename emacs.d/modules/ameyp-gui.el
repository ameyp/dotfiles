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

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))

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

;; Line numbers turned on by default, except in certain major modes
(require 'display-line-numbers)

(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))

;; Makes it so that the line number gutter is wide enough for the max line number in the visited file.
;; Otherwise, the default behavior makes it wide enough for the max visible line number.
(defun display-line-numbers-equalize ()
  "Equalize The width"
  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))

(add-hook 'find-file-hook 'display-line-numbers-equalize)

(global-display-line-numbers-mode)

;; The order of the next three segments is *extremely* important.

;; 1. Load theme
(use-package modus-themes
  :preface
  (setq modus-themes-org-blocks 'gray-background)
  :config
  (load-theme 'modus-vivendi-deuteranopia :no-confirm))

;; (setq custom--inhibit-theme-enable nil)

;; 2. Customize theme
;; (custom-theme-set-faces
;;  'brutalist
;;  `(default ((t (:family "Hack Nerd Font"))))
;;  `(fixed-pitch ((t (:family "Hack Nerd Font"))))
;;  `(helm-bookmark-file ((t (:inherit default))))
;;  `(helm-buffer-file ((t (:inherit default))))
;;  `(helm-ff-file ((t (:inherit default))))
;;  `(helm-keyword-face ((t (:inherit default))))
;;  `(helm-selection ((t (:inherit whitespace-line))))
;;  `(lsp-face-highlight-textual ((t (:inherit lazy-highlight :underline nil))))
;;  `(lsp-face-highlight-read ((t (:inherit lazy-highlight :underline nil))))
;;  `(lsp-face-highlight-write ((t (:inherit lazy-highlight :underline nil :weight bold))))
;;  `(mode-line-inactive ((t (:inherit default :box '(:width 1) :foreground "#666666" :background "#fffff8"))))
;;  )

;; 3. Set font size and line spacing
(let ((face-height
       (if (eq system-type 'darwin)
           ;; macOS
           150
         ;; Default (linux/windows)
         115)))
  (progn (set-face-attribute 'default nil :height face-height)
         (set-face-attribute 'mode-line nil :height face-height)
         (set-face-attribute 'mode-line-inactive nil :height face-height)
         (set-face-attribute 'font-lock-type-face nil :underline nil)))

(setq-default line-spacing 3)

;; 4. Modeline customization
(use-package delight)

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
           :right-divider-width 30
           :scroll-bar-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))

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

(provide 'ameyp-gui)
