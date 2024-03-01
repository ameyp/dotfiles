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

(global-display-line-numbers-mode)

;; The order of the next three segments is *extremely* important.

;; ;; 1. Load theme
;; (use-package brutalist-theme
;;   :config
;;   (load-theme 'brutalist))

;; (setq custom--inhibit-theme-enable nil)

;; ;; 2. Customize theme
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
       (cond ((eq system-type 'darwin)
              (cond
               ;; Work machine
               ((string-match "\.tetra\.ai" (system-name)) 150)
               ;; Personal macOS
               (t 130)))
             ;; Default (linux/windows)
             (t 115))))
  (progn (set-face-attribute 'default nil :height face-height)
         (set-face-attribute 'mode-line nil :height face-height)
         (set-face-attribute 'mode-line-inactive nil :height face-height)
         (set-face-attribute 'font-lock-type-face nil :underline nil)))

(setq-default line-spacing 3)

;; 4. Modeline customization
(use-package delight)

(provide 'ameyp-gui)
