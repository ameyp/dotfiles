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

(setq visible-bell nil
      ring-bell-function 'ignore)

;; The order of the next three segments is *extremely* important.

;; 1. Load theme
(use-package brutalist-theme
  :config
  (load-theme 'brutalist))

;; 2. Customize theme
(custom-theme-set-faces
 'brutalist
 `(default ((t (:family "Hack"))))
 `(fixed-pitch ((t (:family "Hack"))))
 `(helm-bookmark-file ((t (:inherit default))))
 `(helm-buffer-file ((t (:inherit default))))
 `(helm-ff-file ((t (:inherit default))))
 `(helm-keyword-face ((t (:inherit default))))
 `(helm-selection ((t (:inherit whitespace-line))))
 `(lsp-face-highlight-read ((t (:inherit lazy-highlight :underline nil))))
 `(lsp-face-highlight-write ((t (:inherit lazy-highlight :underline nil :weight bold))))
 `(mode-line-inactive ((t (:inherit default :foreground "#666666" :distant-foreground "#666666"))))
 )

;; 3. Set font size and line spacing
(let ((face-height
       (cond ((eq system-type 'darwin)
              (cond
               ;; Work machine
               ((string-match "\.amazon\.com" (system-name)) 150)
               ;; Personal macOS
               (t 130)))
             ;; Default (linux/windows)
             (t 105))))
  (progn (set-face-attribute 'default nil :height face-height)
         (set-face-attribute 'mode-line nil :height face-height)
         (set-face-attribute 'mode-line-inactive nil :height face-height)))

(setq-default line-spacing 3)

;; 4. Modeline customization
(use-package delight)

(provide 'ameyp-gui)
