;; Remove toolbar, menubar and scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; Set font size differently based on OS.
(cond ((eq system-type 'darwin)
       (cond
        ((string-match "\.amazon\.com" system-name)
         (set-face-font 'default "Hack-14" nil))
        (t
         (set-face-font 'default "Hack-13" nil))))
      (t
       (set-face-font 'default "Hack-11")))

(setq-default line-spacing 3)

;; Load theme
(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'brutalist)

;; Instead of an annoying ding as a bell, flash the modeline instead.
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

(provide 'ameyp-gui)
