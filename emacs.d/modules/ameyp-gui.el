;; Remove toolbar, menubar and scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; (cond ((eq system-type 'darwin)
;;        (cond
;;         ((string-match "\.amazon\.com" system-name)
;;          (set-face-attribute 'default nil :height 160))
;;         (t
;;          (set-face-attribute 'default nil :height 140))))
;;       ((eq system-type 'windows-nt)
;;        (set-face-attribute 'default nil :height 130))
;;       (t
;;        (set-face-attribute 'default nil :height 130)))

(add-to-list 'default-frame-alist '(font . "Hack-14" ))
(set-face-font 'default "Hack-14" nil)
;(set-face-font 'mode-line "Hack-14" nil)
(setq-default line-spacing 3)
(setq custom-theme-directory "~/.emacs.d/themes")
(load-theme 'brutalist)

;(require 'brutalist-theme)
(provide 'ameyp-gui)
