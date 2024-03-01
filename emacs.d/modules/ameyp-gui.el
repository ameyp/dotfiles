
;; Remove toolbar, menubar and scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; Don't highlight current line
(global-hl-line-mode 0)

;; Set Cofi color theme
(require 'cofi-color)
(cofi/colorscheme 'cofi-dark)

(cond ((eq system-type 'darwin)
       (cond
        ((string-match "\.amazon\.com" system-name)
         (set-face-attribute 'default nil :height 160))
        (t
         (set-face-attribute 'default nil :height 140))))
      ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :height 130))
      (t
       (set-face-attribute 'default nil :height 130)))

;; Keybindings to increase and decrease font size
(define-key global-map (kbd "M-+")
  '(lambda () (interactive)
     (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 10))))
(define-key global-map (kbd "M--")
  '(lambda () (interactive)
     (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 10))))

(provide 'ameyp-gui)
