;; Load package configuration
(load-file "~/.emacs.d/lisp/packages.el")

;; Load file that has common functions I use
(load-file "~/.emacs.d/lisp/common.el")

;; Remove toolbar, menubar and scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Add custom paths to 'load-path
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/modes")

;; Set Cofi color theme
(require 'cofi-color)
(cofi/colorscheme 'cofi-dark)

;; Set font size
(if (find-font (font-spec :name "Inconsolata"))
    (set-face-attribute 'default nil
                        :font "Inconsolata"))

(cond ((eq system-type 'darwin)
       (set-face-attribute 'default nil :height 140))
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

;; Indent newline by default
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Load helm
(require 'ameyp-helm)

;; Enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Disable those god-awful documentation warnings
(eval-after-load 'flycheck (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Load config files
(mapcar 'require '(ameyp-clojure
		   ameyp-cmake
		   ameyp-coffee
		   ameyp-company
		   ameyp-cpp
		   ameyp-elisp
		   ameyp-haskell
		   ameyp-haxe
		   ameyp-hiccough
		   ameyp-html
		   ameyp-jade
		   ;ameyp-jekyll
		   ameyp-javascript
		   ameyp-lua
		   ameyp-markdown
		   ameyp-opengl
		   ameyp-org
		   ;ameyp-poet
		   ameyp-python
		   ameyp-ruby
		   ameyp-rust
		   ameyp-web
		   ameyp-yaml))

;; Rainbow Delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Show matching paren
(show-paren-mode)

;; Disable autosave
(setq auto-save-default nil)

;; Disable backup
(setq backup-inhibited t)

;; ---- unzip files in-line from dired ------
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.txt.gz\\'")))

;; Don't highlight current line
(global-hl-line-mode 0)

;; Tabs are evil
(setq indent-tabs-mode nil)

;; Before saving:
;; 1. Untabify if indent-tabs-mode is off
;; 2. Delete trailing whitespace
;; 3. Set line-endings to unix
(add-hook 'before-save-hook
          (lambda ()
            (set-buffer-file-coding-system 'utf-8-unix)
            (delete-trailing-whitespace)
            (if (not indent-tabs-mode)
                (untabify (point-min) (point-max)))))

;; Close completions buffer when done with it
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

;; Create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; ---- magit ---------------------------------
;(require 'magit)

;; ---- whitespace-mode ---------------------
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'whitespace-toggle-options "whitespace" "Toggle local options" t)

;(require 'browse-kill-ring)
;(define-key global-map (kbd "C-c k") 'browse-kill-ring)
;(eval-after-load "browse-kill-ring"
;  '(progn
;     (setq browse-kill-ring-quit-action 'save-and-restore)))

;; Show current file name
(define-key global-map (kbd "C-<f1>") '(lambda () (message (buffer-file-name))))

;; Set path
(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path (append exec-path '("%HOME%/Apps/bin")))
      (setenv "PATH" (concat (getenv "PATH") ";%HOME%\\Apps\\bin")))
  (progn
    (setq exec-path (append exec-path '("/usr/local/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("~/Apps/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":~/Apps/bin"))))

;; C-x C-m invokes M-x
(define-key global-map (kbd "C-x C-m") 'execute-extended-command)

(define-key global-map (kbd "C-w") 'backward-kill-word)
(define-key global-map (kbd "C-x C-k") 'kill-region)

(define-key global-map (kbd "M-s") 'isearch-forward-regexp)
(define-key global-map (kbd "M-r") 'isearch-backward-regexp)

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Put autosave files in ~/.emacs.d/.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(inhibit-startup-screen t)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(js2-indent-on-enter-key t)
 '(js2-mirror-mode nil)
 '(js2-mode-show-parse-errors t))
(put 'downcase-region 'disabled nil)
