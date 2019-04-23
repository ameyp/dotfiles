;(package-initialize)

(server-start)

;; Remove toolbar, menubar and scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Inhibit startup screen
(setq inhibit-startup-screen t)

;; Add custom paths to 'load-path
(add-to-list 'load-path "~/.emacs.d/modules")

;; Load package configuration
(require 'ameyp-packages)

;; Load file that has common functions I use
(require 'ameyp-common)

;; Set Cofi color theme
(require 'cofi-color)
(cofi/colorscheme 'cofi-dark)

;; expand-region
(require 'expand-region)
(define-key global-map (kbd "C-=") 'er/expand-region)
(define-key global-map (kbd "C-+") 'er/contract-region)

;; Set font size
(if (find-font (font-spec :name "Inconsolata"))
    (set-face-attribute 'default nil
                        :font "Inconsolata"))

;; Replace selected text while mark is active
(delete-selection-mode 1)

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

;; Indent newline by default
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Disable those god-awful documentation warnings
;(eval-after-load 'flycheck (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Load config files
(mapcar 'require '(ameyp-clojure
                   ameyp-cmake
                   ameyp-coffee
                   ameyp-company
                   ameyp-elisp
                   ameyp-haskell
                   ameyp-helm
                   ameyp-lua
                   ameyp-markdown
                   ameyp-opengl
                   ameyp-org
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

;; ---- whitespace-mode ---------------------
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'whitespace-toggle-options "whitespace" "Toggle local options" t)

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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
