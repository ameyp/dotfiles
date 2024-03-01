(use-package counsel-projectile)

(use-package ivy
  :diminish (ivy-mode . "")
  :bind
  (("C-'" . ivy-avy)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c k" . counsel-yank-pop)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("<f2> j" . counsel-set-variable)
   ("C-x b" . ivy-switch-buffer)
   ("C-c v" . ivy-push-view)
   ("C-c V" . ivy-pop-view)
   ("C-c c" . counsel-compile)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c L" . counsel-git-log)
   ("C-c m" . counsel-linux-app)
   ("C-c n" . counsel-fzf)
   ("C-x l" . counsel-locate)
   ("C-c J" . counsel-file-jump)
   ("C-S-o" . counsel-rhythmbox)
   ("C-c w" . counsel-wmctrl)
   ("C-c C-r" . 'ivy-resume)
   ("C-c b" . counsel-bookmark)
   ("C-c d" . counsel-descbinds)
   ("C-c g" . counsel-git)
   ("C-c o" . counsel-outline)
   ("C-c t" . counsel-load-theme)
   ("C-c F" . counsel-org-file)
   ("C-," . counsel-projectile-find-file)
   ("C-c p s" . counsel-projectile-git-grep) ; C-c C-o to open results in new frame.
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done))
  :config
  ;; Use ripgrep to inform completions for projectile
  (when (executable-find "rg")
    (progn
      (defconst ameyp/rg-arguments
        `("--line-number"                     ; line numbers
          "--smart-case"
          "--mmap")                           ; apply memory map optimization when possible
        "Default rg arguments used in the functions in `projectile' package.")

      (defun ameyp/advice-projectile-use-rg (mode)
        "Always use `rg' for getting a list of all files in the project."
        (mapconcat 'identity
                   (append '("\\rg") ; used unaliased version of `rg': \rg
                           ameyp/rg-arguments
                           '("--null" ; output null separated results,
                             "--files" ; get file names matching the regex '' (all files)
                             "--hidden" ; include hidden files and folders...
                             "-g '!.git/'" ; ...except .git/
                             "-g '!.#*'"   ; ...except emacs temp files
                             ))
                   " "))

      (advice-add 'projectile-get-ext-command :override #'ameyp/advice-projectile-use-rg)))

  (ivy-mode 1)
  (counsel-projectile-mode t)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "(%d/%d) ")
  ;; no regexp by default
  ; (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order)))
  ;; Without this, I cannot create a new file whose name partially matches
  ;; a completion candidate.
  (setq ivy-use-selectable-prompt t)
  (setq ag-highlight-search t))

(use-package counsel)

(provide 'ameyp-ivy)
