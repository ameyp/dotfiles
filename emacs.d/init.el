;; Disable the custom file altogether.
(setq custom-file (make-temp-file "emacs-custom-"))

;; Disable notifications when a buffer is auto-reverted.
(setq auto-revert-verbose nil)

;; For native compilation
(setq comp-speed 2)
;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

(require 'server)
;; Disable showing instructions in the client about how to close it.
(setq server-client-instructions nil)
(unless (server-running-p)
  (server-start))

;; Add custom paths to 'load-path
(add-to-list 'load-path "~/.emacs.d/modules")
(add-to-list 'load-path "~/.emacs.d/themes")

;; Disable those god-awful documentation warnings
;;(eval-after-load 'flycheck (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;; Print complete messages for C-x C-e
(setq eval-expression-print-length 10000)

;; Echo keystrokes faster in minibuffer
(setq echo-keystrokes 0.01)

;; Enable minibuffer-in-minibuffer
(setq enable-recursive-minibuffers t)

;; Macro for keybindings, copied from prot's dotemacs.
(defmacro ameyp-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let (((keymapp ,keymap))
                (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

;; Function for spawning a process and passing a string to its stdin.
(defun ameyp-spawn-process-with-stdin (name command args stdin &optional cleanup)
  "Spawn a process NAME with the specified COMMAND and ARGS, and write STDIN to its stdin.
If CLEANUP is specified, it must be a function or lambda. It is invoked when the process exits."
  (let ((process (make-process :name name
                               :command `(,command ,@args)
                               :connection-type 'pipe)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel process (lambda (proc event)
                                    (when (memq (process-status proc) '(exit signal))
                                      (message "Process %s %s" (process-name proc) event)
                                      (if (boundp 'cleanup) (cleanup)))))
    (process-send-string process stdin)
    (process-send-eof process)))

;; Load config files
(mapcar 'require '(;; load the essential packages first
                   ameyp-packages
                   ameyp-dired
                   ameyp-direnv
                   ameyp-text
                   ameyp-gui
                   ameyp-company
                   ameyp-eglot
                   ;; ameyp-flycheck

                   ;; load language-specific packages next
                   ameyp-clojure
                   ameyp-cmake
                   ameyp-coffee
                   ameyp-dired
                   ameyp-docker
                   ameyp-elisp
                   ameyp-git
                   ameyp-go
                   ameyp-haskell
                   ameyp-java
                   ameyp-jsonnet
                   ameyp-markdown
                   ameyp-nix
                   ameyp-org
                   ameyp-protobuf
                   ameyp-projectile
                   ameyp-python
                   ;; ameyp-ruby
                   ameyp-rust
                   ameyp-swift
                   ameyp-syntax
                   ameyp-terraform
                   ameyp-typescript
                   ameyp-vertico
                   ameyp-warnings
                   ameyp-yaml
                   ameyp-zig
                   ))

;; Load modules that live outside of my dotfiles.
(if (file-readable-p "~/.emacs-extra/init.el")
    (load "~/.emacs-extra/init.el"))
(put 'set-goal-column 'disabled nil)
