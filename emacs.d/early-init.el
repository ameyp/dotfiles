;; Copied from prot

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20)
                  gc-cons-percentage 0.2)))

;; Copied from a hackernews thread
;; Disable GC when the minibuffer is open
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda ()
            (setq gc-cons-threshold (* 1024 1024 20))))

;; improves terminal emulator (vterm/eat) throughput
(setq read-process-output-max (* 1024 1024 2)
      process-adaptive-read-buffering nil)

(setq gcmh-idle-delay-factor 20)
(setq jit-lock-defer-time 0.05)
