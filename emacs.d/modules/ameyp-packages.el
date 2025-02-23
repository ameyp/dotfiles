(setq package-enable-at-startup nil)

(require 'use-package)

(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Disable printing warnings while compiling packages.
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;(require 'use-package-ensure)
;;(setq use-package-always-ensure t)

(use-package package-build)

(provide 'ameyp-packages)
