(setq package-enable-at-startup nil)
(setq package-user-dir "~/.emacs.d/.elpa")

;; Bootstrap https://github.com/raxod502/straight.el
(setq straight-repository-branch "develop")
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;(add-to-list 'package-archives
;  '("melpa" . "http://melpa.org/packages/") t)
;(package-initialize)

(straight-use-package 'use-package)

(straight-use-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;(require 'use-package-ensure)
;(setq use-package-always-ensure t)

(use-package package-build)

(use-package ag)

(provide 'ameyp-packages)
