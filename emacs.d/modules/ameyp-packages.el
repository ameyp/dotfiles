(require 'package)
(setq package-enable-at-startup nil)
(setq package-user-dir "~/.emacs.d/.elpa")

(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package package-build)

(provide 'ameyp-packages)
