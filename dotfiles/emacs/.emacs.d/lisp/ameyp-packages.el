(load "package-list.el")

(require 'package)
(setq package-user-dir "~/.emacs.d/.elpa")

(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package ameyp-package-list)
  (when (and (not (package-installed-p package))
           (assoc package package-archive-contents))
    (package-install package)))

(defun ameyp-packages/package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `ameyp-package-list'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x ameyp-package-list))
                            (not (package-built-in-p x))
                            (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))

(defadvice package-install (after ameyp/package-install (package-name-or-desc))
  "Add the package to the list of installed packages"
  (setq ameyp-package-list
	(delete-dups
	 (cons
	  (cond ((symbolp package-name-or-desc) package-name-or-desc)
		((stringp package-name-or-desc) (intern package-name-or-desc))
		((fboundp 'package-desc-name) (package-desc-name package-name-or-desc))))
	 ameyp-package-list))
  (ameyp-common/dump-vars-to-file '(ameyp-package-list) "package-list.el"))

(ad-enable-advice 'package-install 'after 'ameyp/package-install)
(ad-activate 'package-install)

(provide 'ameyp-packages)
