(defun create-file-list (dir-name folders globs output)
  (let ((output-file (format "%s%s" dir-name output))
	(buffer-name " *File list generation output*"))
    (mapc (lambda (folder)
	    (message (format "Creating %s for %s" output folder))
	    (mapc (lambda (glob)
		    (call-process-shell-command
		     (format "find %s%s -type f -name \"%s\" | sed -e 's/%s/.\\//' >> %s"
			     dir-name
			     folder
			     glob
			     (replace-regexp-in-string "\/" "\\\\/" dir-name)
			     output-file)
		     nil buffer-name t))
		  globs))
	    folders)
    (kill-buffer buffer-name)))

;;; Recursively list files in a given directory
(defun directory-files-recursive (node match &optional maxdepth ignore)
  (cond
   ((eq node nil)
    nil)
   ((and
     ignore
     (string-match ignore node))
    nil)
   ((and
     (file-regular-p node)
     (file-readable-p node)
     (string-match match node))
    file)
   ((and
     (file-directory-p node)
     (file-readable-p node)
     (not (string-equal "." (substring node -1)))
     (not (string-equal ".." (substring node -2)))
     (or (eq maxdepth nil) (> maxdepth 0)))
    (let ((children (directory-files node t))
	  (maxdepth (if (eq maxdepth nil) nil
		      (- maxdepth 1))))
      (append (custom-recurse (car children) match maxdepth ignore)
	      (mapcar (lambda (file)
			(custom-recurse file match maxdepth ignore))
		      (cdr children)))))))

(defun flatten(x)
  (cond ((null x) nil)
    ((listp x) (append (flatten (car x)) (flatten (cdr x))))
    (t (list x))))

(defun parent-directory (dir)
  (unless (or (equal "/" dir)
	      (string-match "^[a-zA-Z]:/$" dir))
    (file-name-directory (directory-file-name dir))))

(defun find-file-in-heirarchy (current-dir fname)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR"
  (let ((file (concat current-dir fname))
        (parent (parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        file
      (when parent
        (find-file-in-heirarchy parent fname)))))

;; Delete duplicate lines
(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun ameyp-common/dump-vars-to-file (varlist filename)
  "simplistic dumping of variables in VARLIST to a file FILENAME"
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (ameyp-common/dump-vars-to-buffer varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun ameyp-common/dump-vars-to-buffer (varlist buffer)
  "insert into buffer the setq statement to recreate the variables in VARLIST"
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

(provide 'ameyp-common)
