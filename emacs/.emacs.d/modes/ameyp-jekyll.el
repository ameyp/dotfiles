(defvar jekyll-roots '("~/Developer/ameyp.github.com/_posts"
		       "/Users/amey/Developer/ameyp.github.com/_posts"))

(defun jekyll:get-filename-from-title (title)
  (format "%s/%s-%s.md"
          (cl-reduce (lambda (result current)
		       (if (and (eq nil result)
				(file-exists-p current))
			   current
			 result)) hiccough-roots :initial-value nil)
	  (format-time-string "%Y-%m-%d")
	  (replace-regexp-in-string "\-+$" ""
	    (replace-regexp-in-string "^\-+" ""
	      (replace-regexp-in-string "[^[:alpha:][:digit:]]" "-" (downcase title))))
	  ))

(defun jekyll:new-post (title)
  (interactive)
  (let ((post-date (jekyll:get-current-date))
        (post-file (jekyll:get-filename-from-title title)))
    (with-current-buffer (find-file-noselect post-file)
      (switch-to-buffer (find-buffer-visiting post-file))
      (insert "---\n")
      (insert "layout: post\n")
      (insert (format "title: %s\n" title))
      (insert (format "tags: []\n"))
      (insert "---\n")
      (insert "{% include JB/setup %}\n\n")
      )))

(provide 'ameyp-jekyll)
