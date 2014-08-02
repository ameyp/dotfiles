(defvar hiccough-roots '("~/Developer/hiccough/posts"
		       "/Users/amey/Developer/hiccough/posts"))

(defun hiccough:get-filename-from-title (title)
  (format "%s/%s-%s.md"
          (cl-reduce (lambda (result current)
		       (if (and (eq nil result)
				(file-exists-p current))
			   current
			 result)) hiccough-roots :initial-value nil)
	  (format-time-string "%Y-%m-%d")
	  (replace-regexp-in-string "\-+$" ""
	    (replace-regexp-in-string "^\-+" ""
	      (replace-regexp-in-string "--" "-"
	        (replace-regexp-in-string "[^[:alpha:][:digit:]]" "-" (downcase title)))))
	  ))

(defun hiccough:new-post (title)
  (interactive)
  (let ((post-file (hiccough:get-filename-from-title title)))
    (with-current-buffer (find-file-noselect post-file)
      (switch-to-buffer (find-buffer-visiting post-file))
      (insert "---\n")
      (insert "layout: post\n")
      (insert (format "title: %s\n" title))
      (insert (format "tags: []\n"))
      (insert "---\n\n")
      )))

(provide 'ameyp-hiccough)
