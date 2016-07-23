(defvar poet-roots '("/home/amey/Developer/wirywolf/_posts"
		     "/home/aparulekar/Developer/wirywolf/_posts"))

(defun poet:get-current-date ()
  (let ((months '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4") ("May" . "5") ("Jun" . "6")
		("Jul" . "7") ("Aug" . "8") ("Sep" . "9") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))
	(month-string (substring (current-time-string) 4 7))
	(date-string (substring (current-time-string) 8 10))
	(year-string (substring (current-time-string) 20)))
    (format "%s-%s-%s" (cdr (assoc month-string months)) date-string year-string)))

(defun poet:get-current-time ()
  (substring (current-time-string) 11 19))

(defun poet:get-filename-from-title (title)
      (format "%s/%s.md"
          ;; The first lambda takes the second lambda as an argument, and then calls it
          ;; with the second lambda itself as the last argument.
          ;; The second lambda then uses this last argument for recursion.
          (funcall (lambda (get-poet-root paths) (funcall get-poet-root paths get-poet-root))
                   (lambda (paths get-poet-root)
                     (cond ((not (car paths))
                            nil)
                           ((file-exists-p (car paths))
                            (car paths))
                           (t
                            (funcall get-poet-root (cdr paths) get-poet-root))))
                   poet-roots)
          (replace-regexp-in-string "\-+$" ""
            (replace-regexp-in-string "^\-+" ""
              (replace-regexp-in-string "[^[:alpha:][:digit:]]" "-" (downcase title))))
	  ))

(defun poet:new-post (title)
  (interactive)
  (let ((post-date (poet:get-current-date))
        (post-time (poet:get-current-time))
        (post-file (poet:get-filename-from-title title)))
    (with-current-buffer (find-file-noselect post-file)
      (switch-to-buffer (find-buffer-visiting post-file))
      (insert "{{{\n")
      (insert (format "\t\"title\": \"%s\",\n" title))
      (insert (format "\t\"date\": \"%s\",\n" post-date))
      (insert (format "\t\"time\": \"%s\",\n" post-time))
      (insert (format "\t\"tags\": \"[]\"\n"))
      (insert "}}}\n\n")
      )))

(provide 'ameyp-poet)
