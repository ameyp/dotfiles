;; Adapted from https://yejun.dev/posts/blogging-using-denote-and-hugo/
;; Converts denote links to hugo's relref shortcodes to generated files.
(advice-add 'denote-link-ol-export :around
            (lambda (orig-fun link description format)
              (if (and (eq format 'md)
                       (eq org-export-current-backend 'hugo))
                  (let* ((path (denote-get-path-by-id link))
                         (export-file-name (ameyp/denote-generate-hugo-export-file-name path)))
                    (format "[%s]({{< relref \"%s\" >}})"
                            description
                            export-file-name))
                (funcall orig-fun link description format))))

;; Add advice around org-export-output-file-name so that I can generate the filename from denote frontmatter
;; rather than needing to add an explicit export_file_name property.
(advice-add 'org-export-output-file-name :around
            (lambda (orig-fun extension &optional subtreep pub-dir)
              (if (and (string-equal extension ".md")
                       (ameyp/denote-should-export-to-hugo))
                  (let ((base-name (concat
                                    (ameyp/denote-generate-hugo-export-file-name (buffer-file-name))
                                    extension)))
                    (cond
                     (pub-dir (concat (file-name-as-directory pub-dir)
                                      (file-name-nondirectory base-name)))
                     (t base-name)))

                (funcall orig-fun extension subtreep pub-dir))))

(defvar ameyp/denote--hugo-export-regexp "hugo_export[[:blank:]]*:[[:blank:]]*"
  "The frontmatter property for indicating that the note should be exported to a hugo post.")

(defun ameyp/denote-generate-hugo-export-file-name (filename)
  "Generates a hugo slug from the supplied filename."
  (let* ((title (denote-retrieve-filename-title filename))
         (date (denote--id-to-date (denote-retrieve-filename-identifier filename))))

    (concat date "-" title)))

(defun ameyp/denote-should-export-to-hugo ()
  "Check whether the current buffer should be exported to a published hugo post."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (re-search-forward ameyp/denote--hugo-export-regexp nil t 1)
          (progn
            (let ((value (buffer-substring (point) (line-end-position))))
              (or (string-equal value "t")
                  (string-equal value "true"))))))))

(defun ameyp/goto-last-consecutive-denote-property-line ()
  "Move point to the last consecutive line at the beginning of the buffer that starts with '#+'"
  (interactive)
  (goto-char (point-min))
  (let ((last-prop-line (point-min)))
    (while (looking-at "^#+")
      (setq last-prop-line (point))
      (forward-line 1))
    (goto-char last-prop-line)
    (if (looking-at "^#+")
        (beginning-of-line)
      (message "No property line found"))))

(defun ameyp/org-hugo-mark-for-export()
  "Inserts a frontmatter property to mark the denote file for export to a hugo post."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (re-search-forward ameyp/denote--hugo-export-regexp nil t 1)
          ;; Found an existing property, set it to t.
          (progn
            (delete-region (point) (line-end-position))
            (insert "t"))
        ;; No existing property found, go to the end of the frontmatter and insert the property.
        (ameyp/goto-last-consecutive-denote-property-line)
        (goto-char (line-end-position))
        (insert "\n#+hugo_export: t")
        )
      ))
  )

(defun ameyp/org-hugo-export ()
  "Export current buffer to a hugo post."
  (interactive)
  (if (ameyp/denote-should-export-to-hugo)
      (let ((org-hugo-section "post")
            (org-hugo-base-dir "~/Developer/wirywolf.com")
            (org-hugo-front-matter-format "yaml"))
        (org-hugo-export-wim-to-md))
    (message (format "Not exporting %s" (buffer-file-name)))))

(defun ameyp/org-hugo-export-marked-files ()
  "Export all marked files in dired buffer to hugo posts."
  (interactive)
  (let ((org-hugo-section "post")
        (org-hugo-base-dir "~/Developer/wirywolf.com")
        (org-hugo-front-matter-format "yaml"))

    (save-window-excursion
      (mapc (lambda (filename)
              (find-file filename)
              (ameyp/org-hugo-export))
            (dired-get-marked-files))
      )))

(provide 'ameyp-hugo)
