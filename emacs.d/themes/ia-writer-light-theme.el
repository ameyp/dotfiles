(deftheme ia-writer-light
  "Light theme inspired by iA Writer")

(let ((bg "#ffffff")         ; Pure white background
      (fg "#000000")         ; Black text for focused content
      (cursor "#000000")
      (highlight "#e4eef5")
      (region "#e4eef5")
      (comment "#808080")
      (string "#2d5e76")
      (keyword "#000000")
      (line-num "#b3b3b3")
      (unfocused "#b8b8b8")) ; More subtle grey for unfocused text

  (custom-theme-set-faces
   'ia-writer-light
   `(default ((t (:background ,bg :foreground ,fg :line-spacing 0.3))))
   `(cursor ((t (:background ,cursor))))
   `(region ((t (:background ,region))))
   `(highlight ((t (:background ,highlight))))
   `(font-lock-comment-face ((t (:foreground ,comment :slant italic))))
   `(font-lock-string-face ((t (:foreground ,string))))
   `(font-lock-keyword-face ((t (:foreground ,keyword :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,fg))))
   `(font-lock-variable-name-face ((t (:foreground ,fg))))
   `(font-lock-type-face ((t (:foreground ,fg))))
   `(font-lock-constant-face ((t (:foreground ,fg))))
   `(linum ((t (:foreground ,line-num))))
   `(line-number ((t (:foreground ,line-num))))
   `(mode-line ((t (:background ,bg :foreground ,fg :box (:line-width 1 :color "#d0d0d0")))))
   `(mode-line-inactive ((t (:background ,bg :foreground ,comment :box (:line-width 1 :color "#e0e0e0")))))
   ;; Focus mode faces
   `(writegood-mode-disabled-face ((t (:foreground ,unfocused))))
   ))

(provide-theme 'ia-writer-light)
