(deftheme ia-writer-dark
  "Dark theme inspired by iA Writer")

(let ((bg "#151515")        ; Darker background from screenshot
      (fg "#898989")        ; Dimmed text color from screenshot
      (cursor "#898989")
      (highlight "#253340")
      (region "#253340")
      (comment "#666666")
      (string "#7ca8c7")
      (keyword "#dedede")   ; Brighter text for keywords
      (line-num "#4a4a4a")
      (unfocused "#444444")) ; Very dim text for unfocused paragraphs

  (custom-theme-set-faces
   'ia-writer-dark
   `(default ((t (:family "Ia Writer Duospace" :background ,bg :foreground ,fg :height 220))))
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
   `(mode-line ((t (:background ,bg :foreground ,fg :box (:line-width 1 :color "#303030")))))
   `(mode-line-inactive ((t (:background ,bg :foreground ,comment :box (:line-width 1 :color "#252525")))))
   ;; Focus mode faces
   `(writegood-mode-disabled-face ((t (:foreground ,unfocused))))
   )

  (custom-theme-set-variables
   'ia-writer-dark
   '(line-spacing 0.5)))

(provide-theme 'ia-writer-dark)
