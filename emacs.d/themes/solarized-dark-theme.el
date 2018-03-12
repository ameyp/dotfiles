(deftheme solarized-dark
  "cofi theme.")

(defvar solarized-colors           ; ANSI(Solarized terminal)
  ;; name     sRGB      Gen RGB   256       16              8
  '((base03  "#002b36" "#042028" "#1c1c1c" "brightblack"   "black")
    (base02  "#073642" "#0a2832" "#262626" "black"         "black")
    (base01  "#586e75" "#465a61" "#585858" "brightgreen"   "green")
    (base00  "#657b83" "#52676f" "#626262" "brightyellow"  "yellow")
    (base0   "#839496" "#708183" "#808080" "brightblue"    "blue")
    (base1   "#93a1a1" "#81908f" "#8a8a8a" "brightcyan"    "cyan")
    (base2   "#eee8d5" "#e9e2cb" "#e4e4e4" "white"         "white")
    (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "brightwhite"   "white")
    (yellow  "#b58900" "#a57705" "#af8700" "yellow"        "yellow")
    (orange  "#cb4b16" "#bd3612" "#d75f00" "brightred"     "red")
    (red     "#dc322f" "#c60007" "#d70000" "red"           "red")
    (magenta "#d33682" "#c61b6e" "#af005f" "magenta"       "magenta")
    (violet  "#6c71c4" "#5859b7" "#5f5faf" "brightmagenta" "magenta")
    (blue    "#268bd2" "#2075c7" "#0087ff" "blue"          "blue")
    (cyan    "#2aa198" "#259185" "#00afaf" "cyan"          "cyan")
    (green   "#859900" "#728a05" "#5f8700" "green"         "green"))
  "This is a table of all the colors used by the Solarized color theme. Each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")

(defcustom solarized-broken-srgb (if (and (eq system-type 'darwin)
                                          (eq window-system 'ns))
                                     (not (and (boundp 'ns-use-srgb-colorspace)
                                               ns-use-srgb-colorspace))
                                   nil)
  "Emacs bug #8402 results in incorrect color handling on Macs. If this is t
\(the default on Macs), Solarized works around it with alternative colors.
However, these colors are not totally portable, so you may be able to edit
the \"Gen RGB\" column in solarized-definitions.el to improve them further."
  :type 'boolean
  :group 'solarized)

(cl-flet ((find-color (name)
             (nth 1 (assoc name solarized-colors))))
  (let ((base03 (find-color 'base03))
	(base02 (find-color 'base02))
	(base01 (find-color 'base01))
	(base00 (find-color 'base00))
	(base0 (find-color 'base0))
	(base1 (find-color 'base1))
	(base2 (find-color 'base2))
	(base3 (find-color 'base3))
	(yellow (find-color 'yellow))
	(orange (find-color 'orange))
	(red (find-color 'red))
	(magenta (find-color 'magenta))
	(violet (find-color 'violet))
	(blue (find-color 'blue))
	(cyan (find-color 'cyan))
	(green (find-color 'green))
	)
    (custom-theme-set-faces
     'solarized-dark
     ;; basic faces
     `(default ((t (:background ,base03 :foreground ,base0))))
     `(highlight ((t (:background ,base02 :foreground ,base1))))
     `(border ((t (:background ,base02 :foreground ,base1))))
     `(header-line ((t (:background ,base03 :foreground ,base1))))
     `(fringe ((t (:background ,base02))))

     `(primary-selection ((t (:background ,base02 :foreground ,base1))))
     `(secondary-selection ((t (:background ,base01 :foreground ,base2))))

     `(minibuffer-prompt ((t (:foreground "orange"))))

     `(region ((t (:background ,base02 :foreground ,base1))))
     `(hl-line ((t (:background ,base01 :foreground ,base2))))
     `(tooltip ((t (:background ,base2 :foreground ,base01))))

     ;; modeline
     `(mode-line ((t (:background ,base0 :foreground ,base03))))
     `(mode-line-inactive ((t (:background ,base2 :foreground ,base01))))
     `(mode-line-buffer-id ((t (:foreground ,base03 :bold t))))
     `(mode-line-emphasis ((t (:foreground ,orange :bold t))))

     ;; diff faces
     `(diff-added ((t (:foreground ,green))))
     `(diff-changed ((t (:foreground ,blue))))
     `(diff-removed ((t (:foreground ,red))))
     `(diff-context ((t (:foreground ,base01))))
     `(diff-file-header ((t (:background ,base03 :foreground ,orange))))
     `(diff-function ((t (:background ,base03 :foreground ,blue))))
     `(diff-function ((t (:background ,base03 :foreground ,orange))))
     `(diff-index ((t (:background ,base02 :bold t))))
     `(diff-nonexistent ((t (:background ,base02 :bold t))))
     )))

(provide-theme 'solarized-dark)
