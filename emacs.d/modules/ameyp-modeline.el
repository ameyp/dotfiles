;; Adapted from https://protesilaos.com/emacs/dotemacs#h:935adc09-abaa-4413-a5ab-a7a562081c20

(defgroup ameyp-modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup ameyp-modeline-faces nil
  "Faces for my custom modeline."
  :group 'ameyp-modeline)

(defcustom ameyp-modeline-string-truncate-length 20
  "String length after which truncation should be done in small windows."
  :type 'natnum)

;;;; Faces

(defface ameyp-modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package).")

(defface ameyp-modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-red-bg
  '((default :inherit (bold ameyp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#aa1111" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff9090" :foreground "black")
    (t :background "red" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-green
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#005f00")
    (((class color) (min-colors 88) (background dark))
     :foreground "#73fa7f")
    (t :foreground "green"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-green-bg
  '((default :inherit (bold ameyp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#207b20" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77d077" :foreground "black")
    (t :background "green" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-yellow
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6f4000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#f0c526")
    (t :foreground "yellow"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-yellow-bg
  '((default :inherit (bold ameyp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#805000" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ffc800" :foreground "black")
    (t :background "yellow" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-blue
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#00228a")
    (((class color) (min-colors 88) (background dark))
     :foreground "#88bfff")
    (t :foreground "blue"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-blue-bg
  '((default :inherit (bold ameyp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#77aaff" :foreground "black")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-magenta
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#6a1aaf")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e0a0ff")
    (t :foreground "magenta"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-magenta-bg
  '((default :inherit (bold ameyp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#6f0f9f" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#e3a2ff" :foreground "black")
    (t :background "magenta" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-cyan
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#004060")
    (((class color) (min-colors 88) (background dark))
     :foreground "#30b7cc")
    (t :foreground "cyan"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'ameyp-modeline-faces)

(defface ameyp-modeline-indicator-cyan-bg
  '((default :inherit (bold ameyp-modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'ameyp-modeline-faces)

;;;; Common helper functions

(defun ameyp-common-window-narrow-p ()
  "Return non-nil if window is narrow.
Check if the `window-width' is less than `split-width-threshold'."
  (and (numberp split-width-threshold)
       (< (window-total-width) split-width-threshold)))

(defun ameyp-modeline--string-truncate-p (str)
  "Return non-nil if STR should be truncated."
  (and (ameyp-common-window-narrow-p)
       (> (length str) ameyp-modeline-string-truncate-length)
       (not (one-window-p :no-minibuffer))))

(defun ameyp-modeline--truncate-p ()
  "Return non-nil if truncation should happen.
This is a more general and less stringent variant of
`ameyp-modeline--string-truncate-p'."
  (and (ameyp-common-window-narrow-p)
       (not (one-window-p :no-minibuffer))))

(defun ameyp-modeline-string-cut-end (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the end of STR by counting from its start up to
`ameyp-modeline-string-truncate-length'."
  (if (ameyp-modeline--string-truncate-p str)
      (concat (substring str 0 ameyp-modeline-string-truncate-length) "...")
    str))

(defun ameyp-modeline-string-cut-beginning (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the beginning of STR by counting from its end up to
`ameyp-modeline-string-truncate-length'."
  (if (ameyp-modeline--string-truncate-p str)
      (concat "..." (substring str (- ameyp-modeline-string-truncate-length)))
    str))

(defun ameyp-modeline-string-cut-middle (str)
  "Return truncated STR, if appropriate, else return STR.
Cut off the middle of STR by counting half of
`ameyp-modeline-string-truncate-length' both from its beginning
and end."
  (let ((half (floor ameyp-modeline-string-truncate-length 2)))
    (if (ameyp-modeline--string-truncate-p str)
        (concat (substring str 0 half) "..." (substring str (- (+ 4 half))))
      str)))

(defun ameyp-modeline--first-char (str)
  "Return first character from STR."
  (substring str 0 1))

(defun ameyp-modeline-string-abbreviate (str)
  "Abbreviate STR individual hyphen or underscore separated words.
Also see `ameyp-modeline-string-abbreviate-but-last'."
  (if (ameyp-modeline--string-truncate-p str)
      (mapconcat #'ameyp-modeline--first-char (split-string str "[_-]") "-")
    str))

(defun ameyp-modeline-string-abbreviate-but-last (str nthlast)
  "Abbreviate STR, keeping NTHLAST words intact.
Also see `ameyp-modeline-string-abbreviate'."
  (if (ameyp-modeline--string-truncate-p str)
      (let* ((all-strings (split-string str "[_-]"))
             (nbutlast-strings (nbutlast (copy-sequence all-strings) nthlast))
             (last-strings (nreverse (ntake nthlast (nreverse (copy-sequence all-strings)))))
             (first-component (mapconcat #'ameyp-modeline--first-char nbutlast-strings "-"))
             (last-component (mapconcat #'identity last-strings "-")))
        (if (string-empty-p first-component)
            last-component
          (concat first-component "-" last-component)))
    str))

;;;; Keyboard macro indicator

(defvar-local ameyp-modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (propertize " KMacro " 'face 'ameyp-modeline-indicator-blue-bg)))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

;;;; Narrow indicator

(defvar-local ameyp-modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (propertize " Narrow " 'face 'ameyp-modeline-indicator-cyan-bg)))
  "Mode line construct to report the multilingual environment.")

;;;; Buffer status

(defvar-local ameyp-modeline-buffer-status
    '(:eval
      (when (file-remote-p default-directory)
        (concat
         (propertize " @ "
                     'face 'ameyp-modeline-indicator-red-bg
                     'mouse-face 'mode-line-highlight)
         " "
         )))
  "Mode line construct for showing remote file name.")

;;;; Buffer name and modified status

(defun ameyp-modeline-buffer-identification-face ()
  "Return appropriate face or face list for `ameyp-modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defun ameyp-modeline--buffer-name ()
  "Return `buffer-name', truncating it if necessary.
See `ameyp-modeline-string-cut-middle'."
  (when-let ((name (buffer-name)))
    (ameyp-modeline-string-cut-middle name)))

(defun ameyp-modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (ameyp-modeline--buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun ameyp-modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `ameyp-modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local ameyp-modeline-buffer-identification
    '(:eval
      (propertize (ameyp-modeline-buffer-name)
                  'face (ameyp-modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (ameyp-modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

;;;; Major mode

(defun ameyp-modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun ameyp-modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "◦"))))
    (propertize indicator 'face 'shadow)))

(defun ameyp-modeline-major-mode-help-echo ()
  "Return `help-echo' value for `ameyp-modeline-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local ameyp-modeline-major-mode
    (list
     (propertize "%[" 'face 'ameyp-modeline-indicator-red)
     '(:eval
       (concat
        (ameyp-modeline-major-mode-indicator)
        " "
        (propertize
         (ameyp-modeline-string-abbreviate-but-last
          (ameyp-modeline-major-mode-name)
          2)
         'mouse-face 'mode-line-highlight
         'help-echo (ameyp-modeline-major-mode-help-echo))))
     (propertize "%]" 'face 'ameyp-modeline-indicator-red))
  "Mode line construct for displaying major modes.")

(defvar-local ameyp-modeline-process
    (list '("" mode-line-process))
  "Mode line construct for the running process indicator.")

;;;; Flymake

(declare-function flymake--severity "flymake" (type))
(declare-function flymake-diagnostic-type "flymake" (diag))

;; Based on `flymake--mode-line-counter'.
(defun ameyp-modeline-flymake-counter (type)
  "Compute number of diagnostics in buffer with TYPE's severity.
TYPE is usually keyword `:error', `:warning' or `:note'."
  (let ((count 0))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (cl-plusp count)
      (number-to-string count))))

(defvar ameyp-modeline-flymake-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'flymake-show-buffer-diagnostics)
    (define-key map [mode-line down-mouse-3] 'flymake-show-project-diagnostics)
    map)
  "Keymap to display on Flymake indicator.")

(defmacro ameyp-modeline-flymake-type (type indicator &optional face)
  "Return function that handles Flymake TYPE with stylistic INDICATOR and FACE."
  `(defun ,(intern (format "ameyp-modeline-flymake-%s" type)) ()
     (when-let ((count (ameyp-modeline-flymake-counter
                        ,(intern (format ":%s" type)))))
       (concat
        (propertize ,indicator 'face 'shadow)
        (propertize count
                    'face ',(or face type)
                    'mouse-face 'mode-line-highlight
                    ;; FIXME 2023-07-03: Clicking on the text with
                    ;; this buffer and a single warning present, the
                    ;; diagnostics take up the entire frame.  Why?
                    'local-map ameyp-modeline-flymake-map
                    'help-echo "mouse-1: buffer diagnostics\nmouse-3: project diagnostics")))))

(ameyp-modeline-flymake-type error "☣")
(ameyp-modeline-flymake-type warning "!")
(ameyp-modeline-flymake-type note "·" success)

(defvar-local ameyp-modeline-flymake
    `(:eval
      (when (and (bound-and-true-p flymake-mode)
                 (mode-line-window-selected-p))
        (list
         ;; See the calls to the macro `ameyp-modeline-flymake-type'
         '(:eval (ameyp-modeline-flymake-error))
         '(:eval (ameyp-modeline-flymake-warning))
         '(:eval (ameyp-modeline-flymake-note)))))
  "Mode line construct displaying `flymake-mode-line-format'.
Specific to the current window's mode line.")

;;;; Git branch and diffstat

(declare-function vc-git--symbolic-ref "vc-git" (file))

(defun ameyp-modeline--vc-branch-name (file backend)
  "Return capitalized VC branch name for FILE with BACKEND."
  (when-let ((rev (vc-working-revision file backend))
             (branch (or (vc-git--symbolic-ref file)
                         (substring rev 0 7))))
    (capitalize branch)))

(declare-function vc-git-working-revision "vc-git" (file))

(defvar ameyp-modeline-vc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'vc-diff)
    (define-key map [mode-line down-mouse-3] 'vc-root-diff)
    map)
  "Keymap to display on VC indicator.")

(defun ameyp-modeline--vc-help-echo (file)
  "Return `help-echo' message for FILE tracked by VC."
  (format "Revision: %s\nmouse-1: `vc-diff'\nmouse-3: `vc-root-diff'"
          (vc-working-revision file)))

(defun ameyp-modeline--vc-text (file branch &optional face)
  "Prepare text for Git controlled FILE, given BRANCH.
With optional FACE, use it to propertize the BRANCH."
  (concat
   (propertize (char-to-string #xE0A0) 'face 'shadow)
   " "
   (propertize branch
               'face face
               'mouse-face 'mode-line-highlight
               'help-echo (ameyp-modeline--vc-help-echo file)
               'local-map ameyp-modeline-vc-map)
   ;; " "
   ;; (ameyp-modeline-diffstat file)
   ))

(defun ameyp-modeline--vc-details (file branch &optional face)
  "Return Git BRANCH details for FILE, truncating it if necessary.
The string is truncated if the width of the window is smaller
than `split-width-threshold'."
  (ameyp-modeline-string-cut-end
   (ameyp-modeline--vc-text file branch face)))

(defvar ameyp-modeline--vc-faces
  '((added . vc-locally-added-state)
    (edited . vc-edited-state)
    (removed . vc-removed-state)
    (missing . vc-missing-state)
    (conflict . vc-conflict-state)
    (locked . vc-locked-state)
    (up-to-date . vc-up-to-date-state))
  "VC state faces.")

(defun ameyp-modeline--vc-get-face (key)
  "Get face from KEY in `ameyp-modeline--vc-faces'."
  (alist-get key ameyp-modeline--vc-faces 'up-to-date))

(defun ameyp-modeline--vc-face (file backend)
  "Return VC state face for FILE with BACKEND."
  (ameyp-modeline--vc-get-face (vc-state file backend)))

(defvar-local ameyp-modeline-vc-branch
    '(:eval
      (when-let* (((mode-line-window-selected-p))
                  (file (buffer-file-name))
                  (backend (vc-backend file))
                  ;; ((vc-git-registered file))
                  (branch (ameyp-modeline--vc-branch-name file backend))
                  (face (ameyp-modeline--vc-face file backend)))
        (ameyp-modeline--vc-details file branch face)))
  "Mode line construct to return propertized VC branch.")

;;;; Eglot

(with-eval-after-load 'eglot
  (setq mode-line-misc-info
        (delete '(eglot--managed-mode (" [" eglot--mode-line-format "] ")) mode-line-misc-info)))

(defvar-local ameyp-modeline-eglot
    `(:eval
      (when (and (featurep 'eglot) (mode-line-window-selected-p))
        '(eglot--managed-mode eglot--mode-line-format)))
  "Mode line construct displaying Eglot information.
Specific to the current window's mode line.")

;;;; Risky local variables

;; The `risky-local-variable' is critical, as those
;; variables will not work without it.
(dolist (construct '(ameyp-modeline-kbd-macro
                     ameyp-modeline-narrow
                     ameyp-modeline-input-method
                     ameyp-modeline-buffer-status
                     ameyp-modeline-buffer-identification
                     ameyp-modeline-major-mode
                     ameyp-modeline-process
                     ameyp-modeline-vc-branch
                     ameyp-modeline-flymake
                     ameyp-modeline-eglot))
  (put construct 'risky-local-variable t))

(provide 'ameyp-modeline)
