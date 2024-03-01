(require 'flycheck)

(flycheck-def-args-var flycheck-javascript-flow-args javascript-flow)
(customize-set-variable 'flycheck-javascript-flow-args '("status"))

(flycheck-define-checker javascript-flow
    "A JavaScript syntax and style checker using Flow.
See URL `http://flowtype.org/'."
    :command (
              "flow"
              (eval flycheck-javascript-flow-args)
              "--old-output-format"
              "--color=never"
              source-original)
    :error-patterns
    ((error line-start
	    (file-name)
	    ":"
	    line
	    ":"
	    (minimal-match (one-or-more not-newline))
	    ": "
	    (message (minimal-match (and (one-or-more anything) "\n")))
	    line-end))
    :modes (web-mode js2-mode))

(add-to-list 'flycheck-checkers 'javascript-flow t)

(provide 'flycheck-flow)
