(require 'warnings)
;; Disables the warning popup when you revert-buffer a big file.
(add-to-list 'warning-suppress-types '(undo discard-info))

(provide 'ameyp-warnings)
