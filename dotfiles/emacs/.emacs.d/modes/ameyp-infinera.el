(autoload 'yang-mode "yang-mode" "Major mode for editing YANG modules." t)
(add-to-list 'auto-mode-alist '("\\.yang$" . yang-mode))

(require 'p4)
(setq p4-my-clients '("aparulekar-lnx"))
(p4-set-client-name "aparulekar")

(defun create-cscope-infinera (dir-name)
  (interactive "DDirectory: ")

  (call-process-shell-command
   (format "cd %s; rm cscope.*" dir-name)
   nil " *Cscope Generation Output*" t)

  (create-file-list dir-name '("src_ne" "etc/src_obc\+\+") '("*.[chCH]" "*.cpp" "*.cxx") "cscope.files")
  (ascope-create-database dir-name)
  (create-file-list dir-name '("src_ne") '("*.ob") "cscope.files"))

(provide 'ameyp-infinera)
