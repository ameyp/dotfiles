(setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=60")
(setq remote-file-name-inhibit-cache 40)
(setq remote-file-name-inhibit-locks t)
(setq vc-handled-backends '(Git))

(provide 'ameyp-tramp)
