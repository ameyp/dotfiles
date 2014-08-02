;; CMake mode
(autoload 'cmake-mode "cmake-mode" "Major mode for CMake files" t)
(add-to-list 'auto-mode-alist '("^CMakeLists\\.txt$" . cmake-mode))

(provide 'ameyp-cmake)
