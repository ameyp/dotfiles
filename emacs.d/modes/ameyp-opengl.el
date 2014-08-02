(autoload 'glsl-mode "glsl-mode" "Major mode for editing GLSL shaders" t)
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

(provide 'ameyp-opengl)
