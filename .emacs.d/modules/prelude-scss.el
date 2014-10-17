(require 'prelude-css)
(prelude-require-packages '(scss-mode))

;; turn off annoying auto-compile on save
(setq scss-compile-at-save nil)

(defun prelude-scss-mode-defaults ()
  (prelude-css-mode-defaults))

(setq prelude-scss-mode-hook 'prelude-scss-mode-defaults)

(add-hook 'scss-mode-hook (lambda () (run-hooks 'prelude-scss-mode-hook)))

(provide 'prelude-scss)
