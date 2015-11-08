(require 'prelude-programming)
(prelude-require-packages '(scala-mode2))

(defun prelude-scala-mode-defaults ()
  (subword-mode +1))

(setq prelude-scala-mode-hook 'prelude-scala-mode-defaults)

(add-hook 'scala-mode-hook (lambda ()
                             (run-hooks 'prelude-scala-mode-hook)))
(provide 'prelude-scala)
