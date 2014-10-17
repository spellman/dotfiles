(require 'prelude-programming)

(eval-after-load 'js-mode
  '(progn
     (defun prelude-js-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with smartparens
       (setq-local electric-layout-rules '((?\; . after))))

     (setq prelude-js-mode-hook 'prelude-js-mode-defaults)

     (add-hook 'js-mode-hook (lambda () (run-hooks 'prelude-js-mode-hook)))))

(provide 'prelude-js)
