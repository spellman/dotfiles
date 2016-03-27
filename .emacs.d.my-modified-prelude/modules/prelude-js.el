(require 'prelude-programming)

(eval-after-load 'js2-mode
  '(progn
     (defun prelude-js-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with smartparens
       (setq-local electric-layout-rules '((?\; . after)))

       (setq-default
        js-basic-offset 2
        js2-indent-level 2
        )
       )

     (setq prelude-js-mode-hook 'prelude-js-mode-defaults)

     (add-hook 'js-mode-hook (lambda () (run-hooks 'prelude-js-mode-hook)))))

(provide 'prelude-js)
