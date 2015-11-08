(require 'prelude-programming)

(eval-after-load 'coffee-mode
  '(progn
     ;; CoffeeScript uses two spaces.
     (setq coffee-tab-width 2)

     ;; If you don't have js2-mode
     (setq coffee-js-mode 'javascript-mode)

     ;; If you don't want your compiled files to be wrapped
     (setq coffee-args-compile '("-c" "--bare"))

     ;; *Messages* spam
     (setq coffee-debug-mode t)

     ;; Emacs key binding
     (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

     (setq coffee-command "coffee")

     (defun prelude-coffee-mode-defaults ()
       "coffee-mode-defaults"
       ;; Compile '.coffee' files on every save
       (and (buffer-file-name)
            (file-exists-p (buffer-file-name))
            (file-exists-p (coffee-compiled-file-name (buffer-file-name)))
            (coffee-cos-mode t))
       (subword-mode +1))

     (setq prelude-coffee-mode-hook 'prelude-coffee-mode-defaults)

     (add-hook 'coffee-mode-hook (lambda ()
                                   (run-hooks 'prelude-coffee-mode-hook)))))
(provide 'prelude-coffee)
