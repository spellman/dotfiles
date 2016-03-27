(require 'prelude-programming)
(prelude-require-packages '(erlang))

(defcustom wrangler-path nil
  "The location of wrangler elisp directory."
  :group 'prelude-erlang
  :type 'string
  :safe 'stringp)

(require 'projectile)

(when (require 'erlang-start nil t)

  (eval-after-load 'erlang-mode
    '(progn
       (flymake-mode)))

  (when (not (null wrangler-path))
    (add-to-list 'load-path wrangler-path)
    (require 'wrangler)))

(add-hook 'erlang-mode-hook (lambda ()
                              (setq erlang-compile-function 'projectile-compile-project)))

(provide 'prelude-erlang)
