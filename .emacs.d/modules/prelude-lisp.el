(require 'prelude-programming)
(require 'paredit)

;; a great lisp coding hook
(defun prelude-lisp-coding-defaults ()
  (modify-syntax-entry ?: "w") ; consider : to be part of :keyword
  (modify-syntax-entry ?! "w") ; consider : to be part of :keyword
  (modify-syntax-entry ?? "w") ; consider : to be part of :keyword
  )

(setq prelude-lisp-coding-hook 'prelude-lisp-coding-defaults)

;; interactive modes don't need whitespace checks or line numbers
(defun prelude-interactive-lisp-coding-defaults ()
  (enable-paredit-mode)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1)
  (linum-mode -1))

(setq prelude-interactive-lisp-coding-hook 'prelude-interactive-lisp-coding-defaults)

;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;(add-hook 'prelude-lisp-coding-hook               #'enable-paredit-mode)
;(add-hook 'prelude-lisp-interactive-coding-hook   #'enable-paredit-mode)
;(add-hook 'eval-expression-minibuffer-setup-hook  #'enable-paredit-mode)
;(add-hook 'ielm-mode-hook                         #'enable-paredit-mode)
;(add-hook 'emacs-lisp-mode-hook             #'enable-paredit-mode)
;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(provide 'prelude-lisp)
