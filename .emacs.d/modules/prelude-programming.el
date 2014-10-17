(prelude-require-packages '(rainbow-delimiters))
(require 'paredit)

(defun prelude-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; in Emacs 24 programming major modes generally derive from a common
;; mode named prog-mode; for others, we'll arrange for our mode
;; defaults function to run prelude-prog-mode-hook directly.  To
;; augment and/or counteract these defaults your own function
;; to prelude-prog-mode-hook, using:
;;
;;     (add-hook 'prelude-prog-mode-hook 'my-prog-mode-defaults t)
;;
;; (the final optional t sets the *append* argument)

(defun prelude-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (enable-paredit-mode)
  (rainbow-delimiters-mode +1)
  (modify-syntax-entry ?_ "w") ; consider _ to be part of words
  (modify-syntax-entry ?- "w") ; consider - to be part of words
  (prelude-enable-whitespace)
  (prelude-local-comment-auto-fill)
  (prelude-font-lock-comment-annotations))

(setq prelude-prog-mode-hook 'prelude-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'prelude-prog-mode-hook)))

;; enable on-the-fly syntax checking
;(if (fboundp 'global-flycheck-mode)
;    (global-flycheck-mode +1)
;  (add-hook 'prog-mode-hook 'flycheck-mode))

(provide 'prelude-programming)
