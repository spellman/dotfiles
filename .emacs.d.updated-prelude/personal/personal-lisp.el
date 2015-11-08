(add-hook
  'prelude-lisp-coding-hook
  (lambda ()
    (modify-syntax-entry ?: "w") ; consider : to be part of :word
    (modify-syntax-entry ?! "w") ; consider ! to be part of word!
    (modify-syntax-entry ?? "w") ; consider ? to be part of word?
    (modify-syntax-entry ?- "w") ; consider - to be part of word-
    (modify-syntax-entry ?> "w") ; consider > to be part of word>
    (modify-syntax-entry ?< "w") ; consider < to be part of word<
    (modify-syntax-entry ?= "w") ; consider = to be part of word=
    (modify-syntax-entry ?* "w") ; consider * to be part of word=
    ))

;; Interactive modes don't need line numbers.
(add-hook
  'prelude-interactive-lisp-coding-hook
  (lambda ()
    (linum-mode -1)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
