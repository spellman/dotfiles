(add-hook
  'prelude-clojure-mode-hook
  (lambda ()
    (put-clojure-indent 'match 1)
    ))

(add-hook
  'prelude-cider-repl-mode-hook
  (lambda ()
    ;;; Cider options
    ;; Prevent the auto-display of the REPL buffer in a separate window after
    ;; connection is established.
    ;(setq cider-repl-pop-to-buffer-on-connect nil)

    ;; Don't automatically show the error buffer with stacktraces on error.
    (setq cider-show-error-buffer nil)

    ;; Wrap error messages to 80 columns for readability.
    (setq cider-stacktrace-fill-column 80)

    ;; Set the result prefix for REPL evaluation (default no prefix).
    (setq cider-repl-result-prefix ";;=> ")

    ;; Change the result prefix for interactive evaluation (default =>).
    (setq cider-interactive-eval-result-prefix ";;=> ")

    ;; Font-lock REPL input and output as in clojure-mode.
    (setq cider-repl-use-clojure-font-lock t)

    ;; Wrap REPL history.
    (setq cider-repl-wrap-history t)

    ;; Set maximum number of items kept in the REPL history (default 500).
    (setq cider-repl-history-size 1000)

    ;; Store REPL history in a file.
    ;(setq cider-repl-history-file "~/.cider-repl-history")

    ;; Pretty-print REPL results.
    (setq cider-repl-use-pretty-printing t)
    ))
