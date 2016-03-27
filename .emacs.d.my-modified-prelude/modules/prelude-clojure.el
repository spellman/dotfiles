(require 'prelude-lisp)
(prelude-require-packages '(clojure-mode cider))

(eval-after-load 'clojure-mode
  '(progn
     (defun prelude-clojure-mode-defaults ()
       (subword-mode +1)
       (put-clojure-indent 'match 1)
       (run-hooks 'prelude-lisp-coding-hook))

     (setq prelude-clojure-mode-hook 'prelude-clojure-mode-defaults)

     (add-hook 'clojure-mode-hook (lambda ()
                                    (run-hooks 'prelude-clojure-mode-hook)))))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t)

     (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

     (defun prelude-cider-repl-mode-defaults ()
       (subword-mode +1)
       (setq cider-repl-use-pretty-printing t)
       (run-hooks 'prelude-interactive-lisp-coding-hook))

     (setq prelude-cider-repl-mode-hook 'prelude-cider-repl-mode-defaults)

     (add-hook 'cider-repl-mode-hook (lambda ()
                                       (run-hooks 'prelude-cider-repl-mode-hook)))))

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
;(setq cider-repl-use-pretty-printing t)

(provide 'prelude-clojure)
