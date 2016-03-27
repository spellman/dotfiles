(require 'prelude-lisp)

;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

;; Common Lisp support depends on SLIME being installed with Quicklisp
(cond ((file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
       (load (expand-file-name "~/quicklisp/slime-helper.el")))
      ((file-exists-p (expand-file-name "~/.quicklisp/slime-helper.el"))
       (load (expand-file-name "~/.quicklisp/slime-helper.el")))
      (t (message "%s" "SLIME is not installed. Use Quicklisp to install it.")))

;; a list of alternative Common Lisp implementations that can be
;; used with SLIME. Note that their presence render
;; inferior-lisp-program useless. This variable holds a list of
;; programs and if you invoke SLIME with a negative prefix
;; argument, M-- M-x slime, you can select a program from that list.
(setq slime-lisp-implementations
      '((ccl ("ccl"))
        (clisp ("clisp" "-q"))
        (cmucl ("cmucl" "-quiet"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

;; select the default value from slime-lisp-implementations
(if (eq system-type 'darwin)
    ;; default to Clozure CL on OS X
    (setq slime-default-lisp 'ccl)
  ;; default to SBCL on Linux and Windows
  (setq slime-default-lisp 'sbcl))

(add-hook 'lisp-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))
(add-hook 'slime-repl-mode-hook (lambda () (run-hooks 'prelude-interactive-lisp-coding-hook)))

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t
           slime-auto-start 'always)

     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))

(provide 'prelude-common-lisp)
