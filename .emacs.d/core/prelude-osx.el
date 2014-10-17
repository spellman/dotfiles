;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(prelude-require-packages '(exec-path-from-shell vkill))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; It's all in the Meta
(setq ns-function-modifier 'hyper)

;; proced-mode doesn't work on OS X so we use vkill instead
(autoload 'vkill "vkill" nil t)
(global-set-key (kbd "C-x p") 'vkill)

(defun prelude-swap-meta-and-super ()
  "Swap the mapping of Meta and Super.
Very useful for people using their Mac with a
Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (progn
      (setq mac-command-modifier 'super)
      (setq mac-option-modifier 'meta)
      (message "Command is now bound to SUPER and Option is bound to META."))))

(define-key prelude-mode-map (kbd "C-c w") 'prelude-swap-meta-and-super)
(define-key prelude-mode-map (kbd "s-/") 'hippie-expand)

(provide 'prelude-osx)
;;; prelude-osx.el ends here
