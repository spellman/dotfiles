(defun conditionally-enable-paredit-mode ()
    "Enable `paredit' in the minibuffer, during `eval-expression'."
      (if (eq this-command 'eval-expression)
        (enable-paredit-mode)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
