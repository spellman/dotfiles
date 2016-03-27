(prelude-require-packages '(yaml-mode))

;; yaml-mode doesn't derive from prog-mode, but we can at least enable
;; whitespace-mode and apply cleanup.
(add-hook 'yaml-mode-hook 'whitespace-mode)
(add-hook 'yaml-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook
          (lambda () (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)))

(provide 'prelude-yaml)
