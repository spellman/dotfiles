(prelude-require-packages '(tuareg utop merlin))

(require 'tuareg)
(require 'utop)
(require 'merlin)

(setq auto-mode-alist
      (append '(("\\.ml[ily]?\\'" . tuareg-mode)
                ("\\.topml\\'" . tuareg-mode))
              auto-mode-alist))

(add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer)
(add-hook 'tuareg-mode-hook 'merlin-mode)

(add-hook 'tuareg-mode-hook (lambda ()
                              (progn
                                (define-key tuareg-mode-map (kbd "C-c C-s")
                                  'utop))))

;; Setup merlin completions company is used by default in prelude
(add-to-list 'company-backends 'merlin-company-backend)

;; But merlin also offers support for autocomplete, uncomment this next line
;; to activate it.
;; (setq merlin-use-auto-complete-mode t)

(setq utop-command "opam config exec \"utop -emacs\""
      merlin-error-after-save nil)

(provide 'prelude-ocaml)
