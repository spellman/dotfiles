(prelude-require-packages '(rainbow-delimiters))
(require 'personal-packages)
(require 'paredit)

(add-hook
  'prelude-prog-mode-hook
  (lambda ()
    (enable-paredit-mode)
    (rainbow-delimiters-mode +1)
    (modify-syntax-entry ?_ "w") ; consider _ to be part of word_
    ))

;; Disable on-the-fly syntax checking.
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode -1))
