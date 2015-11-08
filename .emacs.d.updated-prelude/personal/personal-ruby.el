(prelude-require-package 'rbenv)
(require 'rbenv)

(add-hook
  'prelude-ruby-mode-hook
  (lambda ()
    (modify-syntax-entry ?: "w") ; consider : to be part of :keyword
    (modify-syntax-entry ?! "w") ; consider : to be part of :keyword
    (modify-syntax-entry ?? "w") ; consider : to be part of :keyword
    (rbenv-use-corresponding)
    ))
