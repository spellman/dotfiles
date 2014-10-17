(require 'sh-script)

;; recognize pretzo files as zsh scripts
(defvar prelude-pretzo-files '("zlogin" "zlogin" "zlogout" "zpretzorc" "zprofile" "zshenv" "zshrc"))

(mapc (lambda (file)
        (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
      prelude-pretzo-files)

(add-hook 'sh-mode-hook
          (lambda ()
            (if (and buffer-file-name
                     (member (file-name-nondirectory buffer-file-name) prelude-pretzo-files))
                (sh-set-shell "zsh"))))

(provide 'prelude-shell)
