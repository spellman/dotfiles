(prelude-require-package 'projectile)

;; Moved from core/prelude-editor.el
(require 'projectile)
(setq projectile-cache-file (expand-file-name  "projectile.cache" prelude-savefile-dir))
(projectile-global-mode t)

(provide 'prelude-projectile)
