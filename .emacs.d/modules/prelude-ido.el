(prelude-require-packages '(flx-ido ido-ubiquitous smex))

(require 'ido)
(require 'ido-ubiquitous)
(require 'flx-ido)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" prelude-savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)
(ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

(setq ido-use-virtual-buffers t)

(prelude-require-package 'ido-vertical-mode)
(ido-vertical-mode)

;;; smex, remember recently and most frequently used commands
(require 'smex)
(setq smex-save-file (expand-file-name ".smex-items" prelude-savefile-dir))
(smex-initialize)

(provide 'prelude-ido)
