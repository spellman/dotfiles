(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

;; Nice scrolling
(setq scroll-margin 3
      scroll-conservatively 100000
      auto-window-vscroll nil
      scroll-step 1
      )

; Show line numbers.
(prelude-require-package 'linum)
(require 'linum)
(global-linum-mode t)

;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Make the fringe (gutter) smaller.
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Enable y/n answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable narrowing.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)


;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Prelude - " (:eval (if (buffer-file-name)
                                                  (abbreviate-file-name (buffer-file-name))
                                                  "%b"))))

;; Use dark as the default theme.
(load-theme 'dark t)

(provide 'prelude-ui)
