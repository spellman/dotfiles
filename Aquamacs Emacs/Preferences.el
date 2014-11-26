;; This is the Aquamacs Preferences file.
;; Add Emacs-Lisp code here that should be executed whenever
;; you start Aquamacs Emacs. If errors occur, Aquamacs will stop
;; evaluating this file and print errors in the *Messags* buffer.
;; Use this file in place of ~/.emacs (which is loaded as well.)

(osx-key-mode -1)  ; no Mac-specific key bindings



;; Persistency and modes:
(setq
  aquamacs-scratch-file nil ; do not save scratch file across sessions
  initial-major-mode 'emacs-lisp-mode ; *scratch* shows up in emacs-lisp-mode
  )



; Frame and window management:
(tabbar-mode -1) ; no tabbar
(one-buffer-one-frame-mode -1) ; no one-buffer-per-frame
(setq special-display-regexps nil) ; do not open certain buffers in special windows/frames
; (smart-frame-positioning-mode -1) ; do not place frames behind the Dock or outside of screen boundaries
(tool-bar-mode 0) ; turn off toolbar
(scroll-bar-mode -1)  ; no scrollbars



;; Appearance
(aquamacs-autoface-mode -1) ; no mode-specific faces, everything in Monaco
(set-face-attribute 'mode-line nil :inherit 'unspecified) ; show modeline in Monaco
(set-face-attribute 'echo-area nil :family 'unspecified) ; show echo area in Monaco



;; Editing
(global-smart-spacing-mode -1)  ; not on by default
(remove-hook 'text-mode-hook 'smart-spacing-mode)   ; do not use smart spacing in text modes
(global-visual-line-mode -1)  ; turn off Emacs 23 visual line
(cua-mode nil)
(transient-mark-mode nil)  ; (must switch off CUA mode as well for this to work)
