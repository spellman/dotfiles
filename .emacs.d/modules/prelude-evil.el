(prelude-require-packages
 '(evil
   evil-indent-textobject
   evil-jumper
   evil-leader
   evil-matchit
   evil-nerd-commenter
   evil-numbers ; vim style numeric incrementing and decrementing
   evil-visualstar ; search visual selection with *
   goto-chg ; g; and g-, to go to recent changes
   ))

;;; Settings; to be set before requiring (loading) Evil, according to manual:
;;;   https://bytebucket.org/lyro/evil/raw/aaecd4a4010c09e71a207be74607ca5fa966c852/doc/evil.pdf
(setq evil-default-cursor '("gray" box)
      evil-emacs-state-cursor  '("red" box)
      evil-normal-state-cursor '("gray" box)
      evil-visual-state-cursor '("gray" box)
      evil-insert-state-cursor '("gray" bar)
      evil-motion-state-cursor '("gray" box)
      evil-replace-state-cursor '("gray" box)
      evil-operator-state-cursor '("gray" box))

;; Evil Nerd Commenter operator key
(setq evilnc-hotkey-comment-operator "gc")

;(setq evil-jumper-auto-center t)
;(setq evil-jumper-auto-save-interval 3600)

;;; Search with evil search (as opposed to isearch)
(setq evil-search-module 'evil-search)
;; Vim "very magic" regular expressions - same as Perl, Python, Ruby.
(setq evil-magic 'very-magic)

(setq evil-mode-line-format 'before)

;; Shift width controls numbers of columns < and > move a selection.
;; I think this is set in Evil stuff because it's unique to Vim; I bet tabstop
;; and softtabstop are set for Emacs stuff (maybe core/prelude-editor.el?).
;; THIS DOESN'T SEEM TO WORK. I SET IT TO 2, USING THE help with variable,
;; customize stuff.
(setq evil-shift-width 2)

;; Shift round determines whether shifting < and > moves to the nearest
;; multiple of evil-shift-round (t) or whether it shifts by evil-shift-width
;; columns, as in Vim, (nil).
;; THIS DOESN'T SEEM TO WORK. I SET IT TO nil, USING THE help with variable,
;; customize stuff.
(setq evil-shift-round nil)

;; Prevent esc-key from translating to meta-key in terminal mode.
(setq evil-esc-delay 0)

;; C-u scrolls half a page up. Note that the universal argument (normally C-u)
;; will need to be remapped in custom-keybindings.el.
(setq evil-want-C-u-scroll t)

;;; Load Evil and plugins
(require 'evil)
(require 'evil-indent-textobject)
(require 'evil-jumper)
(require 'evil-matchit)
(require 'evil-nerd-commenter)
(require 'evil-visualstar)

;;  Move all elements of evil-emacs-state-modes to evil-motion-state-modes.
;(setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
;   (setq evil-emacs-state-modes nil)

;; Enable evile in the following modes.
;;   Copied from Bling .emacs.d/config/init-evil.el (with group changed to
;;   prelude).
(defcustom dotemacs-evil-state-modes
  '(fundamental-mode
    text-mode
    prog-mode
    sws-mode
    dired-mode
    comint-mode
    log-edit-mode
    compilation-mode
    ag)
  "List of modes that should start up in Evil state."
  :type '(repeat (symbol))
  :group 'prelude)

(defun my-enable-evil-mode ()
  (if (apply 'derived-mode-p dotemacs-evil-state-modes)
      (turn-on-evil-mode)
    (set-cursor-color "red")))
(add-hook 'after-change-major-mode-hook 'my-enable-evil-mode)

;; You should enable global-evil-leader-mode before you enable evil-mode,
;; otherwise evil-leader won’t be enabled in initial buffers (*scratch*,
;; *Messages*, …).
;; (https://github.com/cofi/evil-leader)
(global-evil-leader-mode t)
(evil-mode 1)

;; Easy navigation of wrapped lines (I.e., nnoremap j gj)
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

  

;; Initial states
(evil-set-initial-state 'cider-nrepl-mode 'insert)

(provide 'prelude-evil)
