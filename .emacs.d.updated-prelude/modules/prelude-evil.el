;;; prelude-evil.el --- Emacs Prelude: evil-mode configuration.
;;
;; Copyright © 2011-2015 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for evil-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(prelude-require-packages
 '(evil
   evil-indent-textobject
   evil-jumper
   evil-leader
   evil-matchit
   evil-nerd-commenter
   evil-numbers ; vim style numeric incrementing and decrementing
   evil-visualstar ; search visual selection with *
   goto-chg ; g; and g, to go to recent changes
   ))

;;; Settings; to be set before requiring (loading) Evil, according to manual:
;;;   https://bytebucket.org/lyro/evil/raw/aaecd4a4010c09e71a207be74607ca5fa966c852/doc/evil.pdf
(setq evil-default-cursor '("gray" box)
      evil-emacs-state-cursor  '("red" box)
      evil-normal-state-cursor '("gray" box)
      evil-visual-state-cursor '("gray" box)
      evil-insert-state-cursor '("gray" box)
      evil-motion-state-cursor '("gray" box)
      evil-replace-state-cursor '("gray" box)
      evil-operator-state-cursor '("gray" box))

;;(setq evil-jumper-auto-center t)
;;(setq evil-jumper-auto-save-interval 3600)

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

;; Enable evil in the following modes.
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


;; Place magit-blame-mode keymap above evil
;; see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(eval-after-load 'magit
  '(progn
     (evil-make-overriding-map magit-blame-mode-map 'normal)
     ;; force update evil keymaps after magit-blame-mode loaded
     (add-hook 'magit-blame-mode-hook #'evil-normalize-keymaps)))

;; Alternative method
;;(defadvice magit-blame-mode (after magit-blame-change-to-emacs-state activate compile)
;;           "when entering magit-blame mode, change evil normal state to emacs state"
;;           (if (evil-normal-state-p)
;;             (evil-emacs-state)
;;             (evil-normal-state)))
;;(ad-activate 'magit-blame-mode)

(provide 'prelude-evil)


;;; goto-chg lets you use the g-; and g-, to go to recent changes
;;; evil-visualstar enables searching visual selection with *
;;; evil-numbers enables vim style numeric incrementing and decrementing

;;;; THIS WAS NEW EVIL STUFF PLUS SOME OF MY OLD. I'M JUST GOING TO USE MY OLD
;;;; ABOVE.
;(prelude-require-packages '(evil goto-chg evil-visualstar evil-numbers))
;(prelude-require-packages
;  '(evil-indent-textobject
;    evil-jumper
;    evil-leader
;    evil-matchit
;    evil-nerd-commenter
;    ))
;
;(require 'evil-visualstar)
;
;(setq evil-mode-line-format 'before)
;
;;;(setq evil-emacs-state-cursor  '("red" box))
;;;(setq evil-normal-state-cursor '("gray" box))
;;;(setq evil-visual-state-cursor '("gray" box))
;;;(setq evil-insert-state-cursor '("gray" bar))
;;;(setq evil-motion-state-cursor '("gray" box))
;
;(setq evil-default-cursor '("gray" box)
;      evil-emacs-state-cursor  '("red" box)
;      evil-normal-state-cursor '("gray" box)
;      evil-visual-state-cursor '("gray" box)
;      evil-insert-state-cursor '("gray" box)
;      evil-motion-state-cursor '("gray" box)
;      evil-replace-state-cursor '("gray" box)
;      evil-operator-state-cursor '("gray" box)
;      )
;
;;;; Search with evil search (as opposed to isearch)
;(setq evil-search-module 'evil-search)
;
;;; Vim "very magic" regular expressions - same as Perl, Python, Ruby.
;(setq evil-magic 'very-magic)
;
;;; C-u scrolls half a page up. Note that the universal argument (normally C-u)
;;; will need to be remapped in custom-keybindings.el.
;(setq evil-want-C-u-scroll t)
;
;;; prevent esc-key from translating to meta-key in terminal mode
;(setq evil-esc-delay 0)
;
;;; Load Evil and plugins
;(require 'evil)
;(require 'evil-leader)
;(require 'evil-indent-textobject)
;(require 'evil-jumper)
;(require 'evil-matchit)
;(require 'evil-nerd-commenter)
;(require 'evil-visualstar) ; installed in modules/prelude-evil.el
;
;;; Enable evil in the following modes.
;;;   Copied from Bling .emacs.d/config/init-evil.el (with group changed to
;;;   prelude).
;(defcustom dotemacs-evil-state-modes
;  '(fundamental-mode
;    text-mode
;    prog-mode
;    sws-mode
;    dired-mode
;    comint-mode
;    log-edit-mode
;    compilation-mode
;    ag)
;  "List of modes that should start up in Evil state."
;  :type '(repeat (symbol))
;  :group 'prelude)
;
;(defun my-enable-evil-mode ()
;  (if (apply 'derived-mode-p dotemacs-evil-state-modes)
;      (turn-on-evil-mode)
;    (set-cursor-color "red")))
;(add-hook 'after-change-major-mode-hook 'my-enable-evil-mode)
;
;;; You should enable global-evil-leader-mode before you enable evil-mode,
;;; otherwise evil-leader won’t be enabled in initial buffers (*scratch*,
;;; *Messages*, …).
;;; (https://github.com/cofi/evil-leader)
;(global-evil-leader-mode t)
;(evil-mode 1)
;
;;; Easy navigation of wrapped lines (I.e., nnoremap j gj)
;(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;
;;; Initial states
;(evil-set-initial-state 'cider-nrepl-mode 'insert)
;
;;; Place magit-blame-mode keymap above evil
;;; see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
;(eval-after-load 'magit
;  '(progn
;     (evil-make-overriding-map magit-blame-mode-map 'normal)
;     ;; force update evil keymaps after magit-blame-mode loaded
;     (add-hook 'magit-blame-mode-hook #'evil-normalize-keymaps)))
;
;;; I ENABLE EVIL MODE IN PERSONAL/EVIL.EL SO I CAN ENABLE GLOBAL EVIL-LEADER
;;; MODE FIRST.
;;; DO NOT git pull AND DELETE THIS.
;;;(evil-mode 1)
;(global-evil-surround-mode 1)
;
;(define-key evil-normal-state-map (kbd "C-A")
;  'evil-numbers/inc-at-pt)
;(define-key evil-normal-state-map (kbd "C-S-A")
;  'evil-numbers/dec-at-pt)
;
;;;
;;; Other useful Commands
;;;
;(evil-ex-define-cmd "W"     'evil-write-all)
;(evil-ex-define-cmd "Tree"  'speedbar-get-focus)
;(evil-ex-define-cmd "linum" 'linum-mode)
;(evil-ex-define-cmd "Align" 'align-regexp)
;
;(defun prelude-yank-to-end-of-line ()
;  "Yank to end of line."
;  (interactive)
;  (evil-yank (point) (point-at-eol)))
;
;(define-key evil-normal-state-map
;  (kbd "Y") 'prelude-yank-to-end-of-line)
;
;(defun prelude-shift-left-visual ()
;  "Shift left and restore visual selection."
;  (interactive)
;  (evil-shift-left (region-beginning) (region-end))
;  (evil-normal-state)
;  (evil-visual-restore))
;
;(defun prelude-shift-right-visual ()
;  "Shift right and restore visual selection."
;  (interactive)
;  (evil-shift-right (region-beginning) (region-end))
;  (evil-normal-state)
;  (evil-visual-restore))
;
;(define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
;(define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)
;
;;; Scrolling
;(defun prelude-evil-scroll-down-other-window ()
;  (interactive)
;  (scroll-other-window))
;
;(defun prelude-evil-scroll-up-other-window ()
;  (interactive)
;  (scroll-other-window '-))
;
;(define-key evil-normal-state-map
;  (kbd "C-S-d") 'prelude-evil-scroll-down-other-window)
;
;(define-key evil-normal-state-map
;  (kbd "C-S-u") 'prelude-evil-scroll-up-other-window)
;
;;;
;;; Magit from avsej
;;;
;(evil-add-hjkl-bindings magit-log-mode-map 'emacs)
;(evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
;(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
;  "K" 'magit-discard-item
;  "L" 'magit-key-mode-popup-logging)
;(evil-add-hjkl-bindings magit-status-mode-map 'emacs
;  "K" 'magit-discard-item
;  "l" 'magit-key-mode-popup-logging
;  "h" 'magit-toggle-diff-refine-hunk)
;
;(setq evil-shift-width 2)
;
;;;; enable avy with evil-mode
;(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-word-1)
;
;;;; snagged from Eric S. Fraga
;;;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-05/msg00153.html
;(defun prelude-evil-key-bindings-for-org ()
;  ;;(message "Defining evil key bindings for org")
;  (evil-declare-key 'normal org-mode-map
;    "gk" 'outline-up-heading
;    "gj" 'outline-next-visible-heading
;    "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
;    "L" 'org-end-of-line ; smarter behaviour on headlines etc.
;    "t" 'org-todo ; mark a TODO item as DONE
;    ",c" 'org-cycle
;    (kbd "TAB") 'org-cycle
;    ",e" 'org-export-dispatch
;    ",n" 'outline-next-visible-heading
;    ",p" 'outline-previous-visible-heading
;    ",t" 'org-set-tags-command
;    ",u" 'outline-up-heading
;    "$" 'org-end-of-line ; smarter behaviour on headlines etc.
;    "^" 'org-beginning-of-line ; ditto
;    "-" 'org-ctrl-c-minus ; change bullet style
;    "<" 'org-metaleft ; out-dent
;    ">" 'org-metaright ; indent
;    ))
;(prelude-evil-key-bindings-for-org)
(provide 'prelude-evil)
