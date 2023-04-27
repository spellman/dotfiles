;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Cort Spellman"
      user-mail-address "spellman.cort@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "Monaco" :size 12 :weight 'normal))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq projectile-project-search-path '("~/Projects/"))

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(remove-hook 'doom-first-input-hook #'evil-snipe-override-mode)

(after! dired-x
  (remove-hook 'dired-mode-hook #'dired-omit-mode))

(after! evil
  (setq evil-move-cursor-back nil))

;; May need to wrap these in (eval-after-load "evil-maps") :/
(evil-define-key 'normal view-mode-map "0" nil)
(evil-define-key 'normal view-mode-map "-" nil)
(evil-define-key 'normal view-mode-map "+" nil)

(map! :desc "dired-jump to current-file directory" :n "-" #'dired-jump)
(eval-after-load "evil-maps"
  '(progn
     (evil-define-key 'normal view-mode-map "0" nil)
     (evil-define-key 'normal view-mode-map "-" nil)
     (evil-define-key 'normal view-mode-map "+" nil)))


(map! :after evil :desc "Move to window left" :gnv "C-h" #'evil-window-left)
(map! :after evil :desc "Move to window left" :gnv "C-j" #'evil-window-down)
(map! :after evil :desc "Move to window left" :gnv "C-k" #'evil-window-up)
(map! :after evil :desc "Move to window left" :gnv "C-l" #'evil-window-right)

;; Bind shift + <arrow key> to move between buffers, including the minibuffer.
(windmove-default-keybindings)

(map! :after better-jumper :desc "Jump backward" :gnvi "s-[" #'better-jumper-jump-backward)
(map! :after better-jumper :desc "Jump backward" :gnvi "s-]" #'better-jumper-jump-forward)

(after! smartparens
  (require 'smartparens-config))

(+global-word-wrap-mode +1)


;; Git
(setq vc-follow-symlinks t)

;; Tramp
(setq tramp-default-method "ssh")

(defun consider-underscore-word-character ()
  (modify-syntax-entry ?_ "w"))

(defun consider-hyphen-word-character ()
  (modify-syntax-entry ?- "w"))

(add-hook 'text-mode-hook #'consider-underscore-word-character)
(add-hook 'prog-mode-hook #'consider-underscore-word-character)
(add-hook 'lisp-mode-hook #'consider-underscore-word-character)

(setq-hook! 'python-mode-hook +format-with-lsp nil)

(use-package! python-black
  :demand t
  :after python)
(add-hook! 'python-mode-hook #'python-black-on-save-mode)
;; Feel free to throw your own personal keybindings here
(map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
(map! :leader :desc "Blacken Buffer" "c f" #'python-black-buffer)
(map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
(map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)

(map! :after multiple-cursors :desc "Add next word" :gnvi "s-d" #'mc/mark-next-like-this-word)
(map! :after multiple-cursors :desc "Add next similar" :gnvi "s-D" #'mc/mark-next-like-this)
(map! :after multiple-cursors :desc "Add previous word" :gnvi "s-u" #'mc/mark-previous-like-this-word)
(map! :after multiple-cursors :desc "Add previous similar" :gnvi "s-U" #'mc/mark-previous-like-this)
(map! :after multiple-cursors :desc "Un-add next similar" :gnvi "s-C-d" #'mc/unmark-next-like-this)
(map! :after multiple-cursors :desc "Un-add previous similar" :gnvi "s-C-u" #'mc/unmark-previous-like-this)
