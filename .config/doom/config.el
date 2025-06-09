;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "Monaco" :size 13 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
      doom-symbol-font (font-spec :family "JetBrainsMono Nerd Font" :size 13)
      doom-big-font (font-spec :family "Fira Mono" :size 19))

(setq-default line-spacing 0.12)

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

;; Enable word-wrap (almost) everywhere.
(+global-word-wrap-mode +1)

(map! :n "-" 'dired-jump)

;; Defining these in the (default) global keymap in addition to in
;; evil states, below, makes them work in ihelp buffers.
(map! :nive
      "C-h" 'evil-window-left
      "C-j" 'evil-window-down
      "C-k" 'evil-window-up
      "C-l" 'evil-window-right)


;; Scroll With Cursor One Line At A Time
;; Instead of the default of half a screen at a time.
;; Note that 0 is the default, which makes Emacs scroll half a screen when point goes off-screen.
(setq scroll-step 1
      scroll-conservatively 10000)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Let s key have normal Vim behavior in normal mode; don't use it for evil-snipe package.
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(map! :leader
      :prefix ("e" . "eval")
      :desc "Eval buffer" "b" 'eval-buffer
      :desc "Eval defun" "f" 'eval-defun
      :desc "Eval last sexp" "e" 'eval-last-sexp
      :desc "Eval region" "r" 'eval-region
      :desc "Eval expression" ":" 'eval-expression)

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
      magit-bury-buffer-function #'magit-restore-window-configuration
      magit-no-confirm '(trash)
      magit-section-initial-visibility-alist '((untracked . show))

      ;; https://doc.endlessparentheses.com/Fun/format-time-string
      ;; %Y is the year
      ;; %m is the numeric month
      ;; %d is the day of the month, zero-padded
      ;; %H is the hour on a 24-hour clock
      ;; %M is the minute
      magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18))

;; (general-define-key
;;  :states '(normal visual)
;;  :keymaps '(magit-mode-map magit-status-mode-map)
;;  "n" 'magit-section-forward
;;  "p" 'magit-section-backward)

(add-hook 'git-commit-mode-hook
          (lambda () (when git-commit-mode (evil-insert-state))))
