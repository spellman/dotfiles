;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Cort Spellman"
      user-mail-address "spellman.cort@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
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
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! 'dired
  (dired-omit-mode -1)
  )

;; (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

(setq doom-font (font-spec :family "Monaco" :size 13 :weight 'normal))

(modify-syntax-entry ?_ "w")

(setq evil-move-cursor-back nil)

;; (setq scroll-margin 3)

;; Git
(setq vc-follow-symlinks t)

;; Tramp
(setq tramp-default-method "ssh")

;; Soft-wrap lines
(global-visual-line-mode t)


;; Window Movements
;; Free up C-h in normal mode. SPC-h still brings up the help command map.
(global-set-key (kbd "C-h") nil)
(map! :n "-"   #'dired-jump

      ;; Set preferred Vim-style window movements.
      :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right

      ;; Make evil-mode up/down operate in screen lines instead of logical lines
      :mv "j"  #'evil-next-visual-line
      :mv "k"  #'evil-previous-visual-line
      )

;; (map! :leader
;;       :desc "Magit status" "g s" #'magit-status-here)
