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


;; =============================================================================
;; Undo
;; =============================================================================
;; This gets its own section because it's very important and I've struggled with
;; buggy undo setups in the past.

;; Undo Limits
(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

;; By default Doom Emacs uses undo-fu For Linear (normal and logical) undo / redo
;; We add undo tree with vundo
(use-package! vundo
  :after undo-fu)

;; Enable the undelete-frame function to reopen any of the last 16 closed
;; frames.
;; That means that I open something in a new frame, close it, and then change my
;; mind and want it back, I can simply call (undelete-frame).
;; Ex:
;; `C-u 2 M-x undelete-frame` calls `undelete-frame` with `arg` bound to 2 to
;; undelete the second-to-last deleted frame.
(undelete-frame-mode 1)



;; Close Minibuffer With Single Press Of escape
;; By default, Emacs requires pressing "ESC" three times to escape-quit the minibuffer. Change this to one:
;; (global-set-key [escape] 'keyboard-escape-quit)
(map! [escape] 'keyboard-escape-quit)


;; Scroll With Cursor One Line At A Time
;; Instead of the default of half a screen at a time.
;; Note that 0 is the default, which makes Emacs scroll half a screen when point goes off-screen.
(setq scroll-step 1
      scroll-conservatively 10000)

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)


(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

(use-package! evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard
                                    dired
                                    eldoc
                                    elisp-mode
                                    git-timemachine
                                    ibuffer
                                    magit
                                    org
                                    vc-annotate
                                    which-key))
  (evil-collection-init))



(after! doom-ui
  (map! "s-n" 'make-frame)
  (map! :nvie "s-n" 'make-frame)

  ;; Doom remaps 'delete-frame to 'doom/delete-frame-with-prompt so we undo that setting.
  (global-set-key [remap delete-frame] nil)
  ;; And then we enable closing windows using a conventional shortcut: `cmd/super+w`.
  (map! "s-w" 'delete-frame)
  (map! :nvie "s-w" 'delete-frame))

(map! :n "-" 'dired-jump)

;; Make org-mode links open in new frames.
(setcdr (assq 'file org-link-frame-setup) 'find-file-other-frame)

;; Don't insert delimiters and quotes as pairs in certain modes.
(dolist (mode '(org-mode-hook
                text-mode-hook))
  (add-hook mode (lambda () (electric-pair-local-mode 0))))

(use-package! hl-todo
  :config
  (setq hl-todo--regexp "\\(\\<\\(TODO\\|FIXME\\|HACK\\|REVIEW\\|NOTE\\|DEPRECATED\\|QUESTION\\)\\>\\)"
        hl-todo-keyword-faces `(("TODO"       warning bold)
                                ("FIXME"      error bold)
                                ("HACK"       font-lock-constant-face bold)
                                ("REVIEW"     font-lock-keyword-face bold)
                                ("NOTE"       success bold)
                                ("DEPRECATED" font-lock-doc-face bold)
                                ("QUESTION" font-lock-constant-face bold)))

  :hook
  (org-mode . hl-todo-mode)
  (prog-mode . hl-todo-mode))

;; ;; Defining these in the (default) global keymap in addition to in
;; ;; evil states, below, makes them work in ihelp buffers.
;; (map! "C-h" 'evil-window-left
;;       "C-j" 'evil-window-down
;;       "C-k" 'evil-window-up
;;       "C-l" 'evil-window-right)
;; (map! :nive
;;       "C-h" 'evil-window-left
;;       "C-j" 'evil-window-down
;;       "C-k" 'evil-window-up
;;       "C-l" 'evil-window-right)

;; Let `s` key have normal Vim behavior in normal mode; don't use it for evil-snipe package.
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

(map! :map emacs-lisp-mode-map
      :localleader
      :prefix ("e" . "eval")
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

(use-package! coffee-mode
  :mode (("\\.coffee\\'" . coffee-mode)))


(defun cs/consider-underscore-word-character ()
  (modify-syntax-entry ?_ "w"))

(defun cs/consider-hyphen-word-character ()
  (modify-syntax-entry ?- "w"))

(add-hook 'text-mode-hook #'cs/consider-underscore-word-character)
(add-hook 'prog-mode-hook #'cs/consider-underscore-word-character)
(add-hook 'emacs-lisp-mode-hook #'cs/consider-underscore-word-character)
(add-hook 'emacs-lisp-mode-hook #'cs/consider-hyphen-word-character)
(add-hook 'lisp-mode-hook #'cs/consider-underscore-word-character)
(add-hook 'lisp-mode-hook #'cs/consider-hyphen-word-character)

