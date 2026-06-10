;;; init.el --- Emacs Bedrock init -*- lexical-binding: t; -*-
;;;  ________                                                _______                 __                            __
;;; /        |                                              /       \               /  |                          /  |
;;; $$$$$$$$/ _____  ____   ______   _______  _______       $$$$$$$  | ______   ____$$ | ______   ______   _______$$ |   __
;;; $$ |__   /     \/    \ /      \ /       |/       |      $$ |__$$ |/      \ /    $$ |/      \ /      \ /       $$ |  /  |
;;; $$    |  $$$$$$ $$$$  |$$$$$$  /$$$$$$$//$$$$$$$/       $$    $$</$$$$$$  /$$$$$$$ /$$$$$$  /$$$$$$  /$$$$$$$/$$ |_/$$/
;;; $$$$$/   $$ | $$ | $$ |/    $$ $$ |     $$      \       $$$$$$$  $$    $$ $$ |  $$ $$ |  $$/$$ |  $$ $$ |     $$   $$<
;;; $$ |_____$$ | $$ | $$ /$$$$$$$ $$ \_____ $$$$$$  |      $$ |__$$ $$$$$$$$/$$ \__$$ $$ |     $$ \__$$ $$ \_____$$$$$$  \
;;; $$       $$ | $$ | $$ $$    $$ $$       /     $$/       $$    $$/$$       $$    $$ $$ |     $$    $$/$$       $$ | $$  |
;;; $$$$$$$$/$$/  $$/  $$/ $$$$$$$/ $$$$$$$/$$$$$$$/        $$$$$$$/  $$$$$$$/ $$$$$$$/$$/       $$$$$$/  $$$$$$$/$$/   $$/

;;; Minimal init.el

;;; Contents:
;;;
;;;  - Basic settings
;;;  - Discovery aids
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - UI/UX enhancements
;;;  - Development tools
;;;  - Vim emulation
;;;  - Built-in customization framework

;;; Guardrail

(when (< emacs-major-version 29)
  (error "Emacs Bedrock only works with Emacs 29 and newer; you have version %s" emacs-major-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package management: Elpaca.
;; The installer snippet lives in its own file (bootstrap-elpaca.el) so that
;; updating Elpaca is a whole-file replace, and it pins Elpaca to a specific
;; commit -- see that file. MELPA, GNU, and NonGNU recipes are all available
;; through Elpaca's default menus, so no package-archives setup is needed.
(load (expand-file-name "bootstrap-elpaca.el" user-emacs-directory))

;; This Emacs build bundles several GNU ELPA "core" packages (via
;; `package--builtin-versions'), so Elpaca treats them as built-in
;; (`elpaca-ignored-dependencies') and won't install newer versions. But current
;; packages need newer ones than are bundled: the Vertico/Corfu/Embark/ etc.
;; stack requires compat >= 31 (bundled 30.2.9999), and Magit requires transient
;; >= 0.13 (bundled 0.7.2.2). Drop those from the ignored list so Elpaca
;; installs up-to-date versions. (Add more here if another bundled dependency
;; turns out to be too old.)
(setopt elpaca-ignored-dependencies
        (seq-difference elpaca-ignored-dependencies '(compat transient)))

;; Pin packages to the versions recorded in elpaca.lockfile (written with
;; `M-x elpaca-write-lock-file'). `elpaca-menu-lock-file' is already first in
;; `elpaca-menu-functions', so pointing this at the lockfile makes its locked
;; recipes win over the live MELPA/ELPA menus: reproducible installs, no drift.
;; (Elpaca itself is also pinned in bootstrap-elpaca.el via an explicit :ref;
;; that explicit recipe takes precedence over this menu, so they don't fight.)
;;
;; Package-update cadence:
;; 1. Temporarily set this to nil, or comment out the setopt below, before
;;    package declarations are evaluated; restart Emacs so Elpaca resolves live
;;    menu recipes instead of lockfile recipes.
;; 2. Run `M-x elpaca-update RET PACKAGE RET' for each package to advance, or
;;    use `M-x elpaca-manager' and mark packages with `p' then execute with `x'.
;; 3. Exercise the updated package set. If it is good, write the new last-known
;;    good state with:
;;    `M-: (elpaca-write-lock-file (expand-file-name "elpaca.lockfile" user-emacs-directory)) RET'
;; 4. Restore the setopt below and restart Emacs so future installs and rebuilds
;;    use the lockfile again.
;;
;; Do not add package-level :ref/:tag/:pin values unless the package really
;; should stay fixed independently of the lockfile. Elpaca treats those as pins,
;; so update commands skip them while the pin remains in the recipe.
(setopt elpaca-lock-file (expand-file-name "elpaca.lockfile" user-emacs-directory))

;; Enable use-package's `:ensure' support via Elpaca. After this, a use-package
;; block with `:ensure t' installs (asynchronously) via Elpaca; `:ensure nil'
;; (or no `:ensure') is treated as built-in and configured immediately.
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; General: the keybinding framework. Every keybinding in this file is defined
;; through general -- either the `:general' use-package keyword or
;; `general-define-key' -- so a key and its (optional) which-key label always
;; live in one place. It is loaded synchronously here (`:ensure (:wait t)') so
;; the `:general' keyword and `general-define-key' are available to every
;; package and top-level binding below. (general itself does not need evil; the
;; evil-state leader bindings are applied later, in the fzfa block, :after
;; evil.)
(use-package general
  :ensure (:wait t)
  :demand t)

;; Leader keys. general supports any number of leaders; we name two and use
;; these constants (rather than literal prefix strings) in our bindings, so a
;; prefix is defined in exactly one place and is obvious at every use site.
;; `cws/leader' is the global leader; `cws/local-leader' is reserved for
;; mode-specific bindings. Pass them to general's `:prefix'.
(defconst cws/leader "SPC"
  "Global leader prefix key, for use with general's `:prefix'.")
(defconst cws/local-leader ","
  "Local (major-mode) leader prefix key, for use with general's `:prefix'.")

;; Make update commands skip pinned packages. `elpaca-fetch' already skips
;; packages pinned with :ref/:tag/:pin, but `elpaca-merge' does not -- and
;; `elpaca-pull'/`elpaca-update' (and their -all variants) all route through
;; `elpaca-merge'. So updating our pinned (detached-HEAD) Elpaca runs git's
;; update-log against a nonexistent upstream branch and fails with "HEAD does
;; not point to a branch". This advice gives the merge path the same skip
;; `elpaca-fetch' uses, so `elpaca-update-all' cleanly leaves Elpaca (and any
;; other pinned package) alone. Bump a pinned package by editing its :ref.
(with-eval-after-load 'elpaca
  (defun cws--elpaca-merge-skip-pinned (orig id &optional fetch interactive)
    "Around-advice for `elpaca-merge': skip pinned packages.
Mirrors the pinned-package handling in `elpaca-fetch'."
    (let ((e (elpaca-get id)))
      (if (and e (elpaca-pinned-p e))
          (progn
            (elpaca--unprocess e)
            (setf (elpaca<-build-steps e) (list #'elpaca--announce-pin))
            (elpaca--set-status e 'queued)
            (when interactive
              (elpaca--maybe-log)
              (elpaca--process e)))
        (funcall orig id fetch interactive))))
  (advice-add 'elpaca-merge :around #'cws--elpaca-merge-skip-pinned))

;; If you want to turn off the welcome screen, uncomment this
;(setopt inhibit-splash-screen t)

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

; Track recently-opened files
(setopt recentf-max-saved-items 100)
;; Keep remote (TRAMP) files out of the list: recentf's auto-cleanup (which runs
;; when the mode is enabled, i.e. at startup) stats every entry to drop dead
;; ones, and a remote path would block Emacs on a network connection.
;; Excluded entries are dropped without being statted.
(setopt recentf-exclude '(file-remote-p))
(recentf-mode)
;; recentf only saves its list on clean exit; save every 5 minutes so a crash or
;; kill doesn't lose the session's recent files. A named function plus
;; cancel-function-timers keeps re-evaluating init.el from stacking duplicate
;; timers, and inhibit-message stops the "Wrote .../recentf" echo-area message
;; from interrupting whatever is in the echo area every 5 minutes.
(defun cws/recentf-save-list-quietly ()
  "Save the recent-files list without echoing a message."
  (let ((inhibit-message t))
    (recentf-save-list)))
(cancel-function-timers #'cws/recentf-save-list-quietly)
(run-at-time nil 300 #'cws/recentf-save-list-quietly)

;; Move through windows with Ctrl-<arrow keys>
(general-define-key
 "C-<left>"  #'windmove-left
 "C-<right>" #'windmove-right
 "C-<up>"    #'windmove-up
 "C-<down>"  #'windmove-down)

;; Close the selected frame when explicitly asked, even when Emacs thinks it is
;; the last visible frame. On macOS, Command-W is `s-w'.
(defun cws/delete-frame-force ()
  "Delete the selected frame, forcing deletion if it is the last frame."
  (interactive)
  (delete-frame nil t))
(general-define-key
 "s-w" #'cws/delete-frame-force
 [remap delete-frame] #'cws/delete-frame-force)

;;;; Undo

;; This gets its own section because undo behavior is important and buggy undo
;; setups are painful.
(setopt undo-limit 67108864) ; 64 MB
(setopt undo-strong-limit 100663296) ; 96 MB
(setopt undo-outer-limit 1006632960) ; 960 MB

;; undo-fu: linear undo/redo backend. Evil uses this below via
;; `evil-undo-system'.
(use-package undo-fu
  :ensure t
  :defer t)

;; vundo: visualize and navigate the undo tree when linear undo/redo is not
;; enough.
(use-package vundo
  :ensure t
  :commands vundo)

;; Reopen recently closed frames with `undelete-frame'. For example,
;; `C-u 2 M-x undelete-frame' restores the second-to-last deleted frame.
(when (fboundp 'undelete-frame-mode)
  (undelete-frame-mode 1))

;; dired-x: extra Dired commands. `dired-jump' opens Dired at the current file's
;; containing directory, with point on the file.
(use-package dired-x
  :ensure nil
  :commands dired-jump
  ;; Doom equivalent: (map! :n "-" 'dired-jump)
  :general (:states 'normal
            "-" #'dired-jump))

;; Fix archaic defaults
(setopt sentence-end-double-space nil)
;; Default hard-wrap column. Major modes can still set buffer-local values for
;; formats with their own rules, such as git commit messages.
(setq-default fill-column 80)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backup-root-dir (concat user-emacs-directory "emacs-backup/"))
         (file-path (replace-regexp-in-string "[A-Za-z]:" "" fpath)) ; remove Windows drive letter in path
         (backup-file-path (replace-regexp-in-string "//" "/" (concat backup-root-dir file-path "~"))))
    (make-directory (file-name-directory backup-file-path) t)
    backup-file-path))
(setopt make-backup-file-name-function #'bedrock--backup-file-name)

;; The above creates nested directories in the backup folder. If instead you
;; would like all backup files in a flat structure, albeit with their full paths
;; concatenated into a filename, then you can use the following configuration:
;; (Run `'M-x describe-variable RET backup-directory-alist RET' for more help)
;;
;; (let ((backup-dir (expand-file-name "emacs-backup/" user-emacs-directory)))
;;   (setopt backup-directory-alist `(("." . ,backup-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer

;; Controls how the *Completions* list is offered. An integer N means: cycle
;; (complete in place, with no popup) when there are N or fewer candidates;
;; otherwise pop up the *Completions* buffer. With 1, a unique completion is
;; inserted silently and anything ambiguous shows the list. (nil never cycles; t
;; always cycles.)
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations

;; Makes TAB do double duty: it first tries to indent the line, and if the line
;; is already indented, it instead completes the symbol at point. Combined with
;; completion-cycle-threshold above, a single TAB either indents, silently
;; finishes a unique completion, or pops up the *Completions* buffer.
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary

;; Lay out the *Completions* buffer one candidate per line, top to bottom (vs.
;; 'horizontal rows or 'vertical newspaper columns). Easiest to scan and to
;; navigate with the arrow keys.
(setopt completions-format 'one-column)

;; Cluster candidates under category headings (commands, variables, files, ...)
;; rather than one flat list. Only visible when the completion source supplies
;; categories.
(setopt completions-group t)

;; Controls keyboard focus into the *Completions* buffer. 'second-tab: the first
;; TAB pops up the list, the second TAB moves the cursor into it so you can pick
;; with the arrow keys / RET. (nil never jumps in; t jumps in as soon as the
;; list appears.)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(general-define-key :keymaps 'minibuffer-mode-map
                    "TAB" #'minibuffer-complete) ; TAB acts more like how it does in the shell

;; Make a single press of <escape> quit/cancel like C-g -- abort the minibuffer
;; (in one press, not the default ESC ESC ESC), clear the region or a prefix
;; arg, exit recursive edits -- instead of <escape> acting as the Meta prefix.
;; Unlike keyboard-escape-quit, this never rearranges windows when there's
;; nothing to cancel. Evil binds <escape> in its insert/visual/normal state maps
;; and those take precedence, so modal editing is unaffected; this fires where
;; Evil doesn't claim <escape> (minibuffer, prompts, etc.).
(defun cws/escape-quit ()
  "Cancel like C-g (abort the minibuffer, clear the region or a prefix arg,
exit recursive edits) without rearranging windows."
  (interactive)
  (cond ((region-active-p) (deactivate-mark))
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        ((> (recursion-depth) 0) (exit-recursive-edit))
        (t (keyboard-quit))))
(general-define-key "<escape>" #'cws/escape-quit)

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the UI/UX enhancements section below.

;(icomplete-vertical-mode)
;(fido-vertical-mode)
;(setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; We won't set these, but they're good to know about
;;
;; (setopt indent-tabs-mode nil)
;; (setopt tab-width 4)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Visual bell: pulse the current line instead of beeping or flashing the whole
;; frame. pulse.el is built in (it's what xref uses to highlight a jump target),
;; so this needs no extra package. (ring-bell-function is not a user option,
;; hence setq rather than setopt.)
(require 'pulse)
(setq ring-bell-function (lambda () (pulse-momentary-highlight-one-line (point))))

;; Use common keystrokes by default
(cua-mode)

;; For terminal users, make the mouse more useful

(xterm-mouse-mode 1)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setopt display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook #'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook #'hl-line-mode)) hl-line-hooks))

;; Scroll With Cursor One Line At A Time
;; Instead of the default of half a screen at a time.
;; Note that 0 is the default, which makes Emacs scroll half a screen when point goes off-screen.
(setq scroll-step 1
      scroll-conservatively 10000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setopt tab-bar-show 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :ensure t
  :demand t
  :config
  ;; The second argument (NO-CONFIRM) loads the theme without prompting,
  ;; which is needed for third-party themes that aren't pre-trusted.
  (load-theme 'doom-one-light t))

(set-face-attribute 'default nil        :family "Monaco"    :height 130 :weight 'medium)
(set-face-attribute 'variable-pitch nil :family "Fira Sans" :height 130)
(set-fontset-font t nil (font-spec :family "JetBrainsMono Nerd Font") nil 'append)
(setq-default line-spacing 0.4) ; 40% of line-height

;; The doom-one-light cursor and evil-ex-search both default to the highlight
;; color, making the character at the current match invisible under the cursor.
;; Override evil-ex-search to use orange (a conventional current-match color)
;; so it differs from the blue cursor. doom-themes-set-faces resolves symbolic
;; color names (orange, bg) from the active Doom theme at load time.
(with-eval-after-load 'evil
  (doom-themes-set-faces 'user
    '(evil-ex-search :background orange :foreground bg :weight 'bold)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   UI/UX enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minibuffer and autocompletion interface enhancements; strongly recommended.
;;
;; The main search/find keybindings invoke fzfa (async fzf-backed completion;
;; see the fzfa block in the Minibuffer section below). The consult package
;; remains installed: it provides the isearch integration, eshell history
;; search, and is a library dependency of affe. Its full command set is still
;; available via M-x; see:
;;
;;     https://github.com/minad/consult

;;;; Motion aids

(use-package avy
  :ensure t
  :demand t
  :general
  ("C-c j" #'avy-goto-line
   "s-j"   #'avy-goto-char-timer))

;;;; Power-ups: Embark and Consult

;; Consult: Misc. enhanced commands. The global search/buffer keybindings these
;; used to claim (C-x b, M-y, M-s ...) now invoke fzfa equivalents -- see the
;; fzfa block below. Consult stays for its isearch integration here, the eshell
;; C-r history search, and as a library affe depends on.
(use-package consult
  :ensure t
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<")
  ;; Respect .ignore/.rgignore/.fdignore but NOT .gitignore (--no-ignore-vcs),
  ;; include hidden files, and skip .git. consult-ripgrep (grep) and consult-fd
  ;; (file find) already default to the project root via
  ;; consult--directory-prompt; a single C-u prompts for a different dir on the
  ;; fly.
  (setopt consult-ripgrep-args
          (concat consult-ripgrep-args " --no-ignore-vcs --hidden --glob !.git"))
  (setopt consult-fd-args
          '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
            "--full-path --color=never --no-ignore-vcs --hidden --exclude .git --type file"))
  ;; Preview only after a brief pause for the heavy commands (each candidate
  ;; opens a different file); skimming past candidates then previews nothing.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key '(:debounce 0.1 any))
  ;; consult-fd/consult-find return a filename that the command itself opens
  ;; with find-file; consult--find registers no preview state, so by default
  ;; these never preview. Give them a preview-only state (consult--file-preview)
  ;; so candidates preview on navigation -- debounced like the grep commands --
  ;; while the command still opens the chosen file on RET (no double-open).
  (consult-customize
   consult-fd consult-find
   :state (consult--file-preview)
   :preview-key '(:debounce 0.1 any))
  ;; consult-buffer: preview on demand only.
  (consult-customize consult-buffer :preview-key "M-."))

(use-package embark-consult
  :ensure t)

;; Embark: supercharged context-dependent menu; kinda like a
;; super-charged right-click.
(use-package embark
  :ensure t
  :demand t
  :after (avy embark-consult)
  :general ("C-c a" #'embark-act)       ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

;;;; Minibuffer and completion

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :demand t
  :config
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :general (:keymaps 'vertico-map
            "M-DEL" #'vertico-directory-delete-word))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :demand t
  :config
  (marginalia-mode))

;; Corfu: Popup completion-at-point
(use-package corfu
  :ensure t
  :demand t
  :general (:keymaps 'corfu-map
            "SPC" #'corfu-insert-separator
            "C-n" #'corfu-next
            "C-p" #'corfu-previous)
  :config
  (global-corfu-mode))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :demand t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (general-define-key :keymaps 'eshell-mode-map "C-r" #'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; Eat: Emulate A Terminal
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm")
  :config
  (eat-eshell-mode)                     ; use Eat to handle term codes in program output
  (eat-eshell-visual-command-mode))     ; commands like less will be handled by Eat

;; Orderless: completion-style library. No longer the active completion style
;; (fussy below owns completion-styles now); kept installed because affe's
;; regexp compiler (cws/affe-orderless-regexp-compiler, further below) builds
;; its match regexps with orderless-compile, which reads these two options.
(use-package orderless
  :ensure t
  :demand t
  :custom
  (orderless-matching-styles
   '(orderless-literal orderless-flex orderless-initialism orderless-regexp))
  ;; Per-word style dispatchers: =literal ~flex ,initialism !exclude
  (orderless-style-dispatchers '(orderless-affix-dispatch)))

;; fzf-native: C dynamic module exposing fzf's Smith-Waterman scoring to Emacs.
;; It is the matching engine behind both fussy (synchronous) and fzfa
;; (asynchronous) below. The MELPA recipe ships prebuilt binaries (bin/,
;; including Darwin/arm64), so nothing is compiled at install time.
(use-package fzf-native
  :ensure t
  :defer t)

;; fussy: fzf-backed completion style. Replaces orderless as the matching and
;; scoring engine for synchronous completion (M-x, find-file, corfu, eglot), so
;; sync completion and fzfa's async commands filter and rank identically.
;; fussy-setup-fzf pushes `fussy' onto completion-styles, sets
;; completion-category-overrides, and sorts candidates by fzf match score.
(use-package fussy
  :ensure t
  :demand t
  :config
  (fussy-setup-fzf)
  (fussy-eglot-setup)   ; eglot defaults to flex; route it through fussy
  (fussy-corfu-setup))  ; score corfu popup candidates with fussy too

;; fzfa: async fuzzy completion -- candidates stream from a background shell
;; command (rg, fd, ...) while fzf-native scores and sorts them across all
;; cores. Unlike consult's two-stage matching (regexp filters the shell output,
;; then the completion style refines), the plain fzfa commands give the whole
;; query to fzf in a single pass. The -2p ("two-pass") variants reproduce the
;; consult split for when a regexp pre-filter is wanted: everything before the
;; first space goes to the shell tool, the rest to fzf. Commands default to the
;; project root (fzfa-project-backend is `project'). Installed from a fork
;; branch (see NOTE below the block): the recipe tracks spellman/fzfa
;; fix-preview-follow-async-refresh, so update commands fetch and merge only
;; that branch -- upstream (jojojames/fzfa) changes arrive only when
;; deliberately merged into the fork branch and pushed.
(use-package fzfa
  :ensure (fzfa :host github :repo "spellman/fzfa"
                :branch "highlight-matches-for-fzfa-grep-commands")
  ;; All fzfa bindings live here, under `cws/leader' in Evil normal state.
  ;; No Emacs-style fzfa bindings are defined here, so configuration waits for
  ;; Evil. :defer t keeps fzfa itself lazy: the bindings below dispatch through
  ;; autoloads, and the package loads on first use. Commands without a binding
  ;; here (fzfa-yank-pop, fzfa-swiper-all, fzfa-outline, ...) are available via
  ;; M-x.
  :after (evil general)
  :defer t
  :init
  ;; The *-2p commands are generated at load time by `fzfa-2p-define', so the
  ;; package autoloads file does not know about them; point cold invocations at
  ;; the files that define them.
  (autoload 'fzfa-rg-2p "fzfa-rg" nil t)
  (autoload 'fzfa-fd-2p "fzfa-fd" nil t)

  (defun cws/fzfa-project-switch-project-dired (&rest _args)
    "Switch to a known project root and open it in Dired."
    (interactive)
    (let ((roots (project-known-project-roots)))
      (unless roots
        (user-error "No known projects"))
      (when-let* ((sel (fzfa-sync-completing-read
                        :candidates (mapcar #'abbreviate-file-name roots)
                        :prompt "switch project: "
                        :category 'fzfa-file)))
        (dired (file-name-as-directory (expand-file-name sel))))))

  ;; `fzfa-project-switch-project' normally selects a project root with fzfa,
  ;; then delegates to `project-switch-project', which asks what project action
  ;; to run. Keep the fzfa root picker but go straight to Dired at the root.
  (with-eval-after-load 'fzfa-project
    (advice-add 'fzfa-project-switch-project
                :override #'cws/fzfa-project-switch-project-dired))

  ;; Plain commands are single-pass fuzzy (the whole query goes to fzf); *-2p
  ;; commands are consult-style two-pass (text before the first space
  ;; pre-filters via the shell tool, the rest goes to fzf). Each binding's
  ;; extended definition `(COMMAND :which-key LABEL)' defines the key and its
  ;; which-key label together, so they cannot drift. The override keymap keeps
  ;; the leader available in modes such as Dired that bind SPC locally.
  (general-define-key
   :states  'normal
   :keymaps 'override
   :prefix  cws/leader
   "b b" '(fzfa-buffer         :which-key "Buffers")
   "b p" '(fzfa-project-buffer :which-key "Buffers in project")

   ;; fd is for finding files. However, rg with the --files flag also finds
   ;; files.
   ;; Let's try using rg for searching for both files and text. The benefit is
   ;; consistency over which files are searched. fd should be the same but using
   ;; the same program for both means they must be the same.
   "SPC" '(fzfa-rg-files       :which-key "Find files")
   "f f" '(fzfa-fd-2p          :which-key "Find files w/filtering")
   "f r" '(fzfa-recent-file    :which-key "Recent files")

   "p"   '(fzfa-project-switch-project :which-key "Projects")

   "/"   '(fzfa-rg             :which-key "Search")
   "s f" '(fzfa-rg-2p          :which-key "Search w/filtering")
   "s i" '(fzfa-imenu          :which-key "iMenu")
   "s b" '(fzfa-swiper         :which-key "Search in buffer"))
  :custom
  ;; Same ignore behavior as the consult/affe commands: respect
  ;; .ignore/.rgignore/.fdignore but NOT .gitignore (--no-ignore-vcs), include
  ;; hidden files, skip .git. The commands run through a non-interactive shell,
  ;; so the quoting in --glob '!.git' is honored.
  (fzfa-rg-files-command
   "rg --files --no-ignore-vcs --hidden --glob '!.git'")
  (fzfa-rg-command
   "rg --line-number --no-heading --with-filename --no-ignore-vcs --hidden --glob '!.git' %s ''")
  (fzfa-fd-command
   "fd --full-path --no-ignore-vcs --hidden --exclude .git --type file")
  ;; Preview after a brief pause, like the 0.1 s debounce consult used.
  (fzfa-preview-delay 0.1)
  ;; Drop the fzfa-buffer entry from the default preview handlers: switching
  ;; buffers should not auto-preview (consult-buffer was preview-on-demand
  ;; only). Files and grep/location hits keep their previews.
  (fzfa-preview-functions
   '((fzfa-file     :setup   fzfa--file-preview-setup
                    :preview fzfa--file-preview
                    :return  fzfa--file-preview-return)
     (fzfa-grep     :preview fzfa--grep-preview)
     (fzfa-location :preview fzfa--location-preview))))
;; NOTE: fzfa is installed from the fork branch spellman/fzfa
;; highlight-matches-for-fzfa-grep-commands (working clone: ~/Projects/fzfa).
;; This branch carries the preview-follow fix (commit "Make live preview follow
;; asynchronously streamed-in results"), which patches an upstream bug where
;; previews were only scheduled from post-command-hook: a selection that changed
;; because results streamed in (no command ran) was never previewed -- no
;; preview on entry until the first keypress, and a stale preview when late
;; results reordered the candidates. The patch makes the async repaint
;; (fzfa--frontend-exhibit) run the same debounced preview check. On top of that
;; the branch adds match highlighting in grep-style and in-Emacs previews and in
;; the minibuffer, and ivy/helm-style :apply persistent actions. To pick up
;; upstream changes, merge jojojames/fzfa into the fork branch and push; if
;; upstream lands this work, point the recipe back at jojojames/fzfa and drop
;; the fork branch.

;; affe: grab-everything fuzzy finding. affe-find = fuzzy file finding across
;; directories (like Telescope's find_files); affe-grep = in-memory fuzzy grep
;; for small/medium projects. Needs fd (affe-find) and rg (affe-grep).
(use-package affe
  :ensure t
  :after (consult orderless)
  :config
  ;; Same ignore behavior as consult: respect .ignore but not .gitignore
  ;; (--no-ignore-vcs), include hidden files, skip .git. affe-find/affe-grep
  ;; also default to the project root via consult--directory-prompt.
  (setopt affe-find-command
          "rg --color=never --files --no-ignore-vcs --hidden --glob !.git")
  (setopt affe-grep-command
          "rg --null --color=never --max-columns=1000 --no-heading --line-number --no-ignore-vcs --hidden --glob !.git -v ^$")
  ;; affe-grep already previews (it uses consult--grep-state); just debounce it.
  (consult-customize affe-grep :preview-key '(:debounce 0.1 any))
  ;; affe-find ships a state that only opens on RET (no preview). Replace it
  ;; with consult's file state, which previews on navigation AND opens the file
  ;; on return (consult--file-preview alone previews but never opens). Debounce
  ;; so a preview fires after a brief pause rather than on every move.
  (consult-customize affe-find
                     :state (consult--file-state)
                     :preview-key '(:debounce 0.1 any))
  ;; Make affe's matching fuzzy in the Orderless sense. By default affe uses
  ;; consult--regexp-compiler, which just splits the input on spaces and matches
  ;; each piece as a plain regexp -- so "efw" looks for a literal "efw" and
  ;; misses EngineFlightWeighting. Routing the whole query through Orderless
  ;; instead gives flex/initialism matching (the same styles as elsewhere) over
  ;; the *entire* input -- no consult-style "#anchor#filter" split needed.
  ;;
  ;; This sidesteps the `file' category override (which forces basic +
  ;; partial-completion, dropping Orderless) precisely because affe filters with
  ;; these regexps in its own async pipeline rather than through
  ;; completion-styles. (orderless-compile returns (PREFIX . REGEXPS); affe
  ;; wants just the regexps.)
  (defun cws/affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setopt affe-regexp-compiler #'cws/affe-orderless-regexp-compiler))

;; vertico-prescient: frecency sorting (recency x frequency, so recent picks
;; float up). Filtering stays off -- fussy is the matching engine. Once you
;; type, fussy's score-based display-sort metadata takes precedence over
;; prescient's vertico-sort-function, so prescient mainly orders the no-input
;; candidate list (recent picks on top before you type anything).
(use-package vertico-prescient
  :ensure t
  :after vertico
  :custom
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode 1))

;;;; Misc. editing enhancements

;; Treat underscore as a word character in text and programming modes so that
;; word-motion commands (Evil's w/b/e, forward-word, etc.) cross it.
(defun cws/consider-underscore-word-character ()
  (modify-syntax-entry ?_ "w"))

;; Treat hyphen as a word character in Lisp modes, where kebab-case identifiers
;; are idiomatic.
(defun cws/consider-hyphen-word-character ()
  (modify-syntax-entry ?- "w"))

(add-hook 'text-mode-hook #'cws/consider-underscore-word-character)
(add-hook 'prog-mode-hook #'cws/consider-underscore-word-character)
(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook clojure-ts-mode-hook))
  (add-hook hook #'cws/consider-underscore-word-character)
  (add-hook hook #'cws/consider-hyphen-word-character))

;; Modify search results en masse
(use-package wgrep
  :ensure t
  ;; wgrep autoloads itself onto grep-setup-hook, so it loads on the first grep
  ;; buffer -- no eager load or explicit trigger needed.
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Development tools
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It is **STRONGLY** recommended that you keep the Base enhancements section
;; above if you want to use Eglot. Lots of completion things will work better.
;;
;; This will try to use tree-sitter modes for many languages. Please run
;;
;;   M-x treesit-install-language-grammar
;;
;; before trying to use a treesit mode.

;;;; Built-in config for developers

(use-package project
  :config
  (when (>= emacs-major-version 30)
    (setopt project-mode-line t))          ; show project name in modeline
  ;; Remember the top-level project dirs under ~/Projects -- immediate children
  ;; only, no recursion -- so project-switch-project can reach them without
  ;; visiting each first. Add more roots, or pass t to recurse, if useful later.
  (project-remember-projects-under "~/Projects"))

;;;; Version Control

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :defer t
  :general ("C-x g" #'magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function    #'magit-restore-window-configuration)
  (magit-no-confirm              '(trash))
  (magit-section-initial-visibility-alist '((untracked . show)))
  ;; %Y year, %m month, %d day, %H 24h hour, %M minute
  (magit-log-margin '(t "%Y-%m-%d %H:%M" magit-log-margin-width t 18)))

;;;; Syntax checking

;; Flycheck: on-the-fly syntax and lint checking. Enabled per-buffer through the
;; prog-mode hook, so it runs in programming buffers -- Emacs Lisp, the
;; tree-sitter modes, JSON, YAML (yaml-ts-mode), Clojure, and so on -- while
;; staying out of text, Markdown, fundamental, Magit, and other non-prog
;; buffers. In any buffer with no applicable checker Flycheck simply does
;; nothing, so enabling it broadly is harmless: it only acts where a checker
;; (and the tool that checker drives) is present. The Clojure checker is
;; supplied by flycheck-clj-kondo in the Programming and Data section below.
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :custom
  ;; Report problems only through the modeline counter (e.g. "FlyC:14|1|4") plus
  ;; the fringe markers and underlines on the offending text. Do NOT
  ;; auto-display the message(s) for the error under point: the default
  ;; `flycheck-display-error-messages' spills long messages out of the echo area
  ;; into a *Flycheck error messages* pop-up window, which steals screen space
  ;; while editing. Setting this to nil disables that display path entirely. To
  ;; see the full list of problems on demand, call `M-x flycheck-list-errors'
  ;; (bound to C-c ! l), which opens the list only when you ask for it.
  (flycheck-display-errors-function nil))

;;;; Programming and Data

(use-package markdown-mode
  :ensure t
  :defer t
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

;; Clojure via tree-sitter. clojure-ts-mode needs the `clojure' grammar, which
;; the package installs itself via clojure-ts-ensure-grammars (NOT our treesit
;; block -- see the note in that block below).
;;
;; The file associations must be declared here with `:mode' rather than left to
;; the package. clojure-ts-mode only registers its own auto-mode-alist entries
;; as a side effect of *loading* the library -- that code is plain top-level
;; code, not behind a `;;;###autoload' cookie -- so with a bare `:defer t' the
;; library never loads, .clj is never in auto-mode-alist, and Clojure files open
;; in fundamental-mode. `:mode' both registers the extensions and autoloads the
;; mode, so the library loads lazily on the first Clojure file. We list the
;; extensions explicitly (instead of relying on the library's own block) to
;; avoid a chicken-and-egg miss when, say, a .cljs is the first file opened.
(use-package clojure-ts-mode
  :ensure t
  :init
  ;; CIDER and clj-refactor (below) depend on the classic `clojure-mode'
  ;; package, whose autoloads add .clj/.cljc/.cljs to `auto-mode-alist' pointing
  ;; at clojure-mode -- NOT the tree-sitter mode. Those entries and ours are
  ;; both pushed onto `auto-mode-alist' at startup, so which one wins is an
  ;; ordering race. Remap the classic modes to their tree-sitter counterparts so
  ;; a Clojure file lands in clojure-ts-mode no matter which entry wins. This is
  ;; the same `major-mode-remap-alist' mechanism the tree-sitter block below
  ;; uses for other languages; it is set in :init so it is in place before the
  ;; first Clojure file is visited, and it remaps onto autoloaded modes (no
  ;; eager load).
  (dolist (remap '((clojure-mode       . clojure-ts-mode)
                   (clojurec-mode      . clojure-ts-clojurec-mode)
                   (clojurescript-mode . clojure-ts-clojurescript-mode)))
    (add-to-list 'major-mode-remap-alist remap))
  :mode (("\\.clj\\'"  . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-clojurec-mode)
         ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
         ("\\.cljd\\'" . clojure-ts-clojuredart-mode)
         ("\\.edn\\'"  . clojure-ts-mode)))

;; CIDER: interactive Clojure development -- a REPL (cider-jack-in /
;; cider-connect), expression evaluation, inline results, the debugger,
;; definition navigation, and documentation lookup. cider-mode is the
;; buffer-local minor mode that provides all of that in a source buffer; hook it
;; onto clojure-ts-mode. The .cljs/.cljc/ .cljd tree-sitter modes derive from
;; clojure-ts-mode, so this single hook covers them too. CIDER has supported
;; clojure-ts-mode since version 1.14; it still pulls in the classic
;; clojure-mode package for a few APIs not yet ported, which is expected (the
;; remap above keeps files in the tree-sitter mode regardless). While connected,
;; CIDER installs its own completion-at-point function, so the existing Corfu
;; setup shows REPL-aware completions with no extra wiring.
(use-package cider
  :ensure t
  :defer t
  :hook (clojure-ts-mode . cider-mode)
  :custom
  (cider-repl-display-help-banner nil))  ; skip the REPL's ASCII help banner

;; clj-refactor: structural refactorings for Clojure -- rename, clean/sort the
;; namespace form, add-missing/require, introduce-let, thread/unwind threading
;; macros, and many more. It builds on CIDER for the operations that need a live
;; connection. clj-refactor-mode is buffer-local; hook it onto clojure-ts-mode
;; next to cider-mode. cljr-add-keybindings-with-prefix puts every refactoring
;; command under one prefix as a short mnemonic (e.g. <prefix> a m for
;; add-missing-libspec). We use "C-c r": clj-refactor's historically documented
;; default is "C-c C-m", but in a terminal that is the same key as "C-c RET",
;; which CIDER already binds to cider-macroexpand-1 -- so "C-c r" avoids the
;; clash. (clj-refactor can use yasnippet to fill templates for a handful of
;; create-* refactorings; with no yasnippet here those insert without
;; interactive placeholders, and every other refactoring is unaffected.)
(use-package clj-refactor
  :ensure t
  :defer t
  :hook (clojure-ts-mode . clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c r"))

;; flycheck-clj-kondo: register the external clj-kondo linter as the Flycheck
;; checker for Clojure. clj-kondo is a separate executable (install it yourself,
;; e.g. `brew install borkdude/brew/clj-kondo'); this package only teaches
;; Flycheck how to invoke it. Loading the package defines checkers for both the
;; classic and the tree-sitter Clojure modes (clj/cljs/cljc, including
;; clojure-ts-mode and its variants). `:after clojure-ts-mode' loads it the
;; first time a Clojure file is visited -- before clojure-ts-mode's prog-mode
;; hook turns on flycheck-mode and picks a checker -- and, because the package
;; depends on flycheck, loading it pulls flycheck in as well. Flycheck itself is
;; enabled in the Syntax checking section above.
(use-package flycheck-clj-kondo
  :ensure t
  :after clojure-ts-mode)

;; Terraform: Emacs has no tree-sitter mode for it, so this is the classic
;; (non-tree-sitter) major mode for .tf/.tfvars files. (CDK for Terraform isn't
;; a distinct language -- those sources are .ts/.py and use the modes for
;; those.)
(use-package terraform-mode
  :ensure t
  :defer t)

;; CoffeeScript: like Terraform, there is no tree-sitter mode shipped with Emacs
;; (no coffee-ts-mode), so this is the classic major mode. It autoloads for
;; .coffee/.iced/.cson and Cakefile, and provides indentation plus compile and
;; REPL commands. CoffeeScript is whitespace-significant and the community
;; convention is two-space indentation, so pin coffee-tab-width to 2 (it
;; otherwise defaults to the ambient tab-width).
(use-package coffee-mode
  :ensure t
  :defer t
  :custom
  (coffee-tab-width 2))

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

(use-package emacs
  :hook
  ;; Auto parenthesis matching, buffer-local to programming buffers.
  ;; (electric-pair-mode is global -- hooking it would turn pairing on
  ;; everywhere, including text/Magit buffers -- so use the local variant.)
  ((prog-mode . electric-pair-local-mode)))

;; Tree-sitter major modes, hand-managed.
;;
;; We wire tree-sitter once, at startup, and only for languages whose grammar is
;; actually installed -- so a missing grammar leaves the classic mode (or
;; package mode) in charge rather than dropping to fundamental, and the rules
;; live in two small tables below.
;;
;; To add a language: put its grammar source in `treesit-language-source-alist',
;; add a row to `cws--treesit-remaps' (if a classic mode already owns the file)
;; or `cws--treesit-auto-modes' (if only the *-ts-mode exists), install the
;; grammar with `M-x cws/treesit-install-grammars', and restart.
(use-package treesit
  :ensure nil                           ; built in
  :demand t
  :config
  ;; Where `treesit-install-language-grammar' fetches each grammar from. tsx and
  ;; typescript live in the same repo under different source dirs.
  (setopt treesit-language-source-alist
          ;; NB: clojure is intentionally absent -- the clojure-ts-mode package
          ;; pins and installs its own grammar revision (and the regex /
          ;; markdown-inline grammars it uses for docstrings) via
          ;; clojure-ts-ensure-grammars. Listing it here would let our install
          ;; helper overwrite it with an incompatible version.
          '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
            (css        "https://github.com/tree-sitter/tree-sitter-css")
            (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
            (html       "https://github.com/tree-sitter/tree-sitter-html")
            (java       "https://github.com/tree-sitter/tree-sitter-java")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
            (json       "https://github.com/tree-sitter/tree-sitter-json")
            (lua        "https://github.com/tree-sitter-grammars/tree-sitter-lua")
            (python     "https://github.com/tree-sitter/tree-sitter-python")
            (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
            (toml       "https://github.com/tree-sitter-grammars/tree-sitter-toml")
            (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
            (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))
  ;; Rows of (GRAMMAR-LANG CLASSIC-MODE TS-MODE): remap CLASSIC-MODE to TS-MODE,
  ;; but only when the grammar is installed and TS-MODE is defined -- so an
  ;; uninstalled grammar leaves CLASSIC-MODE in charge. CLASSIC-MODE is whatever
  ;; already owns the file: a built-in mode, or a package (json-mode,
  ;; yaml-mode). (clojure is handled by the clojure-ts-mode package, below;
  ;; markdown stays on markdown-mode -- Emacs has no built-in tree-sitter
  ;; markdown mode.)
  (defvar cws--treesit-remaps
    '((bash       sh-mode        bash-ts-mode)
      (css        css-mode       css-ts-mode)
      (html       mhtml-mode     html-ts-mode)
      (html       html-mode      html-ts-mode)
      (java       java-mode      java-ts-mode)
      (javascript js-mode        js-ts-mode)
      (json       json-mode      json-ts-mode)
      (python     python-mode    python-ts-mode)
      (ruby       ruby-mode      ruby-ts-mode)
      (toml       conf-toml-mode toml-ts-mode)
      (yaml       yaml-mode      yaml-ts-mode))
    "Rows of (GRAMMAR-LANG CLASSIC-MODE TS-MODE) for tree-sitter remapping.")
  ;; Rows of (GRAMMAR-LANG FILE-REGEXP TS-MODE) for languages with no classic
  ;; mode in this Emacs (only the *-ts-mode exists): point the extension
  ;; straight at the tree-sitter mode, again only when the grammar is installed.
  (defvar cws--treesit-auto-modes
    '((dockerfile "\\(?:Dockerfile\\|\\.dockerfile\\)\\'" dockerfile-ts-mode)
      (lua        "\\.lua\\'"                              lua-ts-mode)
      (typescript "\\.ts\\'"                               typescript-ts-mode)
      (tsx        "\\.tsx\\'"                              tsx-ts-mode))
    "Rows of (GRAMMAR-LANG FILE-REGEXP TS-MODE) for tree-sitter auto-mode.")
  (dolist (row cws--treesit-remaps)
    (pcase-let ((`(,lang ,classic ,ts) row))
      (when (and (treesit-ready-p lang t)  ; QUIET: don't warn on missing grammars
                 (fboundp ts))
        (add-to-list 'major-mode-remap-alist (cons classic ts)))))
  (dolist (row cws--treesit-auto-modes)
    (pcase-let ((`(,lang ,regexp ,ts) row))
      (when (and (treesit-ready-p lang t)
                 (fboundp ts))
        (add-to-list 'auto-mode-alist (cons regexp ts)))))
  (defun cws/treesit-install-grammars (&optional force)
    "Install every grammar in `treesit-language-source-alist' that is missing.
With prefix arg FORCE, reinstall all of them. After installing, restart
(or re-evaluate this block) so the remap/auto-mode tables pick them up."
    (interactive "P")
    (dolist (entry treesit-language-source-alist)
      (let ((lang (car entry)))
        (when (or force (not (treesit-language-available-p lang)))
          (message "Installing tree-sitter grammar: %s" lang)
          (treesit-install-language-grammar lang))))))

;;;; Eglot, the built-in LSP client for Emacs

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  :defer t
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot-ensure))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )

;;;; Templating

(use-package tempel
  :ensure t
  :defer t
  ;; By default, tempel looks at the file "templates" in user-emacs-directory,
  ;; but you can customize that with the tempel-path variable:
  ;; :custom
  ;; (tempel-path (concat user-emacs-directory "custom_template_file"))
  :general
  ("M-*" #'tempel-insert
   "M-+" #'tempel-complete)
  (:keymaps 'tempel-map
   "C-c RET" #'tempel-done
   "C-<down>" #'tempel-next
   "C-<up>" #'tempel-previous
   "M-<down>" #'tempel-next
   "M-<up>" #'tempel-previous)
  :init
  ;; Make a function that adds the tempel expansion function to the list of
  ;; completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  ;; Put tempel-expand on the list whenever you start programming or writing
  ;; prose.
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Vim emulation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Core Packages

;; Evil: vi emulation
(use-package evil
  :ensure t
  :demand t

  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-fu)
  ;; Let evil-collection supply modal keybindings for other modes. This must be
  ;; set before Evil loads; otherwise Evil installs its own overlapping
  ;; integration bindings, which conflict with evil-collection.
  (setq evil-want-keybinding nil)
  ;; Use evil's own search module (not isearch) so that all matches stay
  ;; highlighted persistently, the current match gets a distinct highlight, and
  ;; highlights only clear on :noh -- matching Vim/Neovim search behavior.
  (setq evil-search-module 'evil)

  ;; Enable this if you want C-u to scroll up, more like pure Vim
  ;(setq evil-want-C-u-scroll t)

  :config
  (evil-mode)

  ;; If you use Magit, start editing in insert state
  (add-hook 'git-commit-setup-hook 'evil-insert-state)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)

  ;; In normal state, <escape> should clear search highlights (like Vim's :noh)
  ;; in addition to the existing cws/escape-quit behavior (deactivate region,
  ;; abort recursive edits, etc.). Evil's normal-state binding otherwise claims
  ;; <escape> and cws/escape-quit never fires.
  (defun cws/normal-state-escape ()
    "Clear search highlights and run cws/escape-quit."
    (interactive)
    (evil-ex-nohighlight)
    (cws/escape-quit))
  (general-define-key :states 'normal "<escape>" #'cws/normal-state-escape)

  ;; Evil's `/` search (evil-search-module 'evil) reads input via the minibuffer
  ;; using evil-ex-search-keymap. Bind arrows to history navigation so up
  ;; recalls the previous (older) search term and down moves to the next
  ;; (newer) term.
  (general-define-key :keymaps 'evil-ex-search-keymap
                      "<up>" #'previous-history-element
                      "<down>" #'next-history-element)

  ;; C-s still uses Emacs isearch. isearch binds history to M-p/M-n; bind the
  ;; arrows too for consistency with `/` search above.
  (with-eval-after-load 'isearch
    (general-define-key :keymaps 'isearch-mode-map
                        "<up>" #'isearch-ring-retreat
                        "<down>" #'isearch-ring-advance))

  ;; Evil's current-match overlay (evil-ex-search face, priority 1001) is only
  ;; created during the interactive search session and deleted when the
  ;; minibuffer exits. After confirming a search or navigating with n/N, no
  ;; distinct overlay exists for the current match, so it looks the same as the
  ;; other matches (evil-ex-lazy-highlight face, priority 1000). Re-create the
  ;; overlay after each navigation so the current match is always visually
  ;; distinct. Also clean it up when :noh clears the search, since
  ;; evil-ex-nohighlight only deletes the all-matches overlays.
  (defun cws/evil-update-current-match-overlay (&rest _)
    (when (and evil-ex-search-match-beg evil-ex-search-match-end)
      (if evil-ex-search-overlay
          (move-overlay evil-ex-search-overlay
                        evil-ex-search-match-beg
                        evil-ex-search-match-end)
        (setq evil-ex-search-overlay
              (make-overlay evil-ex-search-match-beg evil-ex-search-match-end))
        (overlay-put evil-ex-search-overlay 'priority 1001)
        (overlay-put evil-ex-search-overlay 'face 'evil-ex-search))))

  (defun cws/evil-nohighlight-cleanup (&rest _)
    (when evil-ex-search-overlay
      (delete-overlay evil-ex-search-overlay)
      (setq evil-ex-search-overlay nil)))

  (advice-add 'evil-ex-search      :after #'cws/evil-update-current-match-overlay)
  (advice-add 'evil-ex-start-search :after #'cws/evil-update-current-match-overlay)
  (advice-add 'evil-ex-nohighlight  :after #'cws/evil-nohighlight-cleanup)

  ;; Leader prefix labels for which-key.
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      (format "%s b" cws/leader) "Buffer"
      (format "%s f" cws/leader) "Find"
      (format "%s s" cws/leader) "Search")))

;; Evil-Collection: Evil-friendly keybindings for many built-in and
;; third-party modes (dired, magit, ibuffer, etc.).
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

;; general is declared early (in Basic settings) so its `:general' keyword and
;; `general-define-key' are available throughout this file. The evil-state
;; leader bindings are applied in the fzfa block above, which is :after evil.
;; The leader-prefix labels (SPC b/f/s) live in the evil block above.

;; Org-mode configuration (inlined but commented out -- opt-in).
;; WARNING: customize the settings below before use, then uncomment to enable.
;; ;;; Org-mode is a fantastically powerful package. It does a lot of things, which
;; ;;; makes it a little difficult to understand at first.
;; ;;;
;; ;;; We will configure Org-mode in phases. Work with each phase as you are
;; ;;; comfortable.
;; ;;;
;; ;;; YOU NEED TO CONFIGURE SOME VARIABLES! The most important variable is the
;; ;;; `org-directory', which tells org-mode where to look to find your agenda
;; ;;; files.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;   Critical variables
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;;; These variables need to be set for Org-mode's full power to be unlocked!
;; ;;;
;; ;;; You can read the documentation for any variable with `C-h v'. If you have
;; ;;; Consult configured (see the Base enhancements section above) then it should
;; ;;; help you find what you're looking for.
;;
;; ;;; Phase 1 variables
;;
;; ;;; Phase 2 variables
;;
;; ;; Agenda variables
;; (setq org-directory "~/Documents/org/") ; Non-absolute paths for agenda and
;;                                         ; capture templates will look here.
;;
;; (setq org-agenda-files '("inbox.org" "work.org"))
;;
;; ;; Default tags
;; (setq org-tag-alist '(
;;                       ;; locale
;;                       (:startgroup)
;;                       ("home" . ?h)
;;                       ("work" . ?w)
;;                       ("school" . ?s)
;;                       (:endgroup)
;;                       (:newline)
;;                       ;; scale
;;                       (:startgroup)
;;                       ("one-shot" . ?o)
;;                       ("project" . ?j)
;;                       ("tiny" . ?t)
;;                       (:endgroup)
;;                       ;; misc
;;                       ("meta")
;;                       ("review")
;;                       ("reading")))
;;
;; ;; Org-refile: where should org-refile look?
;; (setq org-refile-targets 'FIXME)
;;
;; ;;; Phase 3 variables
;;
;; ;; Org-roam variables
;; (setq org-roam-directory "~/Documents/org-roam/")
;; (setq org-roam-index-file "~/Documents/org-roam/index.org")
;;
;; ;;; Optional variables
;;
;; ;; Advanced: Custom link types
;; ;; This example is for linking a person's 7-character ID to their page on the
;; ;; free genealogy website Family Search.
;; (setq org-link-abbrev-alist
;;       '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;   Phase 1: editing and exporting files
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (use-package org
;;   :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
;;          (org-mode . flyspell-mode))    ; spell checking!
;;
;;   :bind (:map global-map
;;               ("C-c l s" . org-store-link)          ; Mnemonic: link → store
;;               ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
;;   :config
;;   (require 'oc-csl)                     ; citation support
;;   (add-to-list 'org-export-backends 'md)
;;
;;   ;; Make org-open-at-point follow file links in the same window
;;   (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
;;
;;   ;; Make exporting quotes better
;;   (setq org-export-with-smart-quotes t)
;;   )
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;   Phase 2: todos, agenda generation, and task tracking
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;; Yes, you can have multiple use-package declarations. It's best if their
;; ;; configs don't overlap. Once you've reached Phase 2, I'd recommend merging the
;; ;; config from Phase 1. I've broken it up here for the sake of clarity.
;; (use-package org
;;   :config
;;   ;; Instead of just two states (TODO, DONE) we set up a few different states
;;   ;; that a task can be in. Run
;;   ;;     M-x describe-variable RET org-todo-keywords RET
;;   ;; for documentation on how these keywords work.
;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))
;;
;;   ;; Refile configuration
;;   (setq org-outline-path-complete-in-steps nil)
;;   (setq org-refile-use-outline-path 'file)
;;
;;   (setq org-capture-templates
;;         '(("c" "Default Capture" entry (file "inbox.org")
;;            "* TODO %?\n%U\n%i")
;;           ;; Capture and keep an org-link to the thing we're currently working with
;;           ("r" "Capture with Reference" entry (file "inbox.org")
;;            "* TODO %?\n%U\n%i\n%a")
;;           ;; Define a section
;;           ("w" "Work")
;;           ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
;;            "** TODO %?\n%U\n%i\n%a")
;;           ("wr" "Work report" entry (file+headline "work.org" "Reports")
;;            "** TODO %?\n%U\n%i\n%a")))
;;
;;   ;; An agenda view lets you see your TODO items filtered and
;;   ;; formatted in different ways. You can have multiple agenda views;
;;   ;; please see the org-mode documentation for more information.
;;   (setq org-agenda-custom-commands
;;         '(("n" "Agenda and All Todos"
;;            ((agenda)
;;             (todo)))
;;           ("w" "Work" agenda ""
;;            ((org-agenda-files '("work.org")))))))
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;
;; ;;;   Phase 3: extensions
;; ;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ;; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in customization framework
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(which-key)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Garbage collection: hand control to gcmh ("Garbage Collector Magic Hack"). It
;; keeps gc-cons-threshold high while you are actively working -- so GC does not
;; fire mid-keystroke during Vertico/Consult/Orderless completion or Consult
;; preview -- and forces a single collection once Emacs goes idle, where the
;; pause is imperceptible. This replaces the old line that reset
;; gc-cons-threshold to the 800 KB default, which made GC fire constantly during
;; completion. gcmh-idle-delay 'auto scales the idle wait to how long the last
;; collection took, so a larger heap simply waits longer before the next GC.
(use-package gcmh
  :ensure t
  :demand t
  :config
  (setopt gcmh-idle-delay 'auto)
  (setopt gcmh-auto-idle-delay-factor 10)
  (setopt gcmh-high-cons-threshold (* 64 1024 1024)) ; 64 MB while active
  (gcmh-mode 1))
