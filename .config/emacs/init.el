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
;;;  - Optional extras
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
;; (`elpaca-ignored-dependencies') and won't install newer versions. But
;; current packages need newer ones than are bundled: the Vertico/Corfu/Embark/
;; etc. stack requires compat >= 31 (bundled 30.2.9999), and Magit requires
;; transient >= 0.13 (bundled 0.7.2.2). Drop those from the ignored list so
;; Elpaca installs up-to-date versions. (Add more here if another bundled
;; dependency turns out to be too old.)
(setopt elpaca-ignored-dependencies
        (seq-difference elpaca-ignored-dependencies '(compat transient)))

;; Pin every package to the versions recorded in elpaca.lockfile (written with
;; `M-x elpaca-write-lockfile'). `elpaca-menu-lock-file' is already first in
;; `elpaca-menu-functions', so pointing this at the lockfile makes its locked
;; recipes win over the live MELPA/ELPA menus: reproducible installs, no drift.
;; (Elpaca itself is also pinned in bootstrap-elpaca.el via an explicit :ref;
;; that explicit recipe takes precedence over this menu, so they don't fight.)
;; To update deliberately: temporarily unset elpaca-lock-file, fetch/merge the
;; packages you want, then re-run `M-x elpaca-write-lockfile'.
(setopt elpaca-lock-file (expand-file-name "elpaca.lockfile" user-emacs-directory))

;; Enable use-package's `:ensure' support via Elpaca. After this, a use-package
;; block with `:ensure t' installs (asynchronously) via Elpaca; `:ensure nil'
;; (or no `:ensure') is treated as built-in and configured immediately.
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

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
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

; Track recently-opened files
(setopt recentf-max-saved-items 100)
;; Keep remote (TRAMP) files out of the list: recentf's auto-cleanup (which
;; runs when the mode is enabled, i.e. at startup) stats every entry to drop
;; dead ones, and a remote path would block Emacs on a network connection.
;; Excluded entries are dropped without being statted.
(setopt recentf-exclude '(file-remote-p))
(recentf-mode)
;; recentf only saves its list on clean exit; save every 5 minutes so a crash
;; or kill doesn't lose the session's recent files. A named function plus
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
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

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

;; The above creates nested directories in the backup folder. If
;; instead you would like all backup files in a flat structure, albeit
;; with their full paths concatenated into a filename, then you can
;; use the following configuration:
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

;; Controls how the *Completions* list is offered. An integer N means:
;; cycle (complete in place, with no popup) when there are N or fewer
;; candidates; otherwise pop up the *Completions* buffer. With 1, a
;; unique completion is inserted silently and anything ambiguous shows
;; the list. (nil never cycles; t always cycles.)
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations

;; Makes TAB do double duty: it first tries to indent the line, and if
;; the line is already indented, it instead completes the symbol at
;; point. Combined with completion-cycle-threshold above, a single TAB
;; either indents, silently finishes a unique completion, or pops up
;; the *Completions* buffer.
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary

;; Lay out the *Completions* buffer one candidate per line, top to
;; bottom (vs. 'horizontal rows or 'vertical newspaper columns).
;; Easiest to scan and to navigate with the arrow keys.
(setopt completions-format 'one-column)

;; Cluster candidates under category headings (commands, variables,
;; files, ...) rather than one flat list. Only visible when the
;; completion source supplies categories.
(setopt completions-group t)

;; Controls keyboard focus into the *Completions* buffer. 'second-tab:
;; the first TAB pops up the list, the second TAB moves the cursor into
;; it so you can pick with the arrow keys / RET. (nil never jumps in;
;; t jumps in as soon as the list appears.)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" #'minibuffer-complete) ; TAB acts more like how it does in the shell

;; Make a single press of <escape> quit/cancel like C-g -- abort the minibuffer
;; (in one press, not the default ESC ESC ESC), clear the region or a prefix
;; arg, exit recursive edits -- instead of <escape> acting as the Meta prefix.
;; Unlike keyboard-escape-quit, this never rearranges windows when there's
;; nothing to cancel. Evil binds <escape> in its insert/visual/normal state
;; maps and those take precedence, so modal editing is unaffected; this fires
;; where Evil doesn't claim <escape> (minibuffer, prompts, etc.).
(defun cws/escape-quit ()
  "Cancel like C-g (abort the minibuffer, clear the region or a prefix arg,
exit recursive edits) without rearranging windows."
  (interactive)
  (cond ((region-active-p) (deactivate-mark))
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        ((> (recursion-depth) 0) (exit-recursive-edit))
        (t (keyboard-quit))))
(keymap-global-set "<escape>" #'cws/escape-quit)

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

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

;; Visual bell: pulse the current line instead of beeping or flashing the
;; whole frame. pulse.el is built in (it's what xref uses to highlight a
;; jump target), so this needs no extra package. (ring-bell-function is not
;; a user option, hence setq rather than setopt.)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
(load-file (expand-file-name "extras/dev.el" user-emacs-directory))

;; Vim-bindings in Emacs (evil-mode configuration)
(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
;; WARNING: need to customize things inside the elisp file before use! See
;; the file extras/org-intro.txt for help.
;(load-file (expand-file-name "extras/org.el" user-emacs-directory))

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

;; Garbage collection: hand control to gcmh ("Garbage Collector Magic Hack").
;; It keeps gc-cons-threshold high while you are actively working -- so GC does
;; not fire mid-keystroke during Vertico/Consult/Orderless completion or Consult
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
