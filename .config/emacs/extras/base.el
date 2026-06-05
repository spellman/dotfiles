;;; Emacs Bedrock
;;;
;;; Extra config: Base enhancements

;;; Usage: Append or require this file from init.el to enable various UI/UX
;;; enhancements.
;;;
;;; The main search/find keybindings invoke fzfa (async fzf-backed completion;
;;; see the fzfa block in the Minibuffer section below). The consult package
;;; remains installed: it provides the isearch integration, eshell history
;;; search, and is a library dependency of affe. Its full command set is still
;;; available via M-x; see:
;;;
;;;     https://github.com/minad/consult

;;; Contents:
;;;
;;;  - Motion aids
;;;  - Power-ups: Embark and Consult
;;;  - Minibuffer and completion
;;;  - Misc. editing enhancements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Consult: Misc. enhanced commands. The global search/buffer keybindings
;; these used to claim (C-x b, M-y, M-s ...) now invoke fzfa equivalents --
;; see the fzfa block below. Consult stays for its isearch integration here,
;; the eshell C-r history search, and as a library affe depends on.
(use-package consult
  :ensure t
  :bind (;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<")
  ;; Respect .ignore/.rgignore/.fdignore but NOT .gitignore (--no-ignore-vcs),
  ;; include hidden files, and skip .git. consult-ripgrep (grep) and consult-fd
  ;; (file find) already default to the project root via consult--directory-prompt;
  ;; a single C-u prompts for a different dir on the fly.
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
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

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
  :bind (:map corfu-map
              ("SPC" . corfu-insert-separator)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous))
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
    (keymap-set eshell-mode-map "C-r" 'consult-history))
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

;; fzf-native: C dynamic module exposing fzf's Smith-Waterman scoring to
;; Emacs. It is the matching engine behind both fussy (synchronous) and fzfa
;; (asynchronous) below. The MELPA recipe ships prebuilt binaries (bin/,
;; including Darwin/arm64), so nothing is compiled at install time.
(use-package fzf-native
  :ensure t
  :defer t)

;; fussy: fzf-backed completion style. Replaces orderless as the matching and
;; scoring engine for synchronous completion (M-x, find-file, corfu, eglot),
;; so sync completion and fzfa's async commands filter and rank identically.
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
;; cores. Unlike consult's two-stage matching (regexp filters the shell
;; output, then the completion style refines), the plain fzfa commands give
;; the whole query to fzf in a single pass. The -2p ("two-pass") variants
;; reproduce the consult split for when a regexp pre-filter is wanted:
;; everything before the first space goes to the shell tool, the rest to fzf.
;; Commands default to the project root (fzfa-project-backend is `project').
;; Installed from a fork branch (see NOTE below the block): the recipe tracks
;; spellman/fzfa fix-preview-follow-async-refresh, so update commands fetch
;; and merge only that branch -- upstream (jojojames/fzfa) changes arrive only
;; when deliberately merged into the fork branch and pushed.
(use-package fzfa
  :ensure (fzfa :host github :repo "spellman/fzfa"
                :branch "fix-preview-follow-async-refresh")
  :init
  ;; The -2p commands are generated at load time by `fzfa-2p-define', so the
  ;; package autoloads file does not know about them; point cold invocations
  ;; at the files that define them.
  (autoload 'fzfa-rg-2p "fzfa-rg" nil t)
  (autoload 'fzfa-fd-2p "fzfa-fd" nil t)
  :bind (;; Drop-in replacements
         ("C-x b" . fzfa-buffer)        ; orig. switch-to-buffer
         ("M-y"   . fzfa-yank-pop)      ; orig. yank-pop
         ;; Searching
         ("M-s r" . fzfa-rg-2p)         ; project grep (was consult-ripgrep)
         ("M-s l" . fzfa-swiper)        ; line search (was consult-line)
         ("M-s s" . fzfa-swiper)
         ("M-s L" . fzfa-swiper-all)    ; was consult-line-multi
         ("M-s o" . fzfa-outline))      ; was consult-outline
  :custom
  ;; Same ignore behavior as the consult/affe commands: respect
  ;; .ignore/.rgignore/.fdignore but NOT .gitignore (--no-ignore-vcs), include
  ;; hidden files, skip .git. The commands run through a non-interactive
  ;; shell, so the quoting in --glob '!.git' is honored.
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
;; NOTE: fzfa is installed from the fork branch
;; spellman/fzfa fix-preview-follow-async-refresh (working clone:
;; ~/Projects/fzfa), which patches an upstream bug where previews were only
;; scheduled from post-command-hook: a selection that changed because results
;; streamed in (no command ran) was never previewed -- no preview on entry
;; until the first keypress, and a stale preview when late results reordered
;; the candidates. The patch (commit "Make live preview follow asynchronously
;; streamed-in results") makes the async repaint (fzfa--frontend-exhibit) run
;; the same debounced preview check. To pick up upstream changes, merge
;; jojojames/fzfa into the fork branch and push; if upstream fixes the bug,
;; point the recipe back at jojojames/fzfa and drop the fork branch.

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
  ;; completion-styles. (orderless-compile returns (PREFIX . REGEXPS); affe wants
  ;; just the regexps.)
  (defun cws/affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))
  (setopt affe-regexp-compiler #'cws/affe-orderless-regexp-compiler))

;; vertico-prescient: frecency sorting (recency x frequency, so recent picks
;; float up). Filtering stays off -- fussy is the matching engine. Once you
;; type, fussy's score-based display-sort metadata takes precedence over
;; prescient's vertico-sort-function, so prescient mainly orders the
;; no-input candidate list (recent picks on top before you type anything).
(use-package vertico-prescient
  :ensure t
  :after vertico
  :custom
  (vertico-prescient-enable-filtering nil)
  :config
  (vertico-prescient-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. editing enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modify search results en masse
(use-package wgrep
  :ensure t
  ;; wgrep autoloads itself onto grep-setup-hook, so it loads on the
  ;; first grep buffer -- no eager load or explicit trigger needed.
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))
