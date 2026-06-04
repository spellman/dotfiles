;;; Emacs Bedrock
;;;
;;; Extra config: Base enhancements

;;; Usage: Append or require this file from init.el to enable various UI/UX
;;; enhancements.
;;;
;;; The consult package in particular has a vast number of functions that you
;;; can use as replacements to what Emacs provides by default. Please see the
;;; consult documentation for more information and help:
;;;
;;;     https://github.com/minad/consult
;;;
;;; In particular, many users may find `consult-line' to be more useful to them
;;; than isearch, so binding this to `C-s' might make sense. This is left to the
;;; user to configure, however, as isearch and consult-line are not equivalent.

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

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)       ; Alternative: rebind C-s to use
         ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi) ; isearch to M-s s
         ("M-s o" . consult-outline)
         ;; Isearch integration
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

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles
   '(orderless-literal orderless-flex orderless-initialism orderless-regexp))
  ;; Per-word style dispatchers: =literal ~flex ,initialism !exclude
  (orderless-style-dispatchers '(orderless-affix-dispatch)))

;; affe: grab-everything fuzzy finding. affe-find = fuzzy file finding across
;; directories (like Telescope's find_files); affe-grep = in-memory fuzzy grep
;; for small/medium projects. Needs fd (affe-find) and rg (affe-grep).
(use-package affe
  :ensure t
  :after consult
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
                     :preview-key '(:debounce 0.1 any)))

;; vertico-prescient: frecency sorting (recency x frequency, so recent picks
;; float up). Keep Orderless as the matching engine -- prescient for sorting only.
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
