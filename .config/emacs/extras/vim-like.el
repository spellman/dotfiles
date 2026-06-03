;;; Emacs Bedrock
;;;
;;; Extra config: Vim emulation

;;; Usage: Append or require this file from init.el for bindings in Emacs.

;;; Contents:
;;;
;;;  - Core Packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil: vi emulation
(use-package evil
  :ensure t
  :demand t

  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  ;; Let evil-collection supply modal keybindings for other modes. This must
  ;; be set before Evil loads; otherwise Evil installs its own overlapping
  ;; integration bindings, which conflict with evil-collection.
  (setq evil-want-keybinding nil)

  ;; Enable this if you want C-u to scroll up, more like pure Vim
  ;(setq evil-want-C-u-scroll t)

  :config
  (evil-mode)

  ;; If you use Magit, start editing in insert state
  (add-hook 'git-commit-setup-hook 'evil-insert-state)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)

  ;; SPC as the leader key in normal and visual state.
  (evil-set-leader '(normal visual) (kbd "SPC"))

  ;; SPC f -- Find (files); SPC s -- Search.
  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements
      "SPC f" "Find"
      "SPC s" "Search"))
  (evil-define-key 'normal 'global
    (kbd "<leader> f f") #'affe-find            ; project files, fuzzy (rg --files)
    (kbd "<leader> f r") #'consult-recent-file
    (kbd "<leader> f p") #'consult-fd           ; project files, async (fd)
    (kbd "<leader> s s") #'consult-line
    (kbd "<leader> s p") #'consult-ripgrep      ; project grep (rg)
    (kbd "<leader> s f") #'affe-grep            ; project grep, fuzzy
    (kbd "<leader> s i") #'consult-imenu
    (kbd "<leader> /")   #'consult-ripgrep)     ; quick project grep
  )

;; Evil-Collection: Evil-friendly keybindings for many built-in and
;; third-party modes (dired, magit, ibuffer, etc.).
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))
