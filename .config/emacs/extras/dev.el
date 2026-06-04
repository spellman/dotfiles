;;; Emacs Bedrock
;;;
;;; Extra config: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el config if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs
;;;  - Templating

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package project
  :config
  (when (>= emacs-major-version 30)
    (setopt project-mode-line t))          ; show project name in modeline
  ;; Remember the top-level project dirs under ~/Projects -- immediate children
  ;; only, no recursion -- so project-switch-project can reach them without
  ;; visiting each first. Add more roots, or pass t to recurse, if useful later.
  (project-remember-projects-under "~/Projects"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Programming and Data
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Clojure via tree-sitter. clojure-ts-mode autoloads for .clj/.cljs/.cljc/.edn
;; and friends; it needs the `clojure' grammar (installed via the treesit block
;; below / M-x cws/treesit-install-grammars).
(use-package clojure-ts-mode
  :ensure t
  :defer t)

;; Terraform: Emacs has no tree-sitter mode for it, so this is the classic
;; (non-tree-sitter) major mode for .tf/.tfvars files. (CDK for Terraform isn't
;; a distinct language -- those sources are .ts/.py and use the modes for those.)
(use-package terraform-mode
  :ensure t
  :defer t)

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
  ;; already owns the file: a built-in mode, or a package (json-mode, yaml-mode).
  ;; (clojure is handled by the clojure-ts-mode package, below; markdown stays on
  ;; markdown-mode -- Emacs has no built-in tree-sitter markdown mode.)
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
  ;; mode in this Emacs (only the *-ts-mode exists): point the extension straight
  ;; at the tree-sitter mode, again only when the grammar is installed.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Templating
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tempel
  :ensure t
  :defer t
  ;; By default, tempel looks at the file "templates" in
  ;; user-emacs-directory, but you can customize that with the
  ;; tempel-path variable:
  ;; :custom
  ;; (tempel-path (concat user-emacs-directory "custom_template_file"))
  :bind (("M-*" . tempel-insert)
         ("M-+" . tempel-complete)
         :map tempel-map
         ("C-c RET" . tempel-done)
         ("C-<down>" . tempel-next)
         ("C-<up>" . tempel-previous)
         ("M-<down>" . tempel-next)
         ("M-<up>" . tempel-previous))
  :init
  ;; Make a function that adds the tempel expansion function to the
  ;; list of completion-at-point-functions (capf).
  (defun tempel-setup-capf ()
    (add-hook 'completion-at-point-functions #'tempel-expand -1 'local))
  ;; Put tempel-expand on the list whenever you start programming or
  ;; writing prose.
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))
