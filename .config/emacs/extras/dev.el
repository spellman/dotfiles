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
;;;   Syntax checking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Flycheck: on-the-fly syntax and lint checking. Enabled per-buffer through the
;; prog-mode hook, so it runs in programming buffers -- Emacs Lisp, the
;; tree-sitter modes, JSON, YAML (yaml-ts-mode), Clojure, and so on -- while
;; staying out of text, Markdown, fundamental, Magit, and other non-prog buffers.
;; In any buffer with no applicable checker Flycheck simply does nothing, so
;; enabling it broadly is harmless: it only acts where a checker (and the tool
;; that checker drives) is present. The Clojure checker is supplied by
;; flycheck-clj-kondo in the Programming and Data section below.
(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

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
  ;; at clojure-mode -- NOT the tree-sitter mode. Those entries and ours are both
  ;; pushed onto `auto-mode-alist' at startup, so which one wins is an ordering
  ;; race. Remap the classic modes to their tree-sitter counterparts so a Clojure
  ;; file lands in clojure-ts-mode no matter which entry wins. This is the same
  ;; `major-mode-remap-alist' mechanism the tree-sitter block below uses for
  ;; other languages; it is set in :init so it is in place before the first
  ;; Clojure file is visited, and it remaps onto autoloaded modes (no eager load).
  (dolist (remap '((clojure-mode       . clojure-ts-mode)
                   (clojurec-mode      . clojure-ts-clojurec-mode)
                   (clojurescript-mode . clojure-ts-clojurescript-mode)))
    (add-to-list 'major-mode-remap-alist remap))
  :mode (("\\.clj\\'"  . clojure-ts-mode)
         ("\\.cljc\\'" . clojure-ts-clojurec-mode)
         ("\\.cljs\\'" . clojure-ts-clojurescript-mode)
         ("\\.cljd\\'" . clojure-ts-clojuredart-mode)
         ("\\.edn\\'"  . clojure-ts-mode)))

;; CIDER: interactive Clojure development -- a REPL (cider-jack-in / cider-connect),
;; expression evaluation, inline results, the debugger, definition navigation, and
;; documentation lookup. cider-mode is the buffer-local minor mode that provides
;; all of that in a source buffer; hook it onto clojure-ts-mode. The .cljs/.cljc/
;; .cljd tree-sitter modes derive from clojure-ts-mode, so this single hook covers
;; them too. CIDER has supported clojure-ts-mode since version 1.14; it still pulls
;; in the classic clojure-mode package for a few APIs not yet ported, which is
;; expected (the remap above keeps files in the tree-sitter mode regardless). While
;; connected, CIDER installs its own completion-at-point function, so the existing
;; Corfu setup shows REPL-aware completions with no extra wiring.
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
;; create-* refactorings; with no yasnippet here those insert without interactive
;; placeholders, and every other refactoring is unaffected.)
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
;; clojure-ts-mode and its variants). `:after clojure-ts-mode' loads it the first
;; time a Clojure file is visited -- before clojure-ts-mode's prog-mode hook turns
;; on flycheck-mode and picks a checker -- and, because the package depends on
;; flycheck, loading it pulls flycheck in as well. Flycheck itself is enabled in
;; the Syntax checking section above.
(use-package flycheck-clj-kondo
  :ensure t
  :after clojure-ts-mode)

;; Terraform: Emacs has no tree-sitter mode for it, so this is the classic
;; (non-tree-sitter) major mode for .tf/.tfvars files. (CDK for Terraform isn't
;; a distinct language -- those sources are .ts/.py and use the modes for those.)
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
