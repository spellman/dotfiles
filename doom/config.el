;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Cort Spellman"
      user-mail-address "spellman.cort@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-font (font-spec :family "Monaco" :size 12 :weight 'normal))
;;
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

;; Enable auto-saving visited buffers to their underlying files.
;; The default save interval is every 5 seconds.
(auto-save-visited-mode)

;; From https://www.gnu.org/software/emacs/manual/html_node/emacs/Tab-Bars.html:
;; The variable tab-bar-show controls whether the Tab Bar mode is turned on
;; automatically. If the value is t, then tab-bar-mode is enabled when using the
;; commands that create new tabs. The value 1 hides the tab bar when it has only
;; one tab, and shows it again when more tabs are created. More generally, a
;; value that is a non-negative integer causes the Tab Bar to be displayed only
;; if the number of tabs is greater than that integer. The value nil always
;; keeps the Tab Bar hidden; in this case it’s still possible to switch between
;; named window configurations without displaying the Tab Bar by using M-x
;; tab-next, M-x tab-switcher, and other commands that provide completion on tab
;; names. Also it’s possible to create and close tabs without the Tab Bar by
;; using commands M-x tab-new, M-x tab-close, etc.
(setq tab-bar-show 1)
;; Enable tab-bar-mode.
(tab-bar-mode)

(setq projectile-project-search-path '("~/Projects/"))

;; Clean-up
;; I'm not using evil snipe so don't waste time trying to make it go everytime
;; text is entered.
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(remove-hook 'doom-first-input-hook #'evil-snipe-override-mode)
;; I would remove the following hook with the following (and I have smartparens
;; disabled) but it seems to break Doom in that emacs start-up hangs at loading
;; evil-collection-elisp-mode.
;; (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; Do not omit results from search.
(after! dired-x
  (remove-hook 'dired-mode-hook #'dired-omit-mode))

;; Doom's +default/project-search dispatches by module loaded and should call
;; +vertico/project-search. However, when called through Doom's fn, the vertico
;; fn does not show a preview of the search term in context while the preview is
;; shown when I call the vertico fn directly. There is probably a configuration
;; option to make the Doom default do what I want. For now, though, I'm just
;; going to rebind the keys to call the vertico fn directly in order to enable
;; previews.
(map! :after vertico :leader :desc "Search project" :n "/" #'+vertico/project-search)

(map! :desc "dired-jump to current-file directory" :n "-" #'dired-jump)

;; As per https://github.com/doomemacs/doomemacs/blob/07fca786154551f90f36535bfb21f8ca4abd5027/docs/faq.org#my-new-keybinds-dont-work
(map! :after evil-maps
      :map view-mode-map
      :n "0" nil
      :n "-" nil
      :n "+" nil)

;; Switch between windows.
(map! :after evil
      :desc "Move to window left" :gnv "C-h" #'evil-window-left
      :desc "Move to window left" :gnv "C-j" #'evil-window-down
      :desc "Move to window left" :gnv "C-k" #'evil-window-up
      :desc "Move to window left" :gnv "C-l" #'evil-window-right)

(map! :after better-jumper
      :desc "Jump backward" :gnvi "s-[" #'better-jumper-jump-backward
      :desc "Jump backward" :gnvi "s-]" #'better-jumper-jump-forward)

;; Paredit
(use-package! paredit
  :after evil
  :config
  (map! :map paredit-mode-map
        ;; Movement
        :gni "M-u" #'paredit-backward-up
        :gni "M-o" #'paredit-forward-up
        :gni "M-h" #'paredit-backward
        :gni "M-l" #'paredit-forward
        :gni "M-n" #'paredit-backward-down
        :gni "M-." #'paredit-forward-down

        ;; Insertion
        :gi "(" #'paredit-open-round
        :gi "[" #'paredit-open-square
        :gi "{" #'paredit-open-curly
        :gi "<" #'paredit-open-angled
        :gnvi "M-(" #'paredit-wrap-round
        :gnvi "M-[" #'paredit-wrap-square
        :gnvi "M-{" #'paredit-wrap-curly
        :gnvi "M-<" #'paredit-wrap-angled
        :gnvi "M-\"" #'paredit-meta-doublequote

        ;; S-Expression Manipulation
        :gni "M-j" #'paredit-join-sexps
        :gni "M-w" #'paredit-split-sexp
        :gni "M-?" #'paredit-convolute-sexp
        :gni "M-s" #'paredit-splice-sexp
        :gni "M-a" #'paredit-splice-sexp-killing-backward
        :gni "M-d" #'paredit-splice-sexp-killing-forward
        :gni "M-r" #'paredit-raise-sexp
        :gni "M-q" #'paredit-backward-slurp-sexp
        :gni "M-e" #'paredit-forward-slurp-sexp
        :gni "M-z" #'paredit-backward-barf-sexp
        :gni "M-c" #'paredit-forward-barf-sexp))

;; Enable wrapping of long lines.
(+global-word-wrap-mode +1)

;; (after! lsp-mode
;;   :config
;;   (dolist (dir '("[/\\\\]\\.my-folder\\'"))
;;     (add-to-list 'lsp-file-watch-ignored-directories dir)))

;; Samuel's approach to eliminate LSP warning:
;; "Watching all the files in <project> would require adding watches to <num>
;; directories, so watching the repo may slow Emacs down. Do you want to watch
;; all files in <project>?",
;; from
;; https://emacs-lsp.github.io/lsp-mode/page/faq/#how-do-i-force-lsp-mode-to-forget-the-workspace-folders-for-multi-root-servers-so-the-workspace-folders-are-added-on-demand
;; He said the folders are then added on demand, when you visit them.
;; I could try this to see how it works :/
;; (advice-add 'lsp :before
;;             (lambda (&rest _args)
;;               (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

(add-to-list 'safe-local-variable-values 'lsp-file-watch-ignored-directories)
(add-to-list 'safe-local-variable-values 'lsp-file-watch-ignored-files)
(advice-add 'lsp :before #'hack-local-variables)


;; Git
(setq vc-follow-symlinks t)

;; Tramp
(setq tramp-default-method "ssh")

;; LSP
;; As per
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Make Emacs and evil treat various characters as word characters in various
;; modes. As per
;; https://evil.readthedocs.io/en/latest/faq.html#underscore-is-not-a-word-character
(defun cws/text-mode-hook ()
  (modify-syntax-entry ?_ "w"))

(defun cws/prog-mode-hook ()
  (rainbow-delimiters-mode-enable)
  (modify-syntax-entry ?_ "w"))

(defun cws/lisp-mode-hook ()
  (enable-paredit-mode)
  (modify-syntax-entry ?: "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?> "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?= "w")
  (modify-syntax-entry ?* "w"))

(add-hook 'text-mode-hook #'cws/text-mode-hook)
(after! rainbow-delimiters
  (add-hook 'prog-mode-hook #'cws/prog-mode-hook))
(after! paredit
  (add-hook 'lisp-mode-hook #'cws/lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook #'cws/lisp-mode-hook)
  (add-hook 'clojure-mode-hook #'cws/lisp-mode-hook))

;; Similarly to Emacs’ definition of a word, the definition of a “symbol” is
;; also dependent on the syntax-class of the buffer, which often includes the
;; underscore.
;; The default text objects keymap associates kbd::o with the symbol object,
;; making kbd::cio a good alternative to Vim’s kbd::ciw, for example.
;; The following will swap between the word and symbol objects in the keymap:
(map! :after evil :map evil-outer-text-objects-map :gnv "w" #'evil-a-symbol)
(map! :after evil :map evil-inner-text-objects-map :gnv "w" #'evil-inner-symbol)
(map! :after evil :map evil-outer-text-objects-map :gnv "w" #'evil-a-word)
(map! :after evil :map evil-inner-text-objects-map :gnv "w" #'evil-inner-word)

(map! :after evil :map evil-motion-state-map :n "<tab>" nil)

(map! :after evil
      :prefix ("<tab>" . "+tabs")
      :desc "New tab" :n "n" #'tab-bar-new-tab
      :desc "Switch to tab" :n "<tab>" #'tab-bar-switch-to-tab
      :desc "Prev tab" :n "h" #'tab-bar-switch-to-prev-tab
      :desc "Next tab" :n "l" #'tab-bar-switch-to-next-tab
      :desc "Close tab" :n "d" #'tab-bar-close-tab)

;; This will not change the motion keys, however. One way to make word motions
;; operate as symbol motions is to alias the evil-word thing to the evil-symbol
;; thing.
;; (Many of Evil’s text objects and motions are defined in terms of the
;; thingatpt library, which in this case are defined entirely in terms of
;; forward-THING functions. Thus aliasing one to another should make all
;; motions and text objects implemented in terms of that thing behave the
;; same.)
(defalias 'forward-evil-word 'forward-evil-symbol)

;; Clojure
(setq +clojure-load-clj-refactor-with-lsp t)

;; Format Python code with black formatter, which does not require going through LSP.
(setq-hook! 'python-mode-hook +format-with-lsp nil)

;; As per
;; https://github.com/emacs-vault/emacs-python-black/blob/cc6919e758b5845b201e1cb08a9b5d9a2598a7f1/README.md#installation
(use-package! python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(map! :after python-mode
      :localleader
      :map python-mode-map
      :desc "Blacken Buffer" :n "b b" #'python-black-buffer
      :desc "Blacken Region" :n "b r" #'python-black-region
      :desc "Blacken Statement" :n "b s" #'python-black-statement)
