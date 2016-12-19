;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     ;; better-defaults
     clojure
     emacs-lisp
     evil-commentary
     git
     github
     html
     ivy
     javascript
     markdown
     org
     pandoc
     react
     (ruby :variables
           ruby-version-manager 'chruby)
     ruby-on-rails
     vim-empty-lines
     vinegar
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     focused-theme
     kotlin-mode
     pt
     rainbow-mode
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     highlight-parentheses
     jade-mode
     smartparens
     )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update t
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading t
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes
   '(
     focused
     spacemacs-dark
     spacemacs-light
     )
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Monaco"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 100
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters nil
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("pt" "ag" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   package-archives (cons '("local-packages" . "/home/cort/local-emacs-packages/")
                          package-archives)
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  (progn
    (require 'package-x)

    (setq-default
     global-hl-line-mode nil

     ;; js2-mode
     js2-basic-offset 2

     ;; web-mode
     css-indent-offset 2
     web-mode-markup-indent-offset 2
     web-mode-css-indent-offset 2
     web-mode-code-indent-offset 2
     web-mode-sql-indent-offset 2
     web-mode-attr-indent-offset 2
     web-mode-indent-style 2
     )

    (setq
     evil-shift-width 2

     ivy-re-builders-alist '((t . ivy--regex-fuzzy))
     ivy-initial-inputs-alist nil

     magit-push-always-verify nil
     magit-repository-directories '("~/Projects/")

     scroll-margin 3
     )

    ;; Make evil-mode up/down operate in screen lines instead of logical lines
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
    (define-key evil-visual-state-map "j" 'evil-next-visual-line)
    (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

    ;; Free up C-h in normal mode
    (global-set-key (kbd "C-h") nil)
    (global-set-key (kbd "C-c h") 'help-map)

    ;; Window Movements
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

    ;; Helper function to bind a list of ex-mode mappings.
    (defun ex-mode-mapping (cmd)
      (let ((binding (car cmd))
            (fn (cdr cmd)))
        (evil-ex-define-cmd binding fn)))

    ;; Evil Ex
    (mapc 'ex-mode-mapping
          '(("[E]xplore"                . dired-jump)
            ))

    (with-eval-after-load 'cider-repl
      (define-key cider-repl-mode-map (kbd "M-[ a") 'cider-repl-backward-input)
      (define-key cider-repl-mode-map (kbd "M-[ b") 'cider-repl-forward-input)
      (evil-define-key 'insert cider-repl-mode-map (kbd "C-d") 'cider-repl-backward-input)
      (evil-define-key 'insert cider-repl-mode-map (kbd "C-f") 'cider-repl-backward-input)
      (evil-define-key 'insert cider-repl-mode-map (kbd "C-h") 'evil-window-left)
      (evil-define-key 'insert cider-repl-mode-map (kbd "C-j") 'evil-window-down)
      (evil-define-key 'insert cider-repl-mode-map (kbd "C-k") 'evil-window-up)
      (evil-define-key 'insert cider-repl-mode-map (kbd "C-l") 'evil-window-right)
      (evil-define-key 'insert cider-repl-mode-map (kbd "C-f") 'cider-repl-newline-and-indent)
      )

    (defun cws-prog-mode-hook ()
      "Default coding hook, useful with any programming language."
      (progn
        (rainbow-delimiters-mode 1)
        (modify-syntax-entry ?_ "w")     ; consider _ to be part of word_
        ;; (message "cws prog mode hook")
        ))
    (add-hook 'prog-mode-hook 'cws-prog-mode-hook)

    (defun cws-lisp-mode-hook ()
      (progn
        (enable-paredit-mode)
        (subword-mode +1)
        (modify-syntax-entry ?: "w")     ; consider : to be part of :word
        (modify-syntax-entry ?! "w")     ; consider ! to be part of word!
        (modify-syntax-entry ?? "w")     ; consider ? to be part of word?
        (modify-syntax-entry ?- "w")     ; consider - to be part of word-
        (modify-syntax-entry ?> "w")     ; consider > to be part of word>
        (modify-syntax-entry ?< "w")     ; consider < to be part of word<
        (modify-syntax-entry ?= "w")     ; consider = to be part of word=
        (modify-syntax-entry ?* "w")     ; consider * to be part of word*
        ;; (message "cws lisp mode hook")
        ))
    (spacemacs/add-to-hook 'lisp-mode-hook '(cws-lisp-mode-hook))

    (defun cws-emacs-lisp-mode-hook ()
      (progn
        (turn-on-eldoc-mode)
        (rainbow-mode +1)
        ;; (message "cws emacs lisp mode hook")
        ))
    (spacemacs/add-to-hook 'emacs-lisp-mode-hook '(cws-lisp-mode-hook
                                                   cws-emacs-lisp-mode-hook))

    (with-eval-after-load 'clojure-mode
      (defun cws-clojure-mode-hook ()
        (progn
          (put-clojure-indent 'match 1)
          ;; (message "cws clojure mode hook")
          ))
      (spacemacs/add-to-hook 'clojure-mode-hook '(cws-lisp-mode-hook
                                                  cws-clojure-mode-hook))
      )

    (with-eval-after-load 'cider
      (setq nrepl-log-messages t)

      (defun cws-cider-repl-mode-hook ()
        (progn
          (enable-paredit-mode)
          (rainbow-delimiters-mode +1)
          (whitespace-mode -1)
          (linum-mode -1)
          (setq cider-repl-use-pretty-printing t)
          (setq cider-repl-pop-to-buffer-on-connect t)
          (setq cider-repl-use-clojure-font-lock t)
          (setq cider-repl-wrap-history t)
          (setq cider-repl-history-size 1000)
          ;; (message "cws clojure repl mode hook")
          ))
      (spacemacs/add-to-hook 'cider-repl-mode-hook '(cws-lisp-mode-hook
                                                     cws-clojure-mode-hook
                                                     cws-cider-repl-mode-hook))
      (add-hook 'cider-mode-hook 'turn-on-eldoc-mode)
      )

    (defun cws-ruby-mode-hook ()
      (subword-mode +1) ; CamelCase aware editing operations
      (modify-syntax-entry ?: "w") ; consider : to be part of :word
      (modify-syntax-entry ?! "w") ; consider ! to be part of word!
      (modify-syntax-entry ?? "w") ; consider ? to be part of word?
      (modify-syntax-entry ?= "w") ; consider = to be part of word?
      )
    (add-hook 'ruby-mode-hook 'cws-ruby-mode-hook)

    (with-eval-after-load 'js2
      (defun device-options ()
        (interactive)
        (shell-command "adb shell input keyevent 82"))
      (evil-leader/set-key "d o" 'device-options)
      )

    (with-eval-after-load 'web-mode
      (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
      (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
      (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))

    ;; Paredit
    (with-eval-after-load 'paredit
      (define-key paredit-mode-map (kbd "M-s") nil)
      (define-paredit-pair ?\" ?\" "quote")
      (define-key paredit-mode-map (kbd "M-g") 'paredit-forward)
      (define-key paredit-mode-map (kbd "M-f") 'paredit-forward-down)
      (define-key paredit-mode-map (kbd "M-d") 'paredit-backward-up)
      (define-key paredit-mode-map (kbd "M-s") 'paredit-backward)
      (define-key paredit-mode-map (kbd "{") 'paredit-open-curly)
      (define-key paredit-mode-map (kbd "}") 'paredit-close-curly)
      (evil-leader/set-key
        "w (" 'paredit-wrap-round
        "w [" 'paredit-wrap-square
        "w {" 'paredit-wrap-curly
        "w <" 'paredit-wrap-angled
        "w \"" 'paredit-wrap-quote
        "s s" 'paredit-splice-sexp
        "s f" 'paredit-splice-sexp-killing-forward
        "s b" 'paredit-splice-sexp-killing-backward
        "s r" 'paredit-raise-sexp
        "L" 'paredit-forward-slurp-sexp
        "H" 'paredit-backward-slurp-sexp
        ">" 'paredit-forward-barf-sexp
        "<" 'paredit-backward-barf-sexp
        "S" 'paredit-split-sexp
        "j" 'paredit-join-sexps
        ))

    (defun conditionally-enable-paredit-mode ()
      "Enable `paredit' in the minibuffer, during `eval-expression'."
      (if (eq this-command 'eval-expression)
          (enable-paredit-mode)))
    (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

    (defun override-slime-repl-bindings-with-paredit ()
      (define-key slime-repl-mode-map
        (read-kbd-macro paredit-backward-delete-key) nil))
    (add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
  ))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(standard-indent 2)
 '(evil-shift-width 2)
 '(js2-bounce-indent-p t)
 '(package-selected-packages
   (quote
    (focused-theme rainbow-mode pt kotlin-mode web-mode web-beautify tagedit smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv pug-mode projectile-rails rake orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download mmm-mode minitest markdown-toc markdown-mode magit-gitflow magit-gh-pulls livid-mode skewer-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc htmlize haml-mode gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh marshal logito pcache ht gh-md feature-mode evil-magit magit magit-popup git-commit with-editor evil-commentary emmet-mode coffee-mode clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider seq queue clojure-mode chruby bundler inf-ruby ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spacemacs-theme spaceline smex restart-emacs request rainbow-delimiters quelpa popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word counsel-projectile column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
