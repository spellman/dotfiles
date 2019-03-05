;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
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
   '(;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     ;; better-defaults
     auto-completion
     clojure
     deft
     (elm :variables
          ;; elm-format-command "elm-format"
          elm-format-on-save t
          elm-sort-imports-on-save t)
     emacs-lisp
     evil-commentary
     git
     github
     helm
     html
     java
     javascript
     markdown
     org
     pandoc
     python
     react
     (ruby :variables
           ruby-version-manager 'chruby)
     ruby-on-rails
     sql
     (typescript :variables
                 typescript-fmt-on-save t)
     vim-empty-lines
     vinegar
     windows-scripts
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     yaml
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     ;; focused-theme
     kotlin-mode
     deadgrep
     rainbow-mode
     zenburn-theme
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
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https nil
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes
   '(
     ;; focused
     zenburn
     spacemacs-dark
     spacemacs-light
     )

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Monaco"
                               :size 11 ; Mac
                               ;; :size 13 ; Linux
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
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
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
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

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
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

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   package-archives (cons '("local-packages" . "/home/cort/local-emacs-packages/")
                          package-archives)
   auto-window-vscroll nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
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
     spaceline-major-mode-p nil
     spaceline-minor-modes-p nil
     spaceline-buffer-encoding-abbrev-p nil
     spaceline-version-control-p nil

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

    ;; Set pasting to replace a visual selection in Spacemacs, like in Vim:
    ;; From https://emacs.stackexchange.com/questions/14940/emacs-doesnt-paste-in-evils-visual-mode-with-every-os-clipboard
    ;; Imagine the following scenario.  One wants to paste some previously copied
    ;; (from application other than Emacs) text to the system's clipboard in place
    ;; of some contiguous block of text in a buffer.  Hence, one switches to
    ;; `evil-visual-state' and selects the corresponding block of text to be
    ;; replaced.  However, one either pastes some (previously killed) text from
    ;; `kill-ring' or (if `kill-ring' is empty) receives the error: "Kill ring is
    ;; empty"; see `evil-visual-paste' and `current-kill' respectively.  The
    ;; reason why `current-kill' does not return the desired text from the
    ;; system's clipboard is because `evil-visual-update-x-selection' is being run
    ;; by `evil-visual-pre-command' before `evil-visual-paste'.  That is
    ;; `x-select-text' is being run (by `evil-visual-update-x-selection') before
    ;; `evil-visual-paste'.  As a result, `x-select-text' copies the selected
    ;; block of text to the system's clipboard as long as
    ;; `x-select-enable-clipboard' is non-nil (and in this scenario we assume that
    ;; it is).  According to the documentation of `interprogram-paste-function',
    ;; it should not return the text from the system's clipboard if it was last
    ;; provided by Emacs (e.g. with `x-select-text').  Thus, one ends up with the
    ;; problem described above.  To solve it, simply make
    ;; `evil-visual-update-x-selection' do nothing:
    (fset 'evil-visual-update-x-selection 'ignore)

    (setq
     evil-shift-width 2

     ivy-re-builders-alist '((t . ivy--regex-fuzzy))
     ivy-initial-inputs-alist nil

     magit-push-always-verify nil
     magit-repository-directories '("~/Projects/")

     scroll-margin 3

     x-select-enable-clipboard t
     x-select-enable-primary t
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



    ;; Clojure
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

    ;; 2018-08-08
    ;; For CIDER 0.18.0, sayid and refactor-nrepl are broken right now QQ
    ;; https://roomkey.slack.com/archives/C02BXQQUC/p1533737499000101
    ;; https://roomkey.slack.com/archives/C02BXQQUC/p1533737513000146
    (setq sayid-inject-dependencies-at-jack-in nil)
    (setq cljr-inject-dependencies-at-jack-in nil)

    (with-eval-after-load 'cider
      (setq nrepl-log-messages t)
      (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

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
          (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")

          ;; (message "cws clojure repl mode hook")
          ))

      ;; Adam Frey's method of configuring cider-jack-in
      ;; https://roomkey.slack.com/archives/C02BXQQUC/p1538746943000100
      (defun cider-prompt-for-jack-in-options (orig-fn project-type)
        (interactive)
        (let ((res (funcall orig-fn project-type)))
          (read-string "Enter global-options: " res)))
      (advice-add 'cider-jack-in-global-options :around #'cider-prompt-for-jack-in-options)

      (spacemacs/add-to-hook 'cider-repl-mode-hook '(cws-lisp-mode-hook
                                                     cws-clojure-mode-hook
                                                     cws-cider-repl-mode-hook))
      (add-hook 'cider-mode-hook 'turn-on-eldoc-mode)
      )

    ;; Define macro to type (figwheel-sidecar.repl-api/cljs-repl) ENTER
    ;; so that this macro persists across Emacs restarts.
    (fset 'figwheel-cljs-repl
          (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item '([escape 105 40 102 105 103 119 104 101 101 108 45 115 105 100 101 99 97 114 46 114 101 112 108 45 97 112 105 47 99 108 106 115 45 114 101 112 108 41 return escape] 0 "%d") arg)))
    ;; Save the macro to the evil f register:
    (evil-set-register ?f [escape 105 40 102 105 103 119 104 101 101 108 45 115 105 100 101 99 97 114 46 114 101 112 108 45 97 112 105 47 99 108 106 115 45 114 101 112 108 41 return escape])



    ;; Ruby
    (defun cws-ruby-mode-hook ()
      (subword-mode +1) ; CamelCase aware editing operations
      (modify-syntax-entry ?: "w") ; consider : to be part of :word
      (modify-syntax-entry ?! "w") ; consider ! to be part of word!
      (modify-syntax-entry ?? "w") ; consider ? to be part of word?
      (modify-syntax-entry ?= "w") ; consider = to be part of word?
      )
    (add-hook 'ruby-mode-hook 'cws-ruby-mode-hook)



    ;; React Native
    ;; Bring up on-device device options screen for React Native app.
    (with-eval-after-load 'js2
      (defun device-options ()
        (interactive)
        (shell-command "adb shell input keyevent 82"))
      (evil-leader/set-key "d o" 'device-options)
      )



    ;; Web
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



    ;; Org Mode
    (with-eval-after-load 'org
      (setq org-directory "~/Dropbox/org")
      ;; 2018-06-27 https://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
      ;; Task states following "|" complete tasks.
      (setq org-todo-keywords
            '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
      (setq org-agenda-text-search-extra-files '(agenda-archives))
      (setq org-blank-before-new-entry (quote ((heading) (plain-list-item))))
      (setq org-log-done (quote time))
      (setq org-log-reschedule (quote time))

      ;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
      (defun air-org-skip-subtree-if-priority (priority)
        "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
        (let ((subtree-end (save-excursion (org-end-of-subtree t)))
              (pri-value (* 1000 (- org-lowest-priority priority)))
              (pri-current (org-get-priority (thing-at-point 'line t))))
          (if (= pri-value pri-current)
              subtree-end
            nil)))

      (defun air-org-skip-subtree-if-habit ()
        "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (if (string= (org-entry-get nil "STYLE") "habit")
              subtree-end
            nil)))

      (setq org-agenda-custom-commands
            '(("d" "Daily agenda and all TODOs"
               ((tags "PRIORITY=\"A\""
                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-agenda-overriding-header "High-priority unfinished tasks:")))
                (agenda "" ((org-agenda-span 1)))
                (alltodo ""
                         ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                         (air-org-skip-subtree-if-priority ?A)
                                                         (org-agenda-skip-if nil '(scheduled deadline))))
                          (org-agenda-overriding-header "ALL normal priority tasks:"))))
               ((org-agenda-compact-blocks t)))))

      ;; Org Agenda and Capture, from Adam Frey
      (setq org-agenda-files '("~/Dropbox/org"))
      (setq org-default-notes-file "~/Dropbox/org/notes.org")
      (setq org-capture-templates
            `(("t" "To Do" entry (file "todo.org")
               "* TODO %?\n%U\n" :empty-lines 1)
              ("n" "Note" entry (file "") ; "" => org-default-notes-file
               "* %? :NOTE:\n%U\n%a\n" :empty-lines 1)
              ("r" "To Read" entry (file "to-read.org") ; "" => org-default-notes-file
               "* TOREAD %?\n%U\n" :empty-lines 1)
              ;; ("j" "Journal Entry"
              ;;  entry (file+datetree "~/Dropbox/org/journal.org")
              ;;  "* %?"
              ;;  :empty-lines 1)
              ))

      (setq org-refile-targets '("refile.org"))
      )

    ;; Deft for org mode
    ;; https://jblevins.org/projects/deft/
    ;; https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Btools/deft
    ;; https://lepisma.github.io/2016/03/17/org-mode/
    (setq deft-directory "~/Dropbox/org")

    ;; (with-eval-after-load 'desktop
    ;;   ;; don't use desktop mode for terminal
    ;;   (when (display-graphic-p)
    ;;     (desktop-save-mode 1) ;; is x window
    ;;     ())

    ;;   ;; Add variables to desktop saving
    ;;   (add-to-list 'desktop-globals-to-save 'register-alist))

    ))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-boot-parameters "cider repl -s wait")
 '(clean-aindent-mode t)
 '(elm-format-on-save t)
 '(elm-indent-after-keywords '(("of" 4) ("in" 4 0) ("{" 0) "if" "then" "else" "let"))
 '(elm-indent-offset 4)
 '(elm-sort-imports-on-save t)
 '(evil-shift-width 2)
 '(helm-always-two-windows t)
 '(helm-bookmark-show-location t)
 '(helm-display-function 'spacemacs//display-helm-window)
 '(helm-display-header-line nil)
 '(helm-echo-input-in-header-line t)
 '(helm-split-window-in-side-p t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(package-selected-packages
   '(treepy graphql deadgrep deft sesman yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode anaconda-mode pythonic magit-gitflow magit-gh-pulls evil-magit magit magit-popup org-journal skewer-mode org-mime parent-mode flx pkg-info epl popup ghub let-alist goto-chg org-category-capture sql-indent ag powerline projectile iedit anzu diminish hydra spinner bind-key packed avy highlight smartparens bind-map f evil undo-tree dash helm helm-core async s eclim winum flycheck-credo powershell ob-elixir flycheck-mix alchemist company elixir-mode helm-themes helm-swoop helm-projectile helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-css-scss helm-ag ace-jump-helm-line ts-comint tide typescript-mode flycheck tern elm-mode yaml-mode pandoc-mode ox-pandoc focused-theme rainbow-mode pt kotlin-mode web-mode web-beautify tagedit smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv pug-mode projectile-rails rake orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download mmm-mode minitest markdown-toc markdown-mode livid-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc htmlize haml-mode gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh marshal logito pcache ht gh-md feature-mode git-commit with-editor evil-commentary emmet-mode coffee-mode clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider seq queue clojure-mode chruby bundler inf-ruby ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spacemacs-theme spaceline smex restart-emacs request rainbow-delimiters quelpa popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word counsel-projectile column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link))
 '(safe-local-variable-values
   '((cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking)))
 '(spaceline-helm-mode t)
 '(standard-indent 2)
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#3F3F3F")))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cider-boot-parameters "cider repl -s wait")
 '(clean-aindent-mode t)
 '(elm-format-on-save t t)
 '(elm-indent-after-keywords
   (quote
    (("of" 4)
     ("in" 4 0)
     ("{" 0)
     "if" "then" "else" "let")))
 '(elm-indent-offset 4)
 '(elm-sort-imports-on-save t t)
 '(evil-shift-width 2)
 '(helm-always-two-windows t)
 '(helm-bookmark-show-location t t)
 '(helm-display-function (quote spacemacs//display-helm-window))
 '(helm-display-header-line nil)
 '(helm-echo-input-in-header-line t)
 '(helm-split-window-in-side-p t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-bounce-indent-p t)
 '(package-selected-packages
   '(zenburn-theme symon string-inflection spaceline-all-the-icons seeing-is-believing ruby-refactor ruby-hash-syntax rjsx-mode prettier-js pippel pipenv password-generator overseer org-brain nameless mvn meghanada maven-test-mode magithub ghub+ apiwrap magit-svn json-navigator hierarchy importmagic epc ctable concurrent deferred impatient-mode helm-xref helm-purpose window-purpose imenu-list helm-org-rifle helm-git-grep groovy-mode groovy-imports gradle-mode gitignore-templates git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter evil-org evil-lion evil-goggles evil-cleverparens ensime sbt-mode scala-mode elm-test-runner editorconfig doom-modeline eldoc-eval shrink-path all-the-icons memoize diff-hl counsel swiper ivy clojure-cheatsheet centered-cursor-mode browse-at-remote font-lock+ dotenv-mode treepy graphql deadgrep deft sesman yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode dash-functional helm-pydoc cython-mode anaconda-mode pythonic magit-gitflow magit-gh-pulls evil-magit magit magit-popup org-journal skewer-mode org-mime parent-mode flx pkg-info epl popup ghub let-alist goto-chg org-category-capture sql-indent ag powerline projectile iedit anzu diminish hydra spinner bind-key packed avy highlight smartparens bind-map f evil undo-tree dash helm helm-core async s eclim winum flycheck-credo powershell ob-elixir flycheck-mix alchemist company elixir-mode helm-themes helm-swoop helm-projectile helm-mode-manager helm-gitignore helm-flx helm-descbinds helm-css-scss helm-ag ace-jump-helm-line ts-comint tide typescript-mode flycheck tern elm-mode yaml-mode pandoc-mode ox-pandoc focused-theme rainbow-mode pt kotlin-mode web-mode web-beautify tagedit smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv pug-mode projectile-rails rake orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download mmm-mode minitest markdown-toc markdown-mode livid-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc htmlize haml-mode gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist gh marshal logito pcache ht gh-md feature-mode git-commit with-editor evil-commentary emmet-mode coffee-mode clj-refactor inflections edn multiple-cursors paredit yasnippet peg cider-eval-sexp-fu cider seq queue clojure-mode chruby bundler inf-ruby ws-butler window-numbering which-key wgrep volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spacemacs-theme spaceline smex restart-emacs request rainbow-delimiters quelpa popwin persp-mode pcre2el paradox org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint ivy-hydra info+ indent-guide ido-vertical-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make google-translate golden-ratio flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump define-word counsel-projectile column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link))
 '(safe-local-variable-values
   '((cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend")
     (elixir-enable-compilation-checking . t)
     (elixir-enable-compilation-checking)))
 '(spaceline-helm-mode t)
 '(standard-indent 2)
 '(typescript-indent-level 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#DCDCCC" :background "#3F3F3F")))))
)
