;;;  ________                                                _______                 __                            __
;;; /        |                                              /       \               /  |                          /  |
;;; $$$$$$$$/ _____  ____   ______   _______  _______       $$$$$$$  | ______   ____$$ | ______   ______   _______$$ |   __
;;; $$ |__   /     \/    \ /      \ /       |/       |      $$ |__$$ |/      \ /    $$ |/      \ /      \ /       $$ |  /  |
;;; $$    |  $$$$$$ $$$$  |$$$$$$  /$$$$$$$//$$$$$$$/       $$    $$</$$$$$$  /$$$$$$$ /$$$$$$  /$$$$$$  /$$$$$$$/$$ |_/$$/
;;; $$$$$/   $$ | $$ | $$ |/    $$ $$ |     $$      \       $$$$$$$  $$    $$ $$ |  $$ $$ |  $$/$$ |  $$ $$ |     $$   $$<
;;; $$ |_____$$ | $$ | $$ /$$$$$$$ $$ \_____ $$$$$$  |      $$ |__$$ $$$$$$$$/$$ \__$$ $$ |     $$ \__$$ $$ \_____$$$$$$  \
;;; $$       $$ | $$ | $$ $$    $$ $$       /     $$/       $$    $$/$$       $$    $$ $$ |     $$    $$/$$       $$ | $$  |
;;; $$$$$$$$/$$/  $$/  $$/ $$$$$$$/ $$$$$$$/$$$$$$$/        $$$$$$$/  $$$$$$$/ $$$$$$$/$$/       $$$$$$/  $$$$$$$/$$/   $$/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings for quick startup and convenience
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup speed, annoyance suppression
;; Raise the GC threshold for the duration of startup so collections don't
;; interrupt it. After startup, gcmh owns the threshold (see the gcmh block in
;; init.el): it keeps GC from firing mid-keystroke and collects on idle. So
;; there is nothing to restore here -- we no longer reset it to the tiny 800 KB
;; default, which used to make GC fire constantly during completion.
(setq gc-cons-threshold (* 64 1024 1024))
;; Read more process output per chunk. The stock 4 KB buffer throttles tools
;; that stream a lot through a process filter -- ripgrep/fd behind fzfa and
;; consult, and language servers behind eglot -- forcing many small reads.
;; 1 MB drains them in far fewer cycles, so fzfa-rg/fzfa-fd populate
;; candidates faster.
(setq read-process-output-max (* 1024 1024))
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Disable the built-in package.el; we manage packages with Elpaca instead
;; (see bootstrap-elpaca.el). This must happen in early-init, before package.el
;; would otherwise activate packages.
(setq package-enable-at-startup nil)

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            ; You can turn off scroll bars by uncommenting these lines:
                            ; (vertical-scroll-bars . nil)
                            (horizontal-scroll-bars . nil)

                            ; Setting the face in here prevents flashes of
                            ; color as the theme gets activated
                            (background-color . "#ffffff")
                            (foreground-color . "#000000")
                            (ns-appearance . dark)
                            (ns-transparent-titlebar . t)))

(menu-bar-mode -1) ; disables menubar
(tool-bar-mode -1) ; disables toolbar
(pixel-scroll-precision-mode 1) ; enable smooth scrolling

(setq inhibit-startup-echo-area-message (user-login-name)
      inhibit-splash-screen t
      use-file-dialog nil ; don't use system file dialog
      tab-bar-new-button-show nil ; don't show new-tab button
      tab-bar-close-button-show nil ; don't show tab-close button
      tab-line-close-button-show nil) ; don't show tab-close button
