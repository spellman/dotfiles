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
(setq bedrock--initial-gc-threshold gc-cons-threshold)
(setq gc-cons-threshold 10000000)
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
