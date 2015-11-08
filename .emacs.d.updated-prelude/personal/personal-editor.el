(require 'personal-packages)

;; Disable whitespace visualization.
(setq prelude-whitespace nil)
(setq whitespace-mode nil)
(lambda ()
  (interactive)
  (let ((current-prefix-arg '(4))) ; C-u prefix
    (call-interactively 'global-whitespace-mode)))

(define-key prog-mode-map (kbd "M-(")  nil)
(define-key prog-mode-map (kbd "M-[") nil)
(define-key prog-mode-map (kbd "M-\"") nil)

;; Disable autosave and backup files.
(setq prelude-auto-save nil)
(setq auto-save-default nil)
(setq vc-make-backup-files nil)
(make-local-variable 'backup-inhibited)
(setq backup-inhibited t)
(setq delete-by-moving-to-trash nil)
(setq make-backup-files nil)

;; Don't blink matching parens.
(show-paren-mode +1)
(setq blink-matching-paren nil)   

;; Do not highlight the current line.
(global-hl-line-mode -1)

;; Show only file / directory names in Dired mode; instead of ls -l style details.
(require 'dired-details+)
(setq dired-details-hidden-string "")
;; dired - reuse dired buffer
(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)

;; Undo
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)
(setq undo-limit 1000000)
(setq undo-strong-limit 1500000)

;; Allow sentence to end with single space after period.
;; (I.e., do not require double space.)
(setq sentence-end-double-space nil)

;; Comment out electric-indent while I try aggressive-indent.
;; Auto-indent on return, instead of requiring <tab>.
;;(electric-indent-mode t)
(global-aggressive-indent-mode 1)
;;(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; Ag
(setq ag-highlight-search t)
(setq ag-reuse-buffers 't)

;; Disable spellchecking.
(setq prelude-flyspell nil)
