(require 'personal-packages)
(require 'personal-defuns)
(require 'evil)
(require 'paredit)
(require 'cider)
(require 'projectile)
(require 'magit)
;(require 'revive)

;;; Prelude Global keybindings
;; Align your code in a pretty way.
;(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Start proced in a similar manner to dired
(unless (eq system-type 'darwin)
    (global-set-key (kbd "C-x p") 'proced))

(global-set-key (kbd "C-x m") 'eshell) ; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t))) ; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M-m") 'shell) ; Start a regular shell if you prefer that.
(define-key 'help-command "A" 'apropos) ; A complementary binding to the apropos-command (C-h a)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

;(global-set-key (kbd "M-/") 'hippie-expand) ; use hippie-expand instead of dabbrev
;(global-set-key (kbd "C-x C-b") 'ibuffer) ; replace buffer-menu with ibuffer
(unless (fboundp 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f11>") 'prelude-fullscreen))
(global-set-key (kbd "<f12>") 'menu-bar-mode) ; toggle menu-bar visibility
(global-set-key (kbd "C-=") 'er/expand-region)
;;;



(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-M-u") 'universal-argument) ;; C-u is Evil scroll-up half page

;; ESC always quits.
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-quit)

;; Use tab to move between links in help mode.
(evil-define-key 'motion help-mode-map (read-kbd-macro "TAB") 'forward-button)

;; Easy window movement
(global-set-key (kbd "C-w") 'evil-window-map)

(global-set-key (kbd "C-c h") help-map)

;; Evil normal
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-A") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-A") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "g s") 'prelude-goto-symbol)
(define-key evil-normal-state-map (kbd "Y") 'prelude-yank-to-end-of-line)
(define-key evil-normal-state-map (kbd "SPC") 'evil-forward-char)

;; Evil insert
;; fix conflict with electric-indent-mode in 24.4
(define-key evil-insert-state-map [remap newline] 'newline)
(define-key evil-insert-state-map [remap newline-and-indent] 'newline-and-indent)

;; Evil visual
(define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)

;; Evil Ex (ex-mode-mapping is defined in core/custom-defuns.el)
(mapc 'ex-mode-mapping
      '(("!"                        . shell-command)
        ("[buff]ers"                . ibuffer)
        ("reset-directory"          . reset-current-dir)
        ("history"                  . git-timemachine)
        ("[er]eval-region"          . eval-region)
        ("[eb]eval-buffer"          . eval-buffer)
        ("ag"                       . projectile-ag)
        ("agl"                      . ag)
        ("agf"                      . ag-project-files)
        ("sh"                       . ansi-term)
        ("[E]xplore"                . dired-jump)
        ))

;; Mapleader
(evil-leader/set-leader ",")
(evil-leader/set-key
  "/" 'evil-ex-nohighlight
  ;"ci" 'evilnc-comment-or-uncomment-lines
  ;"cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  ;"ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  ;"cc" 'evilnc-copy-and-comment-lines
  ;"cp" 'evilnc-comment-or-uncomment-paragraphs
  ;"cr" 'comment-or-uncomment-region
  ;"cv" 'evilnc-toggle-invert-comment-line-by-line
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
  )



;;; Paredit
(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-s") nil)
     ;; Move to next/previous s-exp at same level.
     (define-paredit-pair ?\" ?\" "quote")
     (define-key paredit-mode-map (kbd "M-g") 'paredit-forward)
     (define-key paredit-mode-map (kbd "M-f") 'paredit-forward-down)
     (define-key paredit-mode-map (kbd "M-d") 'paredit-backward-up)
     (define-key paredit-mode-map (kbd "M-s") 'paredit-backward)
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
       )))



;;; Lisp
(define-key read-expression-map (kbd "TAB") 'completion-at-point)

;;; Cider (for Clojure) (note that clojure-mode inherits Lisp settings)
(eval-after-load 'cider
  '(progn
    ;;;; Custom Cider key bindings
    ;;;  Unmap default Cider keys of interest.
    (define-key cider-mode-map (kbd "C-c C-n") nil) ; 'cider-eval-ns-form
    (define-key cider-mode-map (kbd "C-c M-n") nil) ; 'cider-repl-set-ns
    (define-key cider-mode-map (kbd "C-c C-k") nil) ; 'cider-load-current-buffer
    (define-key cider-mode-map (kbd "C-c C-l") nil) ; 'cider-load-file
    (define-key cider-mode-map (kbd "C-c M-p") nil) ; 'cider-insert-last-sexp-in-repl
    (define-key cider-mode-map (kbd "C-x C-e") nil) ; 'cider-eval-last-sexp
    (define-key cider-mode-map (kbd "C-c C-e") nil) ; 'cider-eval-last-sexp
    (define-key cider-mode-map (kbd "C-M-x") nil) ; 'cider-eval-defun-at-point
    (define-key cider-mode-map (kbd "C-c C-c") nil) ; 'cider-eval-defun-at-point
    (define-key cider-mode-map (kbd "C-c C-m") nil) ; 'cider-macroexpand-1
    (define-key cider-mode-map (kbd "C-:") nil) ; 'clojure-toggle-keyword-string

    ;; Idea of expression-eval keys: emulate vim's operator-object paradigm
    ;; of verb (v) noun (n).
    ;; e n -> eval (v) namespace (n)
    ;; n b -> switch REPL namespace (v) to buffer namespace (n)
    ;;
    ;; l b -> load (v) buffer (n)
    ;; l f -> load (v) file (n)
    ;;
    ;; e e -> echo (v) evaluated last s-exp (n)
    ;; b e -> insert into buffer (v) evaluated last s-exp (n)
    ;; ...

    (evil-leader/set-key
      ;; Eval namespace form.
      ;; e n -> eval (v) namespace (n)
      "e n" 'cider-eval-ns-form

      ;; Switch to namespace of current buffer.
      ;; n b -> switch repl namespace (v) to buffer namespace (n)
      "n b" 'cider-repl-set-ns

      ;; load current buffer.
      ;; l b -> load (v) buffer (n)
      "l b" 'cider-load-buffer

      ;; Load file.
      ;; l f -> load (v) file (n)
      "l f" 'cider-load-file

      ;; Insert form preceding point into REPL.
      ;; i e -> insert (into REPL) (v) last s-exp (n)
      "i e" 'cider-insert-last-sexp-in-repl

      ;; Insert top-level form form at point into REPL.
      ;; i E -> insert (into REPL) (v) top-level form at point (n)
      "i E" 'cider-insert-defun-in-repl

      ;; Insert form preceding point into REPL and eval it.
      ;; r e -> insert into REPL (v) evaluated last s-exp (n)
      "r e" (lambda ()
              (interactive)
              (let ((current-prefix-arg '(4))) ; C-u prefix
                (call-interactively 'cider-insert-last-sexp-in-repl)))

      ;; Insert top-level form form at point into REPL and eval it.
      ;; r E -> insert into REPL (v) evaluated top-level form at point (n)
      "r E" (lambda ()
              (interactive)
              (let ((current-prefix-arg '(4))) ; C-u prefix
                (call-interactively 'cider-insert-defun-in-repl)))
      

      ;; Eval form preceding point, echo result.
      ;; e e -> echo (v) evaluated last s-exp (n)
      "e e" 'cider-eval-last-sexp

      ;; Eval form preceding point, insert result into current buffer.
      ;; b e -> insert into buffer (v) evaluated last s-exp (n)
      "b e" (lambda ()
              (interactive)
              (let ((current-prefix-arg '(4))) ; C-u prefix
                (call-interactively 'cider-eval-last-sexp)))

      ;; Eval top-level form at point, echo result.
      ;; e E -> echo (v) evaluated top-level form at point (n)
      "e E" 'cider-eval-defun-at-point

      ;; Eval top-level form at point, insert result into current buffer.
      ;; b E -> insert into buffer (v) evaluated top level form at point (n)
      "b E" (lambda ()
              (interactive)
              (let ((current-prefix-arg '(4))) ; C-u prefix
                (call-interactively 'cider-eval-defun-at-point)))

      ;; Eval region (visual selection), echo result.
      ;; e r -> echo (v) evaluated region (n)
      "e r" 'cider-eval-region

      ;; Macroexpand-1 the form at point.
      ;; m 1 -> insert into buffer (v) the macroexpand-1 of form at point (n).
      "m 1" 'cider-macroexpand-1

      ;; Macroexpand the form at point.
      ;; m m -> insert into buffer (v) the macroexpand of form at point (n).
      "m m" (lambda ()
              (interactive)
              (let ((current-prefix-arg '(4))) ; C-u prefix
                (call-interactively 'cider-macroexpand-1)))

      ":" 'clojure-toggle-keyword-string
      )
    ))

(eval-after-load 'cider-repl
  '(progn
    (define-key cider-repl-mode-map (kbd "M-[ a") 'cider-repl-backward-input)
    (define-key cider-repl-mode-map (kbd "M-[ b") 'cider-repl-forward-input)
    (evil-define-key 'insert cider-repl-mode-map (kbd "C-d") 'cider-repl-backward-input)
    (evil-define-key 'insert cider-repl-mode-map (kbd "C-f") 'cider-repl-backward-input)
    ))



;;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)



;;; Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Evil Org mode, as per https://github.com/mbriggs/.emacs.d/blob/master/init/init-org.el
(define-minor-mode evil-org-mode
  "Buffer local minor mode for evil-org"
  :init-value nil
  :lighter " EvO"
  :keymap (make-sparse-keymap) ; defines evil-org-mode-map
  :group 'evil-org)

(add-hook 'org-mode-hook 'evil-org-mode) ;; Only load with org-mode.

;; normal state shortcuts
(evil-define-key 'normal evil-org-mode-map
  "gh" 'outline-up-heading
  "gj" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
         'org-forward-same-level
         'org-forward-heading-same-level)
  "gk" (if (fboundp 'org-backward-same-level)
         'org-backward-same-level
         'org-backward-heading-same-level)
  "gl" 'outline-next-visible-heading
  "t" 'org-todo
  "T" '(lambda () (interactive) (evil-org-eol-call '(org-insert-todo-heading nil)))
  "H" 'org-beginning-of-line
  "L" 'org-end-of-line
  ";t" 'org-show-todo-tree
  "o" '(lambda () (interactive) (evil-org-eol-call 'evil-org-always-insert-item))
  "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
  "$" 'org-end-of-line
  "^" 'org-beginning-of-line
  "<" 'org-metaleft
  ">" 'org-metaright
  ";a" 'org-agenda
  "-" 'org-cycle-list-bullet
  (kbd "TAB") 'org-cycle)

;; Normal and insert state shortcuts.
(mapc (lambda (state)
        (evil-define-key state evil-org-mode-map
          (kbd "M-l") 'org-metaright
          (kbd "M-h") 'org-metaleft
          (kbd "M-k") 'org-metaup
          (kbd "M-j") 'org-metadown
          (kbd "M-L") 'org-shiftmetaright
          (kbd "M-H") 'org-shiftmetaleft
          (kbd "M-K") 'org-shiftmetaup
          (kbd "M-J") 'org-shiftmetadown
          (kbd "M-o") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-heading)
                             (org-metaright))))
          (kbd "M-t") '(lambda () (interactive)
                         (evil-org-eol-call
                          '(lambda()
                             (org-insert-todo-heading nil)
                             (org-metaright))))
          ))
      '(normal insert))



;;; Projectile
(eval-after-load 'projectile
  '(progn
     (define-key evil-normal-state-map (kbd "C-p") nil)
     (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)))

;;; Revive
;(eval-after-load 'revive
;  '(progn
;     (define-key ctl-x-map (kbd "S") 'save-current-configuration)
;     (define-key ctl-x-map (kbd "F") 'resume)
;     (define-key ctl-x-map (kbd "K") 'wipe)))

;; (define-minor-mode evil-projectile-mode
;;   "Buffer local minor mode for evil-projectile"
;;   :init-value nil
;;   :keymap (make-sparse-keymap) ; defines evil-projectile-mode-map
;;   :group 'evil-projectile)
;; (add-hook 'projectile-mode-hook 'evil-projectile-mode) ;; Only load with projectile-mode.
;; (evil-define-key 'normal evil-projectile-mode-map (kbd "C-p") 'projectile-find-file)

(provide 'custom-keybindings)
