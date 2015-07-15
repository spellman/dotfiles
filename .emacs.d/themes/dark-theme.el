(deftheme dark "The dark color theme")

;;; Color Palette

(defvar dark-colors-alist
  '(("dark-fg+1"          . "#FFFFEF")
    ("dark-fg"            . "#DADADA")
    ("dark-fg-05"         . "#8A8A8A")
    ("dark-fg-1"          . "#656555")
    ("dark-fg-15"         . "#767676")
    ("dark-fg-175"        . "#585858")
    ("dark-bg-2"          . "#000000")
    ("dark-bg-1"          . "#262626")
    ("dark-bg"            . "#303030")
    ("dark-bg+05"         . "#494949")
    ("dark-bg+1"          . "#4F4F4F")
    ("dark-bg+2"          . "#5F5F5F")
    ("dark-bg+3"          . "#6F6F6F")
    ("diff-removed-fg"    . "#AD2828")
    ("diff-removed-bg"    . "#FFDDDD")
    ("dark-red+1"         . "#DCA3A3")
    ("dark-red"           . "#CC9393")
    ("dark-red-1"         . "#BC8383")
    ("dark-red-2"         . "#AC7373")
    ("dark-red-3"         . "#9C6363")
    ("dark-red-4"         . "#8C5353")
    ("dark-orange"        . "#DFAF8F")
    ("dark-yellow"        . "#F0DFAF")
    ("dark-yellow-1"      . "#E0CF9F")
    ("dark-yellow-2"      . "#D0BF8F")
    ("dark-yellow-3"      . "#FFFF00")
    ("dark-green-1"       . "#5F7F5F")
    ("dark-green"         . "#7F9F7F")
    ("dark-green+1"       . "#8FB28F")
    ("dark-green+2"       . "#9FC59F")
    ("dark-green+3"       . "#AFD8AF")
    ("dark-green+4"       . "#BFEBBF")
    ("diff-added-fg"      . "#22AA22")
    ("diff-added-bg"      . "#DDFFDD")
    ("dark-cyan"          . "#93E0E3")
    ("dark-blue+1"        . "#94BFF3")
    ("dark-blue"          . "#8CD0D3")
    ("dark-blue-1"        . "#7CB8BB")
    ("dark-blue-2"        . "#6CA0A3")
    ("dark-blue-3"        . "#5C888B")
    ("dark-blue-4"        . "#4C7073")
    ("dark-blue-5"        . "#366060")
    ("dark-magenta"       . "#DC8CC3"))
  "List of dark colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro dark-with-color-variables (&rest body)
  "`let' bind all colors defined in `dark-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   dark-colors-alist))
     ,@body))

;;; Theme Faces
(dark-with-color-variables
  (custom-theme-set-faces
   'dark
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,dark-yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,dark-yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,dark-fg :background ,dark-bg))))
   `(cursor ((t (:foreground ,dark-fg :background ,dark-fg+1))))
   `(escape-glyph ((t (:foreground ,dark-yellow :bold t))))
   `(fringe ((t (:foreground ,dark-fg :background ,dark-bg+1))))
   `(header-line ((t (:foreground ,dark-yellow
                                  :background ,dark-bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background ,dark-bg))))
   `(success ((t (:foreground ,dark-green :weight bold))))
   `(warning ((t (:foreground ,dark-orange :weight bold))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,dark-yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,dark-green))))
   `(compilation-error-face ((t (:foreground ,dark-red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,dark-fg))))
   `(compilation-info-face ((t (:foreground ,dark-blue))))
   `(compilation-info ((t (:foreground ,dark-green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,dark-green))))
   `(compilation-line-face ((t (:foreground ,dark-yellow))))
   `(compilation-line-number ((t (:foreground ,dark-yellow))))
   `(compilation-message-face ((t (:foreground ,dark-blue))))
   `(compilation-warning-face ((t (:foreground ,dark-orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,dark-green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,dark-red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,dark-yellow :weight bold))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,dark-fg))))
   `(grep-error-face ((t (:foreground ,dark-red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,dark-blue))))
   `(grep-match-face ((t (:foreground ,dark-orange :weight bold))))
   `(match ((t (:background ,dark-bg-1 :foreground ,dark-orange :weight bold))))
;;;;; isearch
   `(isearch ((t (:foreground ,dark-bg :background ,dark-fg))))
   `(isearch-fail ((t (:foreground ,dark-fg :background ,dark-red-4))))
   `(lazy-highlight ((t (:foreground ,dark-yellow-3 :background ,dark-bg-1 :weight bold))))

   `(menu ((t (:foreground ,dark-fg :background ,dark-bg))))
   `(minibuffer-prompt ((t (:foreground ,dark-fg))))
   `(mode-line
     ((,class (:foreground ,dark-fg-15
                           :background ,dark-bg-1
                           :weight bold
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,dark-fg-15 :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,dark-fg-175
                      :background ,dark-bg-1
                      :box (:line-width -1 :style released-button)))))
   ;`(region ((,class (:background ,dark-bg-1))
   ;          (t :inverse-video t)))
   `(region ((,class (:background ,dark-fg :foreground ,dark-bg))))
   `(secondary-selection ((t (:background ,dark-bg+2))))
   `(trailing-whitespace ((t (:background ,dark-red))))
   `(vertical-border ((t (:foreground ,dark-fg-15))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,dark-fg))))
   `(font-lock-comment-face ((t (:foreground ,dark-fg-05))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,dark-fg-05))))
   `(font-lock-constant-face ((t (:foreground ,dark-fg))))
   `(font-lock-doc-face ((t (:foreground ,dark-green+2))))
   `(font-lock-function-name-face ((t (:foreground ,dark-fg))))
   `(font-lock-keyword-face ((t (:foreground ,dark-fg))))
   `(font-lock-negation-char-face ((t (:foreground ,dark-fg))))
   `(font-lock-preprocessor-face ((t (:foreground ,dark-fg))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,dark-fg))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,dark-fg))))
   `(font-lock-string-face ((t (:foreground ,dark-fg))))
   `(font-lock-type-face ((t (:foreground ,dark-fg))))
   `(font-lock-variable-name-face ((t (:foreground ,dark-fg))))
   `(font-lock-warning-face ((t (:foreground ,dark-fg))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,dark-fg))))
   `(newsticker-default-face ((t (:foreground ,dark-fg))))
   `(newsticker-enclosure-face ((t (:foreground ,dark-green+3))))
   `(newsticker-extra-face ((t (:foreground ,dark-bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,dark-fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,dark-green))))
   `(newsticker-new-item-face ((t (:foreground ,dark-blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,dark-red))))
   `(newsticker-old-item-face ((t (:foreground ,dark-bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,dark-fg))))
   `(newsticker-treeview-face ((t (:foreground ,dark-fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,dark-green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,dark-fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,dark-blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,dark-red))))
   `(newsticker-treeview-old-face ((t (:foreground ,dark-bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,dark-bg-1 :foreground ,dark-yellow))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,dark-fg-1 :background ,dark-bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,dark-green+2 :background ,dark-bg :inverse-video nil))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,dark-green+1))))
   `(android-mode-error-face ((t (:foreground ,dark-orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,dark-fg))))
   `(android-mode-verbose-face ((t (:foreground ,dark-green))))
   `(android-mode-warning-face ((t (:foreground ,dark-yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,dark-cyan :weight bold))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,dark-red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,dark-yellow))))
   `(font-latex-italic-face ((t (:foreground ,dark-cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,dark-orange))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,dark-bg+3 :foreground ,dark-bg-2))))
   `(ac-selection-face ((t (:background ,dark-blue-4 :foreground ,dark-fg))))
   `(popup-tip-face ((t (:background ,dark-yellow-2 :foreground ,dark-bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,dark-blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,dark-bg-1))))
   `(popup-isearch-match ((t (:background ,dark-bg :foreground ,dark-fg))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,dark-fg :background ,dark-bg+1))))
   `(company-tooltip-selection ((t (:foreground ,dark-fg :background ,dark-bg-1))))
   `(company-tooltip-mouse ((t (:background ,dark-bg-1))))
   `(company-tooltip-common ((t (:foreground ,dark-green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,dark-green+2))))
   `(company-scrollbar-fg ((t (:background ,dark-green+1))))
   `(company-scrollbar-bg ((t (:background ,dark-bg-1))))
   `(company-preview ((t (:background ,dark-green+1))))
   `(company-preview-common ((t (:background ,dark-bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,dark-yellow-1 :foreground ,dark-bg))))
   `(bm-fringe-face ((t (:background ,dark-yellow-1 :foreground ,dark-bg))))
   `(bm-fringe-persistent-face ((t (:background ,dark-green-1 :foreground ,dark-bg))))
   `(bm-persistent-face ((t (:background ,dark-green-1 :foreground ,dark-bg))))
;;;;; clojure-test-mode
   `(clojure-test-failure-face ((t (:foreground ,dark-orange :weight bold))))
   `(clojure-test-error-face ((t (:foreground ,dark-red :weight bold))))
   `(clojure-test-success-face ((t (:foreground ,dark-green+1 :weight bold))))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,dark-blue :foreground ,dark-bg))))
   `(ctbl:face-continue-bar ((t (:background ,dark-bg-1 :foreground ,dark-bg))))
   `(ctbl:face-row-select ((t (:background ,dark-cyan :foreground ,dark-bg))))
;;;;; diff
   `(diff-added ((,class (:foreground ,dark-green+4 :background nil))
                 (t (:foreground ,dark-green-1 :background nil))))
   `(diff-changed ((t (:foreground ,dark-yellow))))
   `(diff-removed ((,class (:foreground ,dark-red :background nil))
                   (t (:foreground ,dark-red-3 :background nil))))
   `(diff-refine-added ((t (:inherit diff-added :weight bold))))
   `(diff-refine-change ((t (:inherit diff-changed :weight bold))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-header ((,class (:background ,dark-bg+2))
                  (t (:background ,dark-fg :foreground ,dark-bg))))
   `(diff-file-header
     ((,class (:background ,dark-bg+2 :foreground ,dark-fg :bold t))
      (t (:background ,dark-fg :foreground ,dark-bg :bold t))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,dark-blue-2 :background ,dark-bg-1))))
   `(diff-hl-delete ((,class (:foreground ,dark-red+1 :background ,dark-bg-1))))
   `(diff-hl-insert ((,class (:foreground ,dark-green+1 :background ,dark-bg-1))))
   `(diff-hl-unknown ((,class (:foreground ,dark-yellow :background ,dark-bg-1))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,dark-bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,dark-blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,dark-orange))))
   `(diredp-date-time ((t (:foreground ,dark-magenta))))
   `(diredp-deletion ((t (:foreground ,dark-yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,dark-red))))
   `(diredp-dir-heading ((t (:foreground ,dark-blue :background ,dark-bg))))
   `(diredp-dir-priv ((t (:foreground ,dark-cyan))))
   `(diredp-exec-priv ((t (:foreground ,dark-red))))
   `(diredp-executable-tag ((t (:foreground ,dark-green+1))))
   `(diredp-file-name ((t (:foreground ,dark-blue))))
   `(diredp-file-suffix ((t (:foreground ,dark-green))))
   `(diredp-flag-mark ((t (:foreground ,dark-yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,dark-orange))))
   `(diredp-ignored-file-name ((t (:foreground ,dark-red))))
   `(diredp-link-priv ((t (:foreground ,dark-yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,dark-yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,dark-orange))))
   `(diredp-no-priv ((t (:foreground ,dark-fg))))
   `(diredp-number ((t (:foreground ,dark-green+1))))
   `(diredp-other-priv ((t (:foreground ,dark-yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,dark-red-1))))
   `(diredp-read-priv ((t (:foreground ,dark-green-1))))
   `(diredp-symlink ((t (:foreground ,dark-yellow))))
   `(diredp-write-priv ((t (:foreground ,dark-magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:foreground ,dark-fg :background ,dark-red-4))))
   `(ediff-current-diff-Ancestor ((t (:foreground ,dark-fg :background ,dark-red-4))))
   `(ediff-current-diff-B ((t (:foreground ,dark-fg :background ,dark-green-1))))
   `(ediff-current-diff-C ((t (:foreground ,dark-fg :background ,dark-blue-5))))
   `(ediff-even-diff-A ((t (:background ,dark-bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,dark-bg+1))))
   `(ediff-even-diff-B ((t (:background ,dark-bg+1))))
   `(ediff-even-diff-C ((t (:background ,dark-bg+1))))
   `(ediff-fine-diff-A ((t (:foreground ,dark-fg :background ,dark-red-2 :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:foreground ,dark-fg :background ,dark-red-2 weight bold))))
   `(ediff-fine-diff-B ((t (:foreground ,dark-fg :background ,dark-green :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,dark-fg :background ,dark-blue-3 :weight bold ))))
   `(ediff-odd-diff-A ((t (:background ,dark-bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:background ,dark-bg+2))))
   `(ediff-odd-diff-B ((t (:background ,dark-bg+2))))
   `(ediff-odd-diff-C ((t (:background ,dark-bg+2))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,dark-fg))))
   `(egg-help-header-1 ((t (:foreground ,dark-yellow))))
   `(egg-help-header-2 ((t (:foreground ,dark-green+3))))
   `(egg-branch ((t (:foreground ,dark-yellow))))
   `(egg-branch-mono ((t (:foreground ,dark-yellow))))
   `(egg-term ((t (:foreground ,dark-yellow))))
   `(egg-diff-add ((t (:foreground ,dark-green+4))))
   `(egg-diff-del ((t (:foreground ,dark-red+1))))
   `(egg-diff-file-header ((t (:foreground ,dark-yellow-2))))
   `(egg-section-title ((t (:foreground ,dark-yellow))))
   `(egg-stash-mono ((t (:foreground ,dark-green+4))))
;;;;; elfeed
   `(elfeed-search-date-face ((t (:foreground ,dark-yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,dark-green))))
   `(elfeed-search-feed-face ((t (:foreground ,dark-cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,dark-yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,dark-yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,dark-red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,dark-yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,dark-green+2 :background ,dark-bg))))
   `(w3m-lnum-match ((t (:background ,dark-bg-1
                                     :foreground ,dark-orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,dark-yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,dark-blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,dark-fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,dark-yellow))))
   `(erc-keyword-face ((t (:foreground ,dark-blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,dark-yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,dark-red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default))))
   `(erc-notice-face ((t (:foreground ,dark-green))))
   `(erc-pal-face ((t (:foreground ,dark-orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,dark-orange :background ,dark-bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,dark-green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,dark-green+4 :background ,dark-bg))))
   `(ert-test-result-unexpected ((t (:foreground ,dark-red :background ,dark-bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,dark-yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,dark-red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,dark-blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,dark-red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,dark-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,dark-yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,dark-cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,dark-green :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-red-1) :inherit unspecified))
      (t (:foreground ,dark-red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-yellow) :inherit unspecified))
      (t (:foreground ,dark-yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-cyan) :inherit unspecified))
      (t (:foreground ,dark-cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,dark-red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,dark-yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,dark-cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,dark-red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,dark-orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,dark-green-1 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-orange) :inherit unspecified))
      (t (:foreground ,dark-orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-red) :inherit unspecified))
      (t (:foreground ,dark-red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,dark-fg))))
   `(ack-file ((t (:foreground ,dark-blue))))
   `(ack-line ((t (:foreground ,dark-yellow))))
   `(ack-match ((t (:foreground ,dark-orange :background ,dark-bg-1 :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,dark-green :weight bold :inverse-video t))))
   `(git-gutter:deleted ((t (:foreground ,dark-red :weight bold :inverse-video t))))
   `(git-gutter:modified ((t (:foreground ,dark-magenta :weight bold :inverse-video t))))
   `(git-gutter:unchanged ((t (:foreground ,dark-fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,dark-green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,dark-red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,dark-magenta :weight bold))))
;;;;; git-rebase-mode
   `(git-rebase-hash ((t (:foreground ,dark-orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:bold t :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:bold t :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:bold t :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:bold t :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:bold t :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:bold t :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:bold t :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:bold t :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:bold t :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:bold t :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:bold t :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:bold t :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:bold t :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:bold t :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-from))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-summary-cancelled ((t (:foreground ,dark-orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,dark-blue))))
   `(gnus-summary-high-read ((t (:foreground ,dark-green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,dark-orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,dark-fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,dark-blue))))
   `(gnus-summary-low-read ((t (:foreground ,dark-green))))
   `(gnus-summary-low-ticked ((t (:foreground ,dark-orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,dark-fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,dark-blue))))
   `(gnus-summary-normal-read ((t (:foreground ,dark-green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,dark-orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,dark-fg))))
   `(gnus-summary-selected ((t (:foreground ,dark-yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,dark-blue))))
   `(gnus-cite-10 ((t (:foreground ,dark-yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,dark-yellow))))
   `(gnus-cite-2 ((t (:foreground ,dark-blue-1))))
   `(gnus-cite-3 ((t (:foreground ,dark-blue-2))))
   `(gnus-cite-4 ((t (:foreground ,dark-green+2))))
   `(gnus-cite-5 ((t (:foreground ,dark-green+1))))
   `(gnus-cite-6 ((t (:foreground ,dark-green))))
   `(gnus-cite-7 ((t (:foreground ,dark-red))))
   `(gnus-cite-8 ((t (:foreground ,dark-red-1))))
   `(gnus-cite-9 ((t (:foreground ,dark-red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,dark-yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,dark-green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,dark-green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,dark-blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,dark-blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,dark-bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,dark-bg+2))))
   `(gnus-signature ((t (:foreground ,dark-yellow))))
   `(gnus-x ((t (:background ,dark-fg :foreground ,dark-bg))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,dark-blue))))
   `(guide-key/key-face ((t (:foreground ,dark-green))))
   `(guide-key/prefix-command-face ((t (:foreground ,dark-green+1))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,dark-green
                      :background ,dark-bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,dark-yellow
                      :background ,dark-bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,dark-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,dark-bg+1))))
   `(helm-visible-mark ((t (:foreground ,dark-bg :background ,dark-yellow-2))))
   `(helm-candidate-number ((t (:foreground ,dark-green+4 :background ,dark-bg-1))))
   `(helm-separator ((t (:foreground ,dark-red :background ,dark-bg))))
   `(helm-time-zone-current ((t (:foreground ,dark-green+2 :background ,dark-bg))))
   `(helm-time-zone-home ((t (:foreground ,dark-red :background ,dark-bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,dark-orange :background ,dark-bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,dark-magenta :background ,dark-bg))))
   `(helm-bookmark-info ((t (:foreground ,dark-green+2 :background ,dark-bg))))
   `(helm-bookmark-man ((t (:foreground ,dark-yellow :background ,dark-bg))))
   `(helm-bookmark-w3m ((t (:foreground ,dark-magenta :background ,dark-bg))))
   `(helm-buffer-not-saved ((t (:foreground ,dark-red :background ,dark-bg))))
   `(helm-buffer-process ((t (:foreground ,dark-cyan :background ,dark-bg))))
   `(helm-buffer-saved-out ((t (:foreground ,dark-fg :background ,dark-bg))))
   `(helm-buffer-size ((t (:foreground ,dark-fg-1 :background ,dark-bg))))
   `(helm-ff-directory ((t (:foreground ,dark-cyan :background ,dark-bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,dark-fg :background ,dark-bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,dark-green+2 :background ,dark-bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,dark-red :background ,dark-bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,dark-yellow :background ,dark-bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,dark-bg :background ,dark-yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,dark-cyan :background ,dark-bg))))
   `(helm-grep-file ((t (:foreground ,dark-fg :background ,dark-bg))))
   `(helm-grep-finish ((t (:foreground ,dark-green+2 :background ,dark-bg))))
   `(helm-grep-lineno ((t (:foreground ,dark-fg-1 :background ,dark-bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,dark-red :background ,dark-bg))))
   `(helm-moccur-buffer ((t (:foreground ,dark-cyan :background ,dark-bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,dark-fg-1 :background ,dark-bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,dark-fg :background ,dark-bg))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,dark-bg-1))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,dark-bg-1)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,dark-bg+1))
                   (t :weight bold)))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,dark-yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,dark-orange :weight bold))))
   `(ido-subdir ((t (:foreground ,dark-yellow))))
   `(ido-indicator ((t (:foreground ,dark-yellow :background ,dark-red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,dark-bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,dark-green+2))))
   `(jabber-roster-user-online ((t (:foreground ,dark-blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,dark-red+1))))
   `(jabber-rare-time-face ((t (:foreground ,dark-green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,dark-blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,dark-red+1))))
   `(jabber-activity-face((t (:foreground ,dark-red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,dark-blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,dark-orange))))
   `(js2-error ((t (:foreground ,dark-red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,dark-green-1))))
   `(js2-jsdoc-type ((t (:foreground ,dark-green+2))))
   `(js2-jsdoc-value ((t (:foreground ,dark-green+3))))
   `(js2-function-param ((t (:foreground ,dark-green+3))))
   `(js2-external-variable ((t (:foreground ,dark-orange))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,dark-red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,dark-fg :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,dark-bg+1))))
   `(ledger-font-pending-face ((t (:foreground ,dark-orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,dark-fg))))
   `(ledger-font-posting-account-face ((t (:foreground ,dark-blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,dark-fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,dark-orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,dark-orange))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,dark-orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,dark-fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,dark-bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,dark-green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,dark-red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,dark-fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,dark-orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,dark-orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,dark-fg-15 :background ,dark-bg-1))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,dark-green+2 :background ,dark-bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,dark-red+1 :background ,dark-bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,dark-blue+1 :background ,dark-bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,dark-magenta :background ,dark-bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,dark-yellow :background ,dark-bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
   `(magit-item-highlight ((t (:background ,dark-bg+05))))
   `(magit-section-title ((t (:foreground ,dark-yellow :weight bold))))
   `(magit-process-ok ((t (:foreground ,dark-green :weight bold))))
   `(magit-process-ng ((t (:foreground ,dark-red :weight bold))))
   `(magit-branch ((t (:foreground ,dark-blue :weight bold))))
   `(magit-log-author ((t (:foreground ,dark-orange))))
   `(magit-log-sha1 ((t (:foreground ,dark-orange))))
   `(magit-diff-added-highlight ((t (:foreground ,dark-green+1))))
   `(magit-diff-added ((t (:foreground ,dark-green+1))))
   `(magit-diff-removed-highlight ((t (:foreground ,dark-red-2))))
   `(magit-diff-removed ((t (:foreground ,dark-red-2))))
   `(magit-hash ((t (:foreground ,dark-fg-05))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,dark-green+1))))
   `(message-header-other ((t (:foreground ,dark-green))))
   `(message-header-to ((t (:foreground ,dark-yellow :weight bold))))
   `(message-header-from ((t (:foreground ,dark-yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,dark-yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,dark-yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,dark-orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,dark-green))))
   `(message-mml ((t (:foreground ,dark-yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,dark-orange))))
   `(mew-face-header-from ((t (:foreground ,dark-yellow))))
   `(mew-face-header-date ((t (:foreground ,dark-green))))
   `(mew-face-header-to ((t (:foreground ,dark-red))))
   `(mew-face-header-key ((t (:foreground ,dark-green))))
   `(mew-face-header-private ((t (:foreground ,dark-green))))
   `(mew-face-header-important ((t (:foreground ,dark-blue))))
   `(mew-face-header-marginal ((t (:foreground ,dark-fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,dark-red))))
   `(mew-face-header-xmew ((t (:foreground ,dark-green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,dark-red))))
   `(mew-face-body-url ((t (:foreground ,dark-orange))))
   `(mew-face-body-comment ((t (:foreground ,dark-fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,dark-green))))
   `(mew-face-body-cite2 ((t (:foreground ,dark-blue))))
   `(mew-face-body-cite3 ((t (:foreground ,dark-orange))))
   `(mew-face-body-cite4 ((t (:foreground ,dark-yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,dark-red))))
   `(mew-face-mark-review ((t (:foreground ,dark-blue))))
   `(mew-face-mark-escape ((t (:foreground ,dark-green))))
   `(mew-face-mark-delete ((t (:foreground ,dark-red))))
   `(mew-face-mark-unlink ((t (:foreground ,dark-yellow))))
   `(mew-face-mark-refile ((t (:foreground ,dark-green))))
   `(mew-face-mark-unread ((t (:foreground ,dark-red-2))))
   `(mew-face-eof-message ((t (:foreground ,dark-green))))
   `(mew-face-eof-part ((t (:foreground ,dark-yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,dark-cyan :background ,dark-bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,dark-bg :background ,dark-magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,dark-bg :background ,dark-red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,dark-blue))))
   `(mingus-pausing-face ((t (:foreground ,dark-magenta))))
   `(mingus-playing-face ((t (:foreground ,dark-cyan))))
   `(mingus-playlist-face ((t (:foreground ,dark-cyan ))))
   `(mingus-song-file-face ((t (:foreground ,dark-yellow))))
   `(mingus-stopped-face ((t (:foreground ,dark-red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,dark-yellow))))
   `(nav-face-button-num ((t (:foreground ,dark-cyan))))
   `(nav-face-dir ((t (:foreground ,dark-green))))
   `(nav-face-hdir ((t (:foreground ,dark-red))))
   `(nav-face-file ((t (:foreground ,dark-fg))))
   `(nav-face-hfile ((t (:foreground ,dark-red-4))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,dark-blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,dark-green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,dark-blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,dark-green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,dark-blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,dark-green-1 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,dark-blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,dark-bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,dark-bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,dark-bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,dark-bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,dark-bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,dark-bg+1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,dark-fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,dark-fg :weight bold))))
   `(org-checkbox ((t (:background ,dark-bg+2 :foreground ,dark-fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,dark-blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,dark-red-1))))
   `(org-done ((t (:bold t :weight bold :foreground ,dark-green+3))))
   `(org-formula ((t (:foreground ,dark-yellow-2))))
   `(org-headline-done ((t (:foreground ,dark-green+3))))
   `(org-hide ((t (:foreground ,dark-bg-1))))
   `(org-level-1 ((t (:foreground ,dark-orange))))
   `(org-level-2 ((t (:foreground ,dark-green+4))))
   `(org-level-3 ((t (:foreground ,dark-blue-1))))
   `(org-level-4 ((t (:foreground ,dark-yellow-2))))
   `(org-level-5 ((t (:foreground ,dark-cyan))))
   `(org-level-6 ((t (:foreground ,dark-green+2))))
   `(org-level-7 ((t (:foreground ,dark-red-4))))
   `(org-level-8 ((t (:foreground ,dark-blue-4))))
   `(org-link ((t (:foreground ,dark-yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,dark-green+4))))
   `(org-scheduled-previously ((t (:foreground ,dark-red))))
   `(org-scheduled-today ((t (:foreground ,dark-blue+1))))
   `(org-sexp-date ((t (:foreground ,dark-blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,dark-green+2))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,dark-orange))))
   `(org-todo ((t (:bold t :foreground ,dark-red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,dark-red :weight bold :underline nil))))
   `(org-column ((t (:background ,dark-bg-1))))
   `(org-column-title ((t (:background ,dark-bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,dark-fg :background ,dark-bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,dark-bg :background ,dark-red-1))))
   `(org-ellipsis ((t (:foreground ,dark-yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,dark-cyan :underline t))))
;;;;; outline
   `(outline-1 ((t (:foreground ,dark-orange))))
   `(outline-2 ((t (:foreground ,dark-green+4))))
   `(outline-3 ((t (:foreground ,dark-blue-1))))
   `(outline-4 ((t (:foreground ,dark-yellow-2))))
   `(outline-5 ((t (:foreground ,dark-cyan))))
   `(outline-6 ((t (:foreground ,dark-green+2))))
   `(outline-7 ((t (:foreground ,dark-red-4))))
   `(outline-8 ((t (:foreground ,dark-blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,dark-yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,dark-bg-1 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,dark-bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,dark-bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,dark-bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,dark-fg :background ,dark-bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,dark-bg :background ,dark-orange))))
   `(proof-error-face ((t (:foreground ,dark-fg :background ,dark-red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,dark-bg :background ,dark-yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,dark-bg :background ,dark-orange))))
   `(proof-locked-face ((t (:background ,dark-blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,dark-bg :background ,dark-orange))))
   `(proof-queue-face ((t (:background ,dark-red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,dark-red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,dark-bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,dark-bg))))
   `(proof-warning-face ((t (:foreground ,dark-bg :background ,dark-yellow-1))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,dark-fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,dark-green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,dark-yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,dark-cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,dark-green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,dark-blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,dark-yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,dark-green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,dark-blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,dark-orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,dark-green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,dark-blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,dark-blue))))
   `(rcirc-other-nick ((t (:foreground ,dark-orange))))
   `(rcirc-bright-nick ((t (:foreground ,dark-blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,dark-blue-2))))
   `(rcirc-server ((t (:foreground ,dark-green))))
   `(rcirc-server-prefix ((t (:foreground ,dark-green+1))))
   `(rcirc-timestamp ((t (:foreground ,dark-green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,dark-yellow))))
   `(rcirc-nick-in-message-full-line ((t (:bold t))))
   `(rcirc-prompt ((t (:foreground ,dark-yellow :bold t))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:bold t))))
   `(rcirc-url ((t (:bold t))))
   `(rcirc-keyword ((t (:foreground ,dark-yellow :bold t))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,dark-green))))
   `(rpm-spec-doc-face ((t (:foreground ,dark-green))))
   `(rpm-spec-ghost-face ((t (:foreground ,dark-red))))
   `(rpm-spec-macro-face ((t (:foreground ,dark-yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,dark-red))))
   `(rpm-spec-package-face ((t (:foreground ,dark-red))))
   `(rpm-spec-section-face ((t (:foreground ,dark-yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,dark-blue))))
   `(rpm-spec-var-face ((t (:foreground ,dark-red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,dark-orange))))
   `(rst-level-2-face ((t (:foreground ,dark-green+1))))
   `(rst-level-3-face ((t (:foreground ,dark-blue-1))))
   `(rst-level-4-face ((t (:foreground ,dark-yellow-2))))
   `(rst-level-5-face ((t (:foreground ,dark-cyan))))
   `(rst-level-6-face ((t (:foreground ,dark-green-1))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,dark-yellow :bold t))))
   `(sh-quoted-exec ((t (:foreground ,dark-red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,dark-red+1 :background ,dark-bg+3 :weight bold))))
   `(show-paren-match ((t (:background ,dark-bg+3 :weight bold))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,dark-red+1 :background ,dark-bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,dark-bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,dark-red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,dark-green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-red)))
      (t
       (:underline ,dark-red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-orange)))
      (t
       (:underline ,dark-orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-yellow)))
      (t
       (:underline ,dark-yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,dark-green)))
      (t
       (:underline ,dark-green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,dark-green+2))))
   `(speedbar-directory-face ((t (:foreground ,dark-cyan))))
   `(speedbar-file-face ((t (:foreground ,dark-fg))))
   `(speedbar-highlight-face ((t (:foreground ,dark-bg :background ,dark-green+2))))
   `(speedbar-selected-face ((t (:foreground ,dark-red))))
   `(speedbar-separator-face ((t (:foreground ,dark-bg :background ,dark-blue-1))))
   `(speedbar-tag-face ((t (:foreground ,dark-yellow))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,dark-fg
                                    :background ,dark-bg))))
   `(tabbar-selected ((t (:foreground ,dark-fg
                                      :background ,dark-bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,dark-fg
                                        :background ,dark-bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,dark-bg
                                       :background ,dark-bg-1))))
   `(term-color-red ((t (:foreground ,dark-red-2
                                       :background ,dark-red-4))))
   `(term-color-green ((t (:foreground ,dark-green
                                       :background ,dark-green+2))))
   `(term-color-yellow ((t (:foreground ,dark-orange
                                       :background ,dark-yellow))))
   `(term-color-blue ((t (:foreground ,dark-blue-1
                                      :background ,dark-blue-4))))
   `(term-color-magenta ((t (:foreground ,dark-magenta
                                         :background ,dark-red))))
   `(term-color-cyan ((t (:foreground ,dark-cyan
                                       :background ,dark-blue))))
   `(term-color-white ((t (:foreground ,dark-fg
                                       :background ,dark-fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,dark-fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,dark-red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,dark-fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,dark-yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,dark-cyan))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,dark-fg ))))
   `(web-mode-css-prop-face ((t (:foreground ,dark-fg))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,dark-fg))))
   `(web-mode-css-rule-face ((t (:foreground ,dark-fg))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-doc-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,dark-fg))))
   `(web-mode-html-attr-name-face ((t (:foreground ,dark-fg))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,dark-fg-15))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,dark-bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,dark-red))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,dark-bg+1 :foreground ,dark-bg+1))))
   `(whitespace-hspace ((t (:background ,dark-bg+1 :foreground ,dark-bg+1))))
   `(whitespace-tab ((t (:background ,dark-red-1))))
   `(whitespace-newline ((t (:foreground ,dark-bg+1))))
   `(whitespace-trailing ((t (:background ,dark-red))))
   `(whitespace-line ((t (:background ,dark-bg :foreground ,dark-magenta))))
   `(whitespace-space-before-tab ((t (:background ,dark-orange :foreground ,dark-orange))))
   `(whitespace-indentation ((t (:background ,dark-yellow :foreground ,dark-red))))
   `(whitespace-empty ((t (:background ,dark-yellow))))
   `(whitespace-space-after-tab ((t (:background ,dark-yellow :foreground ,dark-red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,dark-red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,dark-red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,dark-orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,dark-blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,dark-fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,dark-blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,dark-red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,dark-red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,dark-green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,dark-blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,dark-blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,dark-green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,dark-red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,dark-green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,dark-green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,dark-green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,dark-green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,dark-fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,dark-blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,dark-fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,dark-blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,dark-fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,dark-yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,dark-magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,dark-fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,dark-green+4))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,dark-bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,dark-bg-1 :foreground ,dark-bg-1))))
   ))

;;; Theme Variables
(dark-with-color-variables
  (custom-theme-set-variables
   'dark
;;;;; ansi-color
   `(ansi-color-names-vector [,dark-bg ,dark-red ,dark-green ,dark-yellow
                                          ,dark-blue ,dark-magenta ,dark-cyan ,dark-fg])
;;;;; fill-column-indicator
   `(fci-rule-color ,dark-bg-1)
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,dark-red-1)
       ( 40. . ,dark-red)
       ( 60. . ,dark-orange)
       ( 80. . ,dark-yellow-2)
       (100. . ,dark-yellow-1)
       (120. . ,dark-yellow)
       (140. . ,dark-green-1)
       (160. . ,dark-green)
       (180. . ,dark-green+1)
       (200. . ,dark-green+2)
       (220. . ,dark-green+3)
       (240. . ,dark-green+4)
       (260. . ,dark-cyan)
       (280. . ,dark-blue-2)
       (300. . ,dark-blue-1)
       (320. . ,dark-blue)
       (340. . ,dark-blue+1)
       (360. . ,dark-magenta)))
   `(vc-annotate-very-old-color ,dark-magenta)
   `(vc-annotate-background ,dark-bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar dark-add-font-lock-keywords nil
  "Whether to add font-lock keywords for dark color names.
In buffers visiting library `dark-theme.el' the dark
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar dark-colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after dark activate)
;;   "Maybe also add font-lock keywords for dark colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or dark-add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "dark-theme.el")))
;;     (unless dark-colors-font-lock-keywords
;;       (setq dark-colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car dark-colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc dark-colors-alist))))))
;;     (font-lock-add-keywords nil dark-colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after dark activate)
;;   "Also remove font-lock keywords for dark colors."
;;   (font-lock-remove-keywords nil dark-colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'dark)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; dark-theme.el ends here
