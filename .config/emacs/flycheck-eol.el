;;; flycheck-eol.el --- End-of-line flycheck diagnostics -*- lexical-binding: t; -*-

;; After flycheck finishes a syntax check, place an overlay at the end of every
;; line that has an error or warning, showing the diagnostic text inline (like
;; VS Code's Error Lens).
;;
;; Toggle per buffer with `M-x cws/flycheck-eol-mode'.

(require 'seq)

(defvar-local cws/flycheck-eol--overlays nil)

(defun cws/flycheck-eol--clear ()
  "Remove all end-of-line diagnostic overlays."
  (mapc #'delete-overlay cws/flycheck-eol--overlays)
  (setq cws/flycheck-eol--overlays nil))

(defun cws/flycheck-eol--face (level)
  "Return the face for a flycheck error LEVEL."
  (pcase level
    ('error 'error)
    ('warning 'warning)
    (_ 'shadow)))

(defconst cws/flycheck-eol--gap 3
  "Minimum number of columns between the code and the diagnostic block.")

(defun cws/flycheck-eol--text-width ()
  "Columns available for buffer text in the selected window.
`window-body-width' counts the line-number margin, but `current-column'
and `:align-to' are measured from the text area that follows it, so the
margin must be subtracted to get a usable right edge.  One extra column
is reserved so a full-width line does not trigger a continuation wrap."
  (- (window-body-width)
     (if (bound-and-true-p display-line-numbers)
         (ceiling (line-number-display-width 'columns))
       0)
     1))

(defun cws/flycheck-eol--wrap (text width)
  "Split TEXT into a list of lines, each at most WIDTH columns, on word boundaries.
A single word longer than WIDTH still occupies its own (over-long) line."
  (let ((words (split-string text " " t))
        (lines '())
        (current ""))
    (dolist (word words)
      (let ((candidate (if (string-empty-p current)
                           word
                         (concat current " " word))))
        (if (and (not (string-empty-p current))
                 (> (length candidate) width))
            (progn
              (push current lines)
              (setq current word))
          (setq current candidate))))
    (unless (string-empty-p current)
      (push current lines))
    (nreverse lines)))

(defun cws/flycheck-eol--format (text face)
  "Build a right-aligned block string for TEXT with FACE.
The block's right edge sits at the window's right edge; lines within the
block are left-aligned, so a short message is flush right and a wrapping
message has its continuation lines aligned under the first.
Point must be on the target line when called."
  (let* ((right (cws/flycheck-eol--text-width))
         (code-end (save-excursion
                     (goto-char (line-end-position))
                     (current-column)))
         (avail (max 20 (- right code-end cws/flycheck-eol--gap)))
         (lines (cws/flycheck-eol--wrap text avail))
         (block-width (apply #'max 0 (mapcar #'length lines)))
         (block-left (max (+ code-end cws/flycheck-eol--gap)
                          (- right block-width)))
         (block (mapconcat
                 (lambda (line)
                   (concat (propertize " " 'display `(space :align-to ,block-left))
                           (propertize line 'face face)))
                 lines
                 "\n")))
    ;; Keep point at the end of the code rather than after the annotation.
    ;; By default the display engine draws the cursor past an end-of-line
    ;; `after-string', putting it out at the far right.  A leading literal
    ;; space carrying a non-nil `cursor' property pins the cursor to that
    ;; space -- right after the code -- the way CIDER/eros place inline
    ;; results.  The space sits in the gap before `block-left', so it does
    ;; not shift the right-aligned block.
    (concat (propertize " " 'cursor t) block)))

(defvar-local cws/flycheck-eol--last-width nil
  "Text-area width used at the last re-flow, to detect genuine width changes.")

(defun cws/flycheck-eol--display ()
  "Place end-of-line overlays for every flycheck diagnostic."
  (cws/flycheck-eol--clear)
  (setq cws/flycheck-eol--last-width (cws/flycheck-eol--text-width))
  (let ((by-line (make-hash-table :test #'eql)))
    (dolist (err flycheck-current-errors)
      (let ((line (flycheck-error-line err)))
        (when line
          (push err (gethash line by-line)))))
    (maphash
     (lambda (line errs)
       (save-excursion
         (goto-char (point-min))
         (forward-line (1- line))
         (let* ((errs (nreverse errs))
                (worst (seq-reduce
                        (lambda (acc e)
                          (let ((lvl (flycheck-error-level e)))
                            (cond ((eq lvl 'error) 'error)
                                  ((and (eq lvl 'warning) (not (eq acc 'error))) 'warning)
                                  (t acc))))
                        errs 'info))
                (text (mapconcat #'flycheck-error-message errs "  |  "))
                (pos (line-end-position))
                (ov (make-overlay pos pos)))
           ;; A zero-width overlay with `after-string' (not `before-string')
           ;; keeps point at the end of the code: an after-string renders past
           ;; point, whereas a before-string would render between the code and
           ;; the cursor.
           (overlay-put ov 'after-string
                        (cws/flycheck-eol--format text (cws/flycheck-eol--face worst)))
           (push ov cws/flycheck-eol--overlays))))
     by-line)
    (cws/flycheck-eol--sync-hl-line)))

(defun cws/flycheck-eol--maybe-reflow ()
  "Rebuild diagnostics for the current buffer if its window width changed.
A buffer-local function for `window-configuration-change-hook', which runs
with the buffer's window temporarily selected.  The overlay text is wrapped
to the window width, so it must be rebuilt when that width changes; it is
left alone otherwise (and while a completion popup is active) so it never
disturbs an in-progress Corfu completion."
  (when (and cws/flycheck-eol-mode
             (not (bound-and-true-p completion-in-region-mode))
             (not (eql (cws/flycheck-eol--text-width)
                       cws/flycheck-eol--last-width)))
    (cws/flycheck-eol--display)))

(defun cws/flycheck-eol--sync-hl-line ()
  "Stamp the hl-line background onto the after-string of the current-line overlay."
  (when (bound-and-true-p hl-line-mode)
    (let ((bol (line-beginning-position))
          (eol (line-end-position))
          (hl-bg (face-background 'hl-line nil t)))
      (when hl-bg
        (dolist (ov cws/flycheck-eol--overlays)
          (when (overlay-buffer ov)
            (let* ((start (overlay-start ov))
                   (on-current (and (>= start bol) (<= start eol)))
                   (orig (overlay-get ov 'cws/flycheck-eol-orig-str))
                   (str (overlay-get ov 'after-string)))
              (cond
               ((and on-current (not orig))
                (overlay-put ov 'cws/flycheck-eol-orig-str str)
                (let ((copy (copy-sequence str)))
                  (add-face-text-property 0 (length copy)
                                          (list :background hl-bg) nil copy)
                  (overlay-put ov 'after-string copy)))
               ((and (not on-current) orig)
                (overlay-put ov 'after-string orig)
                (overlay-put ov 'cws/flycheck-eol-orig-str nil))))))))))

(define-minor-mode cws/flycheck-eol-mode
  "Show flycheck diagnostics at the end of each affected line."
  :lighter nil
  (if cws/flycheck-eol-mode
      (progn
        (add-hook 'flycheck-after-syntax-check-hook #'cws/flycheck-eol--display nil t)
        (add-hook 'flycheck-before-syntax-check-hook #'cws/flycheck-eol--clear nil t)
        ;; Buffer-local so it only ever runs for this buffer's windows -- it can
        ;; never fire for another package's child frame (e.g. Corfu's popup).
        ;; Fires on frame resize and window splits, re-wrapping to the new width.
        (add-hook 'window-configuration-change-hook #'cws/flycheck-eol--maybe-reflow nil t)
        (add-hook 'post-command-hook #'cws/flycheck-eol--sync-hl-line nil t)
        (cws/flycheck-eol--display))
    (remove-hook 'flycheck-after-syntax-check-hook #'cws/flycheck-eol--display t)
    (remove-hook 'flycheck-before-syntax-check-hook #'cws/flycheck-eol--clear t)
    (remove-hook 'window-configuration-change-hook #'cws/flycheck-eol--maybe-reflow t)
    (remove-hook 'post-command-hook #'cws/flycheck-eol--sync-hl-line t)
    (cws/flycheck-eol--clear)))

(provide 'flycheck-eol)
;;; flycheck-eol.el ends here
