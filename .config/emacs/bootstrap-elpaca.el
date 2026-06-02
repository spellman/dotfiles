;;; bootstrap-elpaca.el --- Pinned Elpaca installer  -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the official Elpaca installer snippet (installer version 0.12),
;; isolated in its own file so that updating Elpaca is a whole-file replace
;; rather than surgery inside init.el.
;;
;; Two deliberate changes from the upstream snippet, to keep Elpaca pinned:
;;
;;   :ref   set to a specific commit instead of nil. With :ref nil the
;;          installer tracks the latest Elpaca, so the package manager moves
;;          underneath you. Pinning to a commit means Elpaca only changes when
;;          you decide to bump it.
;;
;;   :depth set to nil (a full clone) instead of 1 (a shallow clone). A shallow
;;          clone only contains the tip of each branch, so once upstream moves
;;          past the pinned commit a fresh shallow clone could not check it out.
;;          A full clone keeps the pinned commit reachable on any machine.
;;
;; To update Elpaca, deliberately:
;;   1. Replace this file with the new upstream installer snippet from
;;      https://github.com/progfolio/elpaca (it bumps elpaca-installer-version).
;;   2. Re-apply the two changes above, setting :ref to the matching new commit
;;      or tag for that installer version.
;;
;; To pin every OTHER package (not just Elpaca itself), run
;; `M-x elpaca-write-lockfile' and commit the result; Elpaca can then install
;; from that lockfile for a fully reproducible set of versions.

;;; Code:

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              ;; Pinned: a specific commit (matching installer
                              ;; version 0.12) and a full clone so the commit
                              ;; stays reachable. See the commentary above.
                              :ref "27c2889f66368bde12b4e243582e343ed9cb75e3"
                              :depth nil :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(provide 'bootstrap-elpaca)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

;;; bootstrap-elpaca.el ends here
