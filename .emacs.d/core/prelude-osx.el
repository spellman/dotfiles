;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(prelude-require-packages '(exec-path-from-shell vkill))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; It's all in the Meta
(setq ns-function-modifier 'hyper)

;; proced-mode doesn't work on OS X so we use vkill instead
(autoload 'vkill "vkill" nil t)
(global-set-key (kbd "C-x p") 'vkill)

;; (defun prelude-swap-meta-and-super ()
;;   "Swap the mapping of Meta and Super.
;; Very useful for people using their Mac with a
;; Windows external keyboard from time to time."
;;   (interactive)
;;   (if (eq mac-command-modifier 'super)
;;       (progn
;;         (setq mac-command-modifier 'meta)
;;         (setq mac-option-modifier 'super)
;;         (message "Command is now bound to META and Option is bound to SUPER."))
;;     (progn
;;       (setq mac-command-modifier 'super)
;;       (setq mac-option-modifier 'meta)
;;       (message "Command is now bound to SUPER and Option is bound to META."))))

(defun prelude-swap-win-mac-keyboards ()
  "Allow the left modifier key nearest the space bar to be control
   and allow the right modifier key nearest the space bar to be meta.
   (command keys on mac keyboard; alt keys on win keyboard)"
  (interactive)
  (if (eq ns-command-modifier 'control) ; if mac keyboard
      (progn ; swap to windows keyboard settings
        (setq ns-alternate-modifier 'control)
        (setq ns-right-alternate-modifier 'meta)
        (setq ns-command-modifier 'meta)
        (message "Left Alt -> CONTROL\nLeft Win -> META\nRight Alt -> META\nRight Win -> META"))
    (progn ; swap to mac keyboard settings
      (setq ns-command-modifier 'control)
      (setq ns-right-command-modifier 'meta)
      (setq ns-alternate-modifier 'meta)
      (message "Left Command -> CONTROL\nLeft Option -> META\nRight Command -> META\nRight Option -> META"))))

;; Default to mac modifier keys.
(setq ns-command-modifier 'control)
(setq ns-right-command-modifier 'meta)
(setq ns-alternate-modifier 'meta)

(define-key prelude-mode-map (kbd "C-c w") 'prelude-swap-win-mac-keyboards)
(define-key prelude-mode-map (kbd "s-/") 'hippie-expand)

(set-face-attribute 'default nil
                    :family "Monaco"
                    :height 130
                    :weight 'normal)

(provide 'prelude-osx)
