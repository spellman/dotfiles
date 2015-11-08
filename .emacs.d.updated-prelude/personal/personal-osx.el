(menu-bar-mode -1)

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
