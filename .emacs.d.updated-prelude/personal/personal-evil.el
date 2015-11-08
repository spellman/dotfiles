;;(add-hook
;;  'prelude-evil-mode-hook
;;  (lambda ()
;;    (prelude-require-packages
;;      '(evil-indent-textobject
;;        evil-jumper
;;        evil-leader
;;        evil-matchit
;;        evil-nerd-commenter
;;        ))
;;    (setq evil-default-cursor '("gray" box)
;;          evil-emacs-state-cursor  '("red" box)
;;          evil-normal-state-cursor '("gray" box)
;;          evil-visual-state-cursor '("gray" box)
;;          evil-insert-state-cursor '("gray" box)
;;          evil-motion-state-cursor '("gray" box)
;;          evil-replace-state-cursor '("gray" box)
;;          evil-operator-state-cursor '("gray" box)
;;          )
;;    ;; C-u scrolls half a page up. Note that the universal argument (normally C-u)
;;    ;; will need to be remapped in custom-keybindings.el.
;;    (setq evil-want-C-u-scroll t)
;;
;;    ;; Load Evil and plugins
;;    (require 'evil)
;;    (require 'evil-leader)
;;    (require 'evil-indent-textobject)
;;    (require 'evil-jumper)
;;    (require 'evil-matchit)
;;    (require 'evil-nerd-commenter)
;;    (require 'evil-visualstar) ; installed in modules/prelude-evil.el
;;
;;    ;; Enable evil in the following modes.
;;    ;;   Copied from Bling .emacs.d/config/init-evil.el (with group changed to
;;    ;;   prelude).
;;    (defcustom dotemacs-evil-state-modes
;;      '(fundamental-mode
;;        text-mode
;;        prog-mode
;;        sws-mode
;;        dired-mode
;;        comint-mode
;;        log-edit-mode
;;        compilation-mode
;;        ag)
;;      "List of modes that should start up in Evil state."
;;      :type '(repeat (symbol))
;;      :group 'prelude)
;;
;;    (defun my-enable-evil-mode ()
;;      (if (apply 'derived-mode-p dotemacs-evil-state-modes)
;;          (turn-on-evil-mode)
;;        (set-cursor-color "red")))
;;    (add-hook 'after-change-major-mode-hook 'my-enable-evil-mode)
;;
;;    ;; You should enable global-evil-leader-mode before you enable evil-mode,
;;    ;; otherwise evil-leader won’t be enabled in initial buffers (*scratch*,
;;    ;; *Messages*, …).
;;    ;; (https://github.com/cofi/evil-leader)
;;    (global-evil-leader-mode t)
;;    (evil-mode 1)
;;
;;    ;; Easy navigation of wrapped lines (I.e., nnoremap j gj)
;;    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;;    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;;    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
;;    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
;;
;;    ;; Initial states
;;    (evil-set-initial-state 'cider-nrepl-mode 'insert)
;;
;;    ;; Place magit-blame-mode keymap above evil
;;    ;; see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
;;    (eval-after-load 'magit
;;      '(progn
;;         (evil-make-overriding-map magit-blame-mode-map 'normal)
;;         ;; force update evil keymaps after magit-blame-mode loaded
;;         (add-hook 'magit-blame-mode-hook #'evil-normalize-keymaps)))
;;
;;    ;; Alternative method
;;    ;;(defadvice magit-blame-mode (after magit-blame-change-to-emacs-state activate compile)
;;    ;;           "when entering magit-blame mode, change evil normal state to emacs state"
;;    ;;           (if (evil-normal-state-p)
;;    ;;             (evil-emacs-state)
;;    ;;             (evil-normal-state)))
;;    ;;(ad-activate 'magit-blame-mode)
;;    ))
