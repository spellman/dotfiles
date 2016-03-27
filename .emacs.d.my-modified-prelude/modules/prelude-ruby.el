(require 'prelude-programming)

(prelude-require-packages '(ruby-tools inf-ruby yari rbenv))
(require 'rbenv)

;; Rake files are ruby, too, as are gemspecs, rackup files, and gemfiles.
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Puppetfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Appraisals\\'" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(define-key 'help-command (kbd "R") 'yari)

(eval-after-load 'ruby-mode
  '(progn
     (defun prelude-ruby-mode-defaults ()
       (inf-ruby-minor-mode +1)
       (ruby-tools-mode +1)
       (modify-syntax-entry ?: "w") ; consider : to be part of :word
       (modify-syntax-entry ?! "w") ; consider ! to be part of word!
       (modify-syntax-entry ?? "w") ; consider ? to be part of word?
       (subword-mode +1) ; CamelCase aware editing operations
       (rbenv-use-corresponding))

     (setq prelude-ruby-mode-hook 'prelude-ruby-mode-defaults)

     (add-hook 'ruby-mode-hook (lambda ()
                                 (run-hooks 'prelude-ruby-mode-hook)))))

(provide 'prelude-ruby)
