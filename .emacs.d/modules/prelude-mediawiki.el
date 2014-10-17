(prelude-require-packages '(mediawiki))

(eval-after-load 'mediawiki
  '(progn
     (setq mediawiki-site-alist '(("Wikipedia" "http://en.wikipedia.org/w" "" "" "Main Page")
                                  ("WikEmacs" "http://wikemacs.org/w/" "" "" "Main Page")))

     ;; Emacs users care more for WikEmacs than Wikipedia :-)
     (setq mediawiki-site-default "WikEmacs")))

(provide 'prelude-mediawiki)
