;; Tramp. Tramp pings host.does.not.exist in startup, which should
;; immediately return unknown host and startup should continue. The ISP seems
;; to be catching unresolved host names and it's messing up Tramp's strategy
;; and causing it to hang until something cuts it off after about 2 minutes.
;; This makes for a very slow emacs startup time.
;;
;; SPACEMACS FIX:
;; Setting tramp-ssh-controlmaster-options as below is an attempt to stop
;; this behavior, as per https://github.com/emacs-helm/helm/issues/1000
;;tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"
;;
;; PRELUDE FIX:
(setq tramp-ssh-controlmaster-options)
(require 'tramp)
