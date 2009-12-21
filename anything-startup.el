;;; anything-startup.el --- anything.el startup file

;;; $Id: anything-startup.el,v 1.9 2009-12-21 12:41:43 rubikitch Exp $

;;;; Installation

;;; anything.el is just a framework and predefined configuration is in
;;; anything-config.el. You need install both to use anything
;;; practically.
;;; 
;;; Note: anything-config.el loads anything.el.
(require 'anything-config)

;;; anything-match-plugin.el extends pattern matching. Some Anything
;;; Applications requires it. It is a must-have plugin now.
;;; 
(require 'anything-match-plugin)

;;; If you use Japanese, you should install Migemo and anything-migemo.el.
;;;
;;; Migemo  http://0xcc.net/migemo/
(and (equal current-language-environment "Japanese")
     (require 'anything-migemo nil t))

;;; anything-complete.el replaces various completion with anything
;;; (like Icicles). Use Anything power for normal completion.
(require 'anything-complete nil t)
;; Automatically collect symbols by 150 secs
(anything-lisp-complete-symbol-set-timer 150)
;; Comment if you do not want to replace completion commands with `anything'.
(anything-read-string-mode 1)

;;; anything-show-completion.el shows current selection prettily.
(require 'anything-show-completion)

;;; anything-auto-install.el integrates auto-install.el with anything.
(require 'anything-auto-install nil t)

;;; descbinds-anything.el replaces describe-bindings with anything interface.
(require 'descbinds-anything nil t)
;; Comment if you do not want to replace `describe-bindings' with `anything'.
(descbinds-anything-install)

;;; `anything-grep' replaces standard `grep' command.
(require 'anything-grep nil t)

(provide 'anything-startup)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-startup.el")
;;; anything-startup.el ends here
