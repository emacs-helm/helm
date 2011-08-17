;;; anything-startup.el --- Minimal configuration for anything.

;;; $Id: anything-startup.el,v 1.10 2010-02-04 19:57:31 rubikitch Exp $

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
;;; http://www.emacswiki.org/emacs/anything-migemo.el
(and (equal current-language-environment "Japanese")
     (require 'anything-migemo nil t))

;;; Completion for lisp symbols and apropos.
(when (require 'anything-complete nil t)
  ;; Automatically collect symbols by 150 secs
  (anything-lisp-complete-symbol-set-timer 150))


(provide 'anything-startup)

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything-startup.el"))
;;; anything-startup.el ends here
