;;; anything-startup.el --- anything.el startup file

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
(and (equal current-language-environment "Japanese")
     (require 'anything-migemo nil t))

;;; anything-complete.el replaces various completion with anything
;;; (like Icicles). Use Anything power for normal completion.
(when (require 'anything-complete nil t)
  ;; Automatically collect symbols by 150 secs
  (anything-lisp-complete-symbol-set-timer 150)
  (define-key emacs-lisp-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)
  (define-key lisp-interaction-mode-map "\C-\M-i" 'anything-lisp-complete-symbol-partial-match)
  ;; Comment if you do not want to replace completion commands with `anything'.
  (anything-read-string-mode 1)
  )

;;; anything-show-completion.el shows current selection prettily.
(require 'anything-show-completion)

;;; anything-auto-install.el integrates auto-install.el with anything.
(require 'anything-auto-install nil t)

;;; descbinds-anything.el replaces describe-bindings with anything interface.
(when (require 'descbinds-anything nil t)
  ;; Comment if you do not want to replace `describe-bindings' with `anything'.
  (descbinds-anything-install)
  )

;;; `anything-grep' replaces standard `grep' command.
(require 'anything-grep nil t)

(provide 'anything-startup)

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything-startup.el"))
;;; anything-startup.el ends here
