;;; anything-startup.el --- anything.el startup file

;;; Installation

;; anything.el and add-ons have complex dependencies. So you should
;; install install-elisp.el to semi-automate Emacs Lisp installation.
;; Install install-elisp.el from
;; http://www.emacswiki.org/cgi-bin/wiki/download/install-elisp.el
;; Then byte-compile and load it.
(require 'install-elisp nil t)
;; (setq install-elisp-repository-directory "~/.emacs.d/")

;; anything.el is just a framework and predefined configuration is in
;; anything-config.el. You need install both to use anything
;; practically.

;; [EVAL IT] (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/anything.el")
;; [EVAL IT] (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/anything-config.el")

;; Note: anything-config.el loads anything.el.
(require 'anything-config)
(setq anything-idle-delay 0.5)
(setq anything-input-idle-delay 0.2)
(setq anything-sources
      '(anything-c-source-buffers
        anything-c-source-file-name-history
        anything-c-source-locate))
