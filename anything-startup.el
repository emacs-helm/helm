;;; anything-startup.el --- anything.el startup file

;;;; Installation

;;; anything.el is just a framework and predefined configuration is in
;;; anything-config.el. You need install both to use anything
;;; practically.
;;; 
;;; Note: anything-config.el loads anything.el.
;;; 
;;; [EVAL IT] (install-elisp-from-emacswiki "anything.el")
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-config.el")
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-match-plugin.el")
;;; 
;;; anything-match-plugin.el extends pattern matching. Some Anything
;;; Applications requires it. It is a must-have plugin now.
;;; 
(require 'anything-match-plugin)
(require 'anything-config)

;;; If you use Japanese, you should install Migemo and anything-migemo.el.
;;;
;;; Migemo  http://0xcc.net/migemo/
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-migemo.el")
(and (equal current-language-environment "Japanese")
     (require 'anything-migemo nil t))

;;; anything-complete.el replaces various completion with anything
;;; (like Icicles). Use Anything power for normal completion.
(require 'anything-complete nil t)

;;; anything-auto-install.el integrates auto-install.el with anything.
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-auto-install")
(require 'anything-auto-install nil t)

;;; anything-yaoddmuse.el integrates yaoddmuse.el with anything.
;;; [EVAL IT] (install-elisp-from-emacswiki "yaoddmuse.el")
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-yaoddmuse.el")
(require 'anything-yaoddmuse nil t)

;;; anything-emms.el integrates EMMS with anything.
;;; EMMS  http://www.gnu.org/software/emms/
;;; [EVAL IT] (install-elisp-from-emacswiki "emms-extension.el")
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-emms.el")
(require 'anything-emms nil t)

;;; anything-traverse.el integrates traverselisp.el with anything (like grep).
;;; [EVAL IT] (install-elisp-from-emacswiki "traverselisp.el")
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-traverse.el")
(require 'anything-traverse nil t)

;;; anything-mercurial.el manages your mercurial patchs from anything (hg qpatch).
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-mercurial.el")
(require 'anything-mercurial nil t)

;;; anything-delicious.el manages your delicious bookmarks from anything.
;;; (add, delete, update, browse, tag completion)
;;;
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-delicious.el")
(require 'anything-delicious nil t)

;;; php-completion.el complets everything PHP with anything.
;;; [EVAL IT] (install-elisp-from-emacswiki "php-completion.el")
(require 'php-completion nil t)

;;; anything-etags.el searches ExuberantCtags / EmacsTags tag.
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-etags.el")
(require 'anything-etags nil t)

;;; anything-gtags.el replaces gtags-select-mode with anything.
;;; You have to install gtags.el.
;;;
;;; GNU GLOBAL  http://www.gnu.org/software/global/
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-gtags.el")
(and (require 'gtags nil t)
     (require 'anything-gtags nil t))

;;; anything-include.el maintains history of #include and reusable (C, C++).
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-include.el")
(require 'anything-include nil t)

;;; descbinds-anything.el replaces describe-bindings with anything interface.
;;; [EVAL IT] (install-elisp-from-emacswiki "descbinds-anything.el")
(require 'descbinds-anything nil t)

;;; anything-dabbrev-expand.el integrates dabbrev with anything.
;;; [EVAL IT] (install-elisp-from-emacswiki "anything-dabbrev-expand.el")
(require 'anything-dabbrev-expand nil t)


;; anything-c-shell-history.el
;; anything-extension.el
;; anything-grep.el
;; anything-ipa.el
;; anything-irfc.el
;; anything-kyr-config.el
;; anything-kyr.el
