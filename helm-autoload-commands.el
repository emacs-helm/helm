;;; helm-autoload-commands.el --- Search command and execute command in runtime.

;; Filename: helm-autoload-commands.el
;; Description: Search command and execute command in runtime.
;; Author:  Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer:  Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2014, , all rights reserved.
;; Created: 2014-05-05 08:03:10
;; Version: 0.1
;; Last-Updated: 2014-05-05 08:03:10
;;           By:
;; URL: http://www.emacswiki.org/emacs/download/helm-autoload-commands.el
;; Keywords: helm, autoload
;; Compatibility: GNU Emacs 24.3.50.1
;;
;; Features that might be required by this library:
;;
;; `helm'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Sometimes, we need execute command that not load default.
;; So we need `load-library' first, then type command name in M-x, we even forgot spell of command. :(
;;
;; `helm-autoload-commands' make we life eaiser, you just need write:
;;
;; (setq helm-autoload-commands-list
;;       '(
;;         ("command name" "command documentation" "library that load command")
;;         )
;;       )
;;
;; in your ~/.emacs then execute command `helm-autoload-commands',
;; this extension will show all commands completion, it will load library first and then execute command.
;; Everything is automatic in runtime, don't need execute require code in startup time.
;;
;; If you're helm guy, you can put `helm-autoload-commands' in your helm command.
;;
;; Enjoy! ;)
;;

;;; Installation:
;;
;; Put helm-autoload-commands.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'helm-autoload-commands)
;;
;; No need more.

;;; Customize:
;;
;;
;;

;;; Change log:
;;
;; 2014/05/05
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require

(require 'helm)

;;; Code:

(defvar helm-autoload-commands-source-buffer "*helm autoload source select*")

(defvar helm-autoload-commands-list nil)

(defvar helm-source-autoload-commands
  `((name . "Helm autoload commands")
    (candidate-number-limit . 9999)
    (candidates
     . (lambda nil
         (loop for symname in helm-autoload-commands-list
               for command = (intern (car symname))
               for doc = (cadr symname)
               for module = (caddr symname)
               collect
               (cons
                (concat
                 (propertize (format "%s" command)
                             'face 'font-lock-function-name-face)
                 (propertize (format " %s" doc)
                             'face 'font-lock-doc-face))
                (list command module)))
         ))
    (action . (("Execute autoload command" .
                (lambda (candidate)
                  (load-library (cadr candidate))
                  (call-interactively (car candidate))))
               ))
    (persistent-action . describe-command)))

;;;###autoload
(defun helm-autoload-commands nil
  "Select from autoload commands to execute."
  (interactive)
  (helm :sources 'helm-source-autoload-commands
        :buffer helm-autoload-commands-source-buffer))

(provide 'helm-autoload-commands)

;;; helm-autoload-commands.el ends here
