;;;; anything-menu.el --- anything.el candidate selection outside Emacs 
;; $Id: anything-menu.el,v 1.6 2010-04-01 12:10:35 rubikitch Exp $

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: menu, tools, convenience, anything
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-menu.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This file provides anything.el candidate selection outside
;; Emacs. You have to enable emacsserver or gnuserv by M-x
;; server-start or M-x gnuserv-start.
;;
;; [EVAL IT] (describe-function 'anything-menu)
;; [EVAL IT] (describe-function 'anything-menu-select)
;; [EVAL IT] (describe-function 'anything-menu-select-from-file)
;;
;; First you have to install anything-menu script, which takes one argument, candidate file.
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-menu
;;
;; To demonstrate anything-menu, execute the following from shell
;; $ anything-menu ~/.emacs
;;

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-menu'
;;    Call `anything' outside Emacs.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put anything-menu.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-menu)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-menu RET
;;


;;; History:

;; $Log: anything-menu.el,v $
;; Revision 1.6  2010-04-01 12:10:35  rubikitch
;; * document
;; * `anything-menu': ANY-KEYMAP argument
;;
;; Revision 1.5  2010/02/23 20:39:41  rubikitch
;; add `make-frame-visible'
;;
;; Revision 1.4  2010/02/23 16:48:41  rubikitch
;; migemized
;;
;; Revision 1.3  2010/02/23 10:23:52  rubikitch
;; New function `anything-menu-select-from-file'
;;
;; Revision 1.2  2010/02/23 10:10:34  rubikitch
;; implemented
;;
;; Revision 1.1  2010/02/23 09:44:09  rubikitch
;; initial
;;

;;; Code:

(defvar anything-menu-version "$Id: anything-menu.el,v 1.6 2010-04-01 12:10:35 rubikitch Exp $")
(require 'anything)
(defgroup anything-menu nil
  "anything-menu"
  :group 'emacs)

(defvar am/tmp-file "/tmp/.am-tmp-file")
(defvar am/frame nil)
(defun am/set-frame ()
  (unless (and am/frame (frame-live-p am/frame))
    (setq am/frame (make-frame '((name . "anything menu")
                                 (title . "anything menu")))))
  (select-frame am/frame)
  (make-frame-visible am/frame)
  (sit-for 0))

(defun am/close-frame ()
  (ignore-errors (make-frame-invisible am/frame))
  (when (fboundp 'do-applescript)
    (funcall 'do-applescript "tell application \"iTerm\"
                                activate
                             end")))
(defun am/write-result (line)
  (write-region (or line "") nil am/tmp-file nil 'silent))

(defun anything-menu (&optional any-sources any-input any-prompt any-resume any-preselect any-buffer any-keymap)
  "Call `anything' outside Emacs.
Arguments are the same as `anything'.
Pop up anything frame and close it after session."
  (interactive)
  (am/set-frame)
  (unwind-protect
      (let ((anything-samewindow t)
            (anything-display-function 'anything-default-display-buffer)
            (anything-after-action-hook (lambda () (am/write-result (anything-get-selection)))))
        (anything any-sources any-input any-prompt any-resume any-preselect any-buffer any-keymap))
    (am/close-frame)))

(defun anything-menu-select (am-prompt &rest am-selections)
  "Select from a list AM-SELECTIONS and write selection to /tmp/.am-tmp-file,
the default file of `am/tmp-file'. "
  (anything-menu `(((name . ,am-prompt)
                    (candidates . am-selections)
                    (migemo)
                    (action . am/write-result)))
                 nil (concat am-prompt ": ") nil nil "*anything menu select*"))

(defun* anything-menu-select-from-file (am-filename &optional (am-prompt "selection"))
  "Select a candidate in file AM-FILENAME and write selection to /tmp/.am-tmp-file,
the default file of `am/tmp-file'.

The anything-menu script calls this function and print selection to stdout."
  (anything-menu `(((name . ,am-prompt)
                    (init . (lambda ()
                              (with-current-buffer (anything-candidate-buffer 'global)
                                (insert-file-contents am-filename))))
                    (candidates-in-buffer)
                    (migemo)
                    (action . am/write-result)))
                 nil (concat am-prompt ": ") nil nil "*anything menu select*"))

(provide 'anything-menu)

;; (save-window-excursion (bg2 "gnudoit '(anything-menu-select \"selections\" \"a\" \"b\")'"))
;; (find-sh0 "cat /tmp/.am-tmp-file")

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything-menu.el"))
;;; anything-menu.el ends here
