;;; anything-show-completion.el --- Show selection in buffer for anything completion
;; $Id: anything-show-completion.el,v 1.2 2009-04-18 09:21:14 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: anything, convenience, complete
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-show-completion.el

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
;; 

;;; Commands:
;;
;; Below are complete command list:
;;
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Installation:
;;
;; Put anything-show-completion.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-show-completion)
;;
;; No need more.

;;; Customize:
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET anything-show-completion RET
;;


;;; History:

;; $Log: anything-show-completion.el,v $
;; Revision 1.2  2009-04-18 09:21:14  rubikitch
;; `use-anything-show-completion' as a function.
;; It enables us to affect multiple commands with `dolist'.
;;
;; Revision 1.1  2009/04/17 17:07:35  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-show-completion-version "$Id: anything-show-completion.el,v 1.2 2009-04-18 09:21:14 rubikitch Exp $")
(require 'anything)
(defgroup anything-show-completion nil
  "anything-show-completion"
  :group 'anything)

(defvar asc-overlay nil)
(defvar anything-show-completion-face anything-selection-face)

(defun asc-initialize-maybe ()
  (unless asc-overlay
    (setq asc-overlay (make-overlay (point-min) (point-min)))
    (overlay-put asc-overlay 'face anything-show-completion-face)))
(asc-initialize-maybe)

(defun asc-cleanup ()
  (delete-overlay asc-overlay))
(add-hook 'anything-cleanup-hook 'asc-cleanup)

(defun use-anything-show-completion (f length-sexp)
  (eval `(defadvice ,f (before anything-show-completion activate)
           (asc-initialize-maybe)
           (move-overlay asc-overlay (point) (point) (current-buffer))
           (overlay-put asc-overlay 'length-sexp ',length-sexp))))

(defun asc-overlay-activate-p ()
  "Return non-nil if `anything' is being used for any completionic purposes."
  (overlay-buffer asc-overlay))

(defadvice anything-mark-current-line (after anything-show-completion activate)
  "Display the `anything-get-selection' contents as an overlay at the
current (point)."
  (anything-aif (and (asc-overlay-activate-p)
                     (with-anything-window
                       (not (equal (anything-buffer-get) anything-action-buffer)))
                     (anything-get-selection))
      (with-current-buffer anything-current-buffer
        (asc-display-overlay it))))

(defun asc-display-overlay (selection)
  (overlay-put asc-overlay 'display selection)
  (move-overlay asc-overlay
                (- (point) (eval (overlay-get asc-overlay 'length-sexp)))
                (point)
                anything-current-buffer))

(provide 'anything-show-completion)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-show-completion.el")
;;; anything-show-completion.el ends here
