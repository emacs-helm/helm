;;; anything-ipa.el --- Anything interface of In Place Annotation
;; $Id: anything-ipa.el,v 1.1 2009-02-13 00:20:05 rubikitch Exp $

;; Copyright (C) 2009  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience, anything
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-ipa.el

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

;; Anything interface of in place annotations.  Variable
;; `anything-c-source-ipa' is source for in place annotations in
;; current buffer. And command `anything-ipa' is anything menu of it.

;;; Installation:

;; Get ipa.el and anything.el from EmacsWiki
;; http://www.emacswiki.org/cgi-bin/wiki/download/ipa.el
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything.el
;;
;; Then add the code below in your ~/.emacs.
;; (require 'anything-ipa)
;;

;;; History:

;; $Log: anything-ipa.el,v $
;; Revision 1.1  2009-02-13 00:20:05  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-ipa-version "$Id: anything-ipa.el,v 1.1 2009-02-13 00:20:05 rubikitch Exp $")
(eval-when-compile (require 'cl))
(require 'anything)
(require 'ipa)

(defvar anything-c-source-ipa
  '((name . "In Place Annotations (Current Buffer)")
    (candidates . anything-ipa-candidates)
    (action . goto-char)
    (multiline))
  "`anything' source of ipa in current-buffer.")

(defun anything-ipa-candidates ()
  (save-excursion
    (set-buffer anything-current-buffer)
    (loop for (overlay . text) in ipa-annotations-in-buffer 
          for pos = (overlay-start overlay)
          for line = (progn (goto-char pos)
                            (buffer-substring (point-at-bol) (point-at-eol)))
          for lineno = (line-number-at-pos pos)
          collect
          (cons (format "%d:%s\n %s" lineno text line)
                pos))))

(defun anything-ipa ()
  "`anything' interface of ipa."
  (interactive)
  (anything 'anything-c-source-ipa))

(provide 'anything-ipa)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-ipa.el")
;;; anything-ipa.el ends here
