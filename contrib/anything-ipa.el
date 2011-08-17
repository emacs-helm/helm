;;; anything-ipa.el --- Anything interface of In Place Annotation
;; $Id: anything-ipa.el,v 1.6 2009-03-01 22:52:44 rubikitch Exp $

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

;; Anything interface of in place annotations.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-ipa'
;;    `anything' interface of ipa.
;;  `anything-ipa-global'
;;    `anything' interface of ipa (global).
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; Variable `anything-c-source-ipa' is source for in place annotations
;; in current buffer. And command `anything-ipa' is anything menu of
;; it. `anything-c-source-ipa-global' and `anything-ipa-global' are
;; global ones.


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
;; Revision 1.6  2009-03-01 22:52:44  rubikitch
;; Use `ipa-annotation-face' for annotation text
;;
;; Revision 1.5  2009/02/13 01:18:32  rubikitch
;; migemize
;;
;; Revision 1.4  2009/02/13 00:49:36  rubikitch
;; *** empty log message ***
;;
;; Revision 1.3  2009/02/13 00:48:08  rubikitch
;; `anything-c-source-ipa': format change
;;
;; Revision 1.2  2009/02/13 00:46:16  rubikitch
;; New variable: `anything-c-source-ipa-global'
;; New command: `anything-ipa-global'
;;
;; Revision 1.1  2009/02/13 00:20:05  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-ipa-version "$Id: anything-ipa.el,v 1.6 2009-03-01 22:52:44 rubikitch Exp $")
(eval-when-compile (require 'cl))
(require 'anything)
(require 'ipa nil t)

;;;; file-local source
(defvar anything-c-source-ipa
  '((name . "In Place Annotations (Current Buffer)")
    (candidates . anything-ipa-candidates)
    (action ("Go To" . goto-char)
            ("Edit (empty string to delete)" .
             (lambda (p) (save-excursion (goto-char p) (ipa-edit t))))
            ("Move" .
             (lambda (p) (goto-char p) (ipa-move t))))
    (migemo))
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
          (cons (format "%5d:[%s]%s"
                        lineno (propertize text 'face ipa-annotation-face) line)
                pos))))

(defun anything-ipa ()
  "`anything' interface of ipa."
  (interactive)
  (anything 'anything-c-source-ipa))

;;;; global source
(defvar anything-c-source-ipa-global
  '((name . "In Place Annotations (global)")
    (init . (lambda () (anything-candidate-buffer (ipa-find-storage-file))))
    (get-line . (lambda (s e) (unless (= s e) (cons (buffer-substring s e) s))))
    (candidates-in-buffer)
    (migemo)
    (action ("Go To" . anything-ipa-go-to-annotation)))
  "`anything' source of all IPAs.")

(defun anything-ipa-go-to-annotation (pos)
  (with-current-buffer (ipa-find-storage-file)
    (goto-char pos)
    (ipa-go-to-annotation)))

(defun anything-ipa-global ()
  "`anything' interface of ipa (global)."
  (interactive)
  (anything 'anything-c-source-ipa-global))

(provide 'anything-ipa)

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything-ipa.el"))
;;; anything-ipa.el ends here
