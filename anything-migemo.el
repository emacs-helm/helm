;;; anything-migemo.el --- Migemo extension for anything
;; $Id: anything-migemo.el,v 1.4 2007-12-26 08:36:01 rubikitch Exp $

;; Copyright (C) 2007  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: anything, convenience, tools, i18n, japanese
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-migemo.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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

;; Migemo extension of `anything'. Use `anything-migemo' instead of
;; `anything'. If `anything-migemo' is invoked with prefix argument,
;; `anything' is migemo-ized. This means that pattern matching of
;; `anything' candidates is done by migemo-expanded `anything-pattern'.

;;; Setting:

;; (require 'anything-config)
;; (require 'anything-migemo)
;; (define-key global-map [(control ?:)] 'anything-migemo)

;;; History:

;; $Log: anything-migemo.el,v $
;; Revision 1.4  2007-12-26 08:36:01  rubikitch
;; changed match priority
;;
;; Revision 1.3  2007/12/25 19:55:59  rubikitch
;; patch is not needed anymore.
;;
;; Revision 1.2  2007/12/25 13:05:46  rubikitch
;; speed up by memoization
;;
;; Revision 1.1  2007/12/25 12:03:25  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-use-migemo nil
  "[Internal] If non-nil, `anything' is migemo-ized.")
(defun anything-migemo (with-migemo)
  "`anything' with migemo extension.
With prefix arugument, `anything-pattern' is migemo-ized, otherwise normal `anything'."
  (interactive "P")
  (let ((anything-use-migemo with-migemo))
    (anything)))

(defvar anything-previous-migemo-info '("" . "")
  "[Internal] Previous migemo query for anything-migemo.")
(defun anything-string-match-with-migemo (str)
  "Migemo version of `string-match'."
  (unless (string= anything-pattern (car anything-previous-migemo-info))
    (setq anything-previous-migemo-info (cons anything-pattern (migemo-get-pattern anything-pattern))))
  (string-match (cdr anything-previous-migemo-info) str))

(defadvice anything-get-sources (around anything-migemo activate)
  "All the `anything-sources' is delayed when `anything' is migemo-ized."
  (if (not anything-use-migemo)
      ad-do-it
    (setq ad-return-value
          (mapcar (lambda (source)
                    `((delayed)
                      ,@source
                      (match . (anything-string-match-with-migemo))))
                  ad-do-it))))


(provide 'anything-migemo)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-migemo.el")
;;; anything-migemo.el ends here
