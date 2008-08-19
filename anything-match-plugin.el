;;; anything-match-plugin.el --- Humane match plug-in for anything
;; $Id: anything-match-plugin.el,v 1.2 2008-08-19 23:02:29 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: anything, matching
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-match-plugin.el

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

;;; History:

;; $Log: anything-match-plugin.el,v $
;; Revision 1.2  2008-08-19 23:02:29  rubikitch
;; candidates-in-buffer hack
;;
;; Revision 1.1  2008/08/19 19:45:11  rubikitch
;; Initial revision
;;

;;; Code:

(require 'anything)

;;;; multiple patterns
(defvar anything-use-multiple-patterns t
  "If non-nil, enable anything-use-multiple-patterns.")

(defvar amp-mp-pattern-info nil)
(defun amp-mp-make-regexps (pattern)
  (if (string= pattern "") '("")
    (loop for s in (split-string (replace-regexp-in-string "\\\\ " "\000\000" pattern) " " t)
        collect (replace-regexp-in-string "\000\000" " " s))))
(defun amp-mp-get-regexps (pattern)
  (unless (equal pattern (car amp-mp-pattern-info))
    (setq amp-mp-pattern-info (cons pattern (amp-mp-make-regexps pattern))))
  (cdr amp-mp-pattern-info))

(defun anything-multiple-pattern-search (pattern &rest ignore)
  "search function of anything-multiple-patterns."
  (loop with patterns = (amp-mp-get-regexps pattern)
        with first = (car patterns)
        with rest = (cdr patterns)
        while (re-search-forward first nil t)
        when (loop for re in rest
                   always (progn
                            (goto-char (point-at-bol))
                            (re-search-forward re (point-at-eol) t)))
        do (return t)
        else
        do (forward-line 1)))
(defun* anything-multiple-pattern-match (str &optional (regexps (amp-mp-get-regexps anything-pattern)))
  "match function of anything-multiple-patterns."
  (loop for re in regexps
        always (string-match re str)))

;;;; prefix match
(defvar amp-prefix-pattern-info nil)
(defun amp-prefix-get-regexp (pattern)
  (unless (equal pattern (car amp-prefix-pattern-info))
    (setq amp-prefix-pattern-info
          (cons pattern (concat "^" (regexp-quote pattern)))))
  (cdr amp-prefix-pattern-info))

(defun* anything-prefix-match (str &optional (pattern anything-pattern))
  (string-match (amp-prefix-get-regexp pattern) str))
(defun* anything-prefix-search (pattern &rest ignore)
  (re-search-forward (amp-prefix-get-regexp pattern) nil t))

;;;; source compier
(defvar anything-default-match-functions
  '(anything-prefix-match anything-multiple-pattern-match))
(defvar anything-default-search-functions
  '(anything-prefix-search anything-multiple-pattern-search))
(defun anything-compile-source--match-plugin (source)
  `(,(if (assoc 'candidates-in-buffer source) '(match identity))
    (match ,@anything-default-match-functions
           ,@(assoc-default 'match source))
     (search ,@anything-default-search-functions
             ,@(assoc-default 'search source))
    ,@source))

(add-to-list 'anything-compile-source-functions 'anything-compile-source--match-plugin t)

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(declare (warn (unresolved 0)))
(when (fboundp 'expectations)
  (expectations
    (desc "amp-mp-make-regexps")
    (expect '("")
      (amp-mp-make-regexps ""))
    (expect '("foo" "bar")
      (amp-mp-make-regexps "foo bar"))
    (expect '("foo" "bar")
      (amp-mp-make-regexps " foo bar"))
    (expect '("foo" "bar")
      (amp-mp-make-regexps " foo bar "))
    (expect '("foo bar" "baz")
      (amp-mp-make-regexps "foo\\ bar baz"))
    (desc "anything-multiple-pattern-search")
    (expect t
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-multiple-pattern-search "under hu" nil t)))
    (expect nil
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-multiple-pattern-search "under hue" nil t)))
    (expect t
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-multiple-pattern-search "th+ r" nil t)))
    (expect t
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-multiple-pattern-search "r th+" nil t)))
    (desc "anything-multiple-pattern-match")
    (expect t
      (anything-multiple-pattern-match "thunder" '("th+" "r")))
    (expect t
      (anything-multiple-pattern-match "thunder" '("r" "th+")))
    (expect nil
      (anything-multiple-pattern-match "thunder" '("under" "hue")))
    (desc "anything-prefix-match")
    (expect (type integer)
      (anything-prefix-match "fobar" "fo"))
    (expect nil
      (anything-prefix-match "xfobar" "fo"))
    
    (desc "with candidates-in-buffer")
    (expect '(identity)
      (assoc-default 'match
                     (car (anything-compile-sources
                           '(((name . "FOO")
                              (candidates-in-buffer)))
                           '(anything-compile-source--candidates-in-buffer
                             anything-compile-source--match-plugin)))))
          
    (desc "functional")
    (expect '(("FOO" ("thunder")))
      (anything-test-candidates '(((name . "FOO")
                                   (candidates "fire" "thunder")))
                                "th+ r"
                                '(anything-compile-source--match-plugin)))
    (expect '(("FOO" ("one two")))
      (anything-test-candidates '(((name . "FOO")
                                   (candidates "one two" "three four")))
                                "e\\ t"
                                '(anything-compile-source--match-plugin)))
    (expect '(("FOO" ("thunder")))
      (anything-test-candidates '(((name . "FOO")
                                   (init
                                    . (lambda ()
                                        (with-current-buffer (anything-candidates-buffer 'global)
                                          (insert "fire\nthunder\nthanks\n"))))
                                   (candidates-in-buffer)))
                                "th+ r"
                                '(anything-compile-source--candidates-in-buffer
                                  anything-compile-source--match-plugin)))
    ))




(provide 'anything-match-plugin)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-match-plugin.el")
;;; anything-match-plugin.el ends here
