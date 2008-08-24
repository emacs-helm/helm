;;; anything-match-plugin.el --- Humane match plug-in for anything
;; $Id: anything-match-plugin.el,v 1.10 2008-08-24 17:48:53 rubikitch Exp $

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

;; Change anything.el matching algorithm humanely.
;; It gives anything.el search refinement functionality.
;; exact match -> prefix match -> multiple regexps match

;; Just require it to use.

;;; History:

;; $Log: anything-match-plugin.el,v $
;; Revision 1.10  2008-08-24 17:48:53  rubikitch
;; Add commentary
;;
;; Revision 1.9  2008/08/24 08:23:16  rubikitch
;; Rename `anything-candidates-buffer' -> `anything-candidate-buffer'
;;
;; Revision 1.8  2008/08/22 21:25:44  rubikitch
;; *** empty log message ***
;;
;; Revision 1.7  2008/08/22 21:17:58  rubikitch
;; exact, prefix match: faster
;;
;; Revision 1.6  2008/08/22 19:40:22  rubikitch
;; exact -> prefix -> mp-3 by default because of speed
;;
;; Revision 1.5  2008/08/22 19:04:53  rubikitch
;; reimplemented
;;
;; Revision 1.4  2008/08/20 00:10:15  rubikitch
;; *** empty log message ***
;;
;; Revision 1.3  2008/08/19 23:30:39  rubikitch
;; exact match support
;;
;; Revision 1.2  2008/08/19 23:02:29  rubikitch
;; candidates-in-buffer hack
;;
;; Revision 1.1  2008/08/19 19:45:11  rubikitch
;; Initial revision
;;

;;; Code:

(require 'anything)
(require 'cl)

;;;; multiple patterns
(defvar anything-use-multiple-patterns t
  "If non-nil, enable anything-use-multiple-patterns.")

(defun amp-permute (list)
  (if (null list)
      (list nil)
    (mapcan (lambda (first)
              (mapcar (lambda (rest)
                        (cons first rest))
                      (amp-permute (remove* first list :count 1 :test #'equal))))
            list)))
(defun amp-mp-make-regexps (pattern)
  (if (string= pattern "") '("")
    (loop for s in (split-string (replace-regexp-in-string "\\\\ " "\000\000" pattern) " " t)
        collect (replace-regexp-in-string "\000\000" " " s))))

(defun amp-mp-1-make-regexp (pattern)
  (mapconcat 'identity (amp-mp-make-regexps pattern) ".*"))

(defmacro amp-define (prefix regexp-expr)
  (let ((pattern-str (intern (concat prefix "pattern-str")))
        (pattern-regexp (intern (concat prefix "pattern-regexp")))
        (get-regexp (intern (concat prefix "get-regexp"))) 
        (match (intern (concat prefix "match")))
        (search (intern (concat prefix "search"))))
    `(progn
       (defvar ,pattern-str nil)
       (defvar ,pattern-regexp nil)
       (defsubst ,get-regexp (pattern)
         (unless (equal pattern ,pattern-str)
           (setq ,pattern-str pattern
                 ,pattern-regexp ,regexp-expr))
         ,pattern-regexp)
       (defun* ,match (str &optional (pattern anything-pattern))
         (string-match (,get-regexp pattern) str))
       (defun ,search (pattern &rest ignore)
         (re-search-forward (,get-regexp pattern) nil t)))))
  
;; exact match
;(amp-define "anything-exact-" (concat (anything-prefix-get-regexp pattern) "$"))
(defun anything-exact-match (str &optional pattern)
  (string= str (or pattern anything-pattern)))
(defun anything-exact-search (pattern &rest ignore)
  (and (search-forward (concat "\n" pattern "\n") nil t)
       (forward-line -1)))
;; prefix match
;;(amp-define "anything-prefix-" (concat "^" (regexp-quote pattern)))
(defun anything-prefix-match (str &optional pattern)
  (setq pattern (or pattern anything-pattern))
  (let ((len (length pattern)))
    (and (<= len (length str))
         (string= (substring str 0 len) pattern ))))
(defun anything-prefix-search (pattern &rest ignore)
  (search-forward (concat "\n" pattern) nil t))
;; multiple regexp patterns 1 (order is preserved / prefix)
(amp-define "anything-mp-1-" (concat "^" (amp-mp-1-make-regexp pattern)))
;; multiple regexp patterns 2 (order is preserved / partial)
(amp-define "anything-mp-2-" (concat "^.+" (amp-mp-1-make-regexp pattern)))
;; multiple regexp patterns 3 (permutation)
(amp-define "anything-mp-3-"
            (mapconcat (lambda (regexps)
                         (concat "\\(" (mapconcat #'identity regexps ".*") "\\)"))
                       (amp-permute (amp-mp-make-regexps pattern))
                       "\\|"))

                         
;;;; source compier
(defvar anything-default-match-functions
  '(anything-exact-match anything-prefix-match  anything-mp-3-match))
(defvar anything-default-search-functions
  '(anything-exact-search anything-prefix-search anything-mp-3-search))

(defun anything-compile-source--match-plugin (source)
  `(,(if (or (assoc 'candidates-in-buffer source)
             (equal '(identity) (assoc-default 'match source)))
         '(match identity)
       `(match ,@anything-default-match-functions
               ,@(assoc-default 'match source)))
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
    (desc "anything-exact-match")
    (expect (non-nil)
      (anything-exact-match "thunder" "thunder"))
    (expect nil
      (anything-exact-match "thunder" "fire"))
    (desc "anything-exact-search")
    (expect (non-nil)
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-exact-search "thunder" nil t)))
    (expect (non-nil)
      (with-temp-buffer
        (insert "\nfire\nthunder\n")
        (goto-char 1)
        (anything-exact-search "fire" nil t)))
    (desc "amp-mp-1-make-regexp")
    (expect "a.*b"
      (amp-mp-1-make-regexp "a b"))
    (expect "a b"
      (amp-mp-1-make-regexp "a\\ b"))
    (expect "a.*b c"
      (amp-mp-1-make-regexp "a b\\ c"))
    (expect ""
      (amp-mp-1-make-regexp ""))
    (desc "anything-mp-1-search")
    (expect (non-nil)
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-mp-1-search "th+ r" nil t)))
    (desc "anything-mp-2-search")
    (expect (non-nil)
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-mp-2-search "h+ r" nil t)))
    (expect nil
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-mp-2-search "th+ r" nil t)))
    (desc "anything-mp-3-search")
    (expect (non-nil)
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-mp-3-search "h+ r" nil t)))
    (expect (non-nil)
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-mp-3-search "th+ r" nil t)))
    (expect (non-nil)
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-mp-3-search "r th+" nil t)))
    (expect nil
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-mp-3-search "under hue" nil t)))
    (expect (non-nil)
      (with-temp-buffer
        (insert "fire\nthunder\n")
        (goto-char 1)
        (anything-mp-3-search "r th+ n" nil t)))
    (desc "anything-mp-1-match")
    (expect (non-nil)
      (anything-mp-1-match "thunder" "th+ r"))
    (desc "anything-mp-2-match")
    (expect (non-nil)
      (anything-mp-2-match "thunder" "h+ r"))
    (expect nil
      (anything-mp-2-match "thunder" "th+ r"))
    (desc "anything-mp-3-match")
    (expect (non-nil)
      (anything-mp-3-match "thunder" "h+ r"))
    (expect (non-nil)
      (anything-mp-3-match "thunder" "th+ r"))
    (expect (non-nil)
      (anything-mp-3-match "thunder" "r th+"))
    (expect nil
      (anything-mp-3-match "thunder" "under hue"))
    (expect (non-nil)
      (anything-mp-3-match "thunder" "r th+ n"))
    (desc "anything-prefix-match")
    (expect (non-nil)
      (anything-prefix-match "fobar" "fo"))
    (expect nil
      (anything-prefix-match "xfobar" "fo"))
    
    (desc "with identity match")
    (expect '(identity)
      (assoc-default 'match
                     (car (anything-compile-sources
                           '(((name . "FOO")
                              (candidates-in-buffer)))
                           '(anything-compile-source--candidates-in-buffer
                             anything-compile-source--match-plugin)))))
    (expect '(identity)
      (assoc-default 'match
                     (car (anything-compile-sources
                           '(((name . "FOO")
                              (match identity)))
                           '(anything-compile-source--match-plugin)))))
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
                                        (with-current-buffer (anything-candidate-buffer 'global)
                                          (insert "fire\nthunder\nthanks\n"))))
                                   (candidates-in-buffer)))
                                "th+ r"
                                '(anything-compile-source--candidates-in-buffer
                                  anything-compile-source--match-plugin)))
    (expect '(("FOO" ("foo" "foobar")))
      (anything-test-candidates '(((name . "FOO")
                                   (candidates "foobar" "foo")))
                                "foo"
                                '(anything-compile-source--match-plugin)))
    (expect '(("FOO" ("foo" "foobar")))
      (anything-test-candidates '(((name . "FOO")
                                   (init
                                    . (lambda ()
                                        (with-current-buffer (anything-candidate-buffer 'global)
                                          (insert "foobar\nfoo\n"))))
                                   (candidates-in-buffer)))
                                "foo"
                                '(anything-compile-source--candidates-in-buffer
                                  anything-compile-source--match-plugin)))
    (expect '(("FOO" ("foo")))
      (anything-test-candidates '(((name . "FOO")
                                   (init
                                    . (lambda ()
                                        (with-current-buffer (anything-candidate-buffer 'global)
                                          (insert "foo\n"))))
                                   (candidates-in-buffer)))
                                "foo"
                                '(anything-compile-source--candidates-in-buffer
                                  anything-compile-source--match-plugin)))
    (expect '(("FOO" ("foo")))
      (anything-test-candidates '(((name . "FOO")
                                   (init
                                    . (lambda ()
                                        (with-current-buffer (anything-candidate-buffer 'global)
                                          (insert "bar\nfoo\ntest\n"))))
                                   (candidates-in-buffer)))
                                "foo"
                                '(anything-compile-source--candidates-in-buffer
                                  anything-compile-source--match-plugin)))
    (expect '(("FOO" ("foobar" "foo")))
      (anything-test-candidates '(((name . "FOO")
                                   (init
                                    . (lambda ()
                                        (with-current-buffer (anything-candidate-buffer 'global)
                                          (insert "foobar\nfoo\n"))))
                                   (candidates-in-buffer)))
                                ""
                                '(anything-compile-source--candidates-in-buffer
                                  anything-compile-source--match-plugin)))
    ))




(provide 'anything-match-plugin)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-match-plugin.el")
;;; anything-match-plugin.el ends here
