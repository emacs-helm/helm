;;; anything-migemo.el --- Migemo plug-in for anything
;; $Id: anything-migemo.el,v 1.18 2009-06-07 17:52:22 rubikitch Exp $

;; Copyright (C) 2007, 2008, 2009  rubikitch

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

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-migemo'
;;    `anything' with migemo extension.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; If you want to use migemo search source-locally, add (migemo) to
;; the source. It sets match and search attribute appropriately for
;; migemo.

;;; Setting:

;; (require 'anything-config)
;; (require 'anything-migemo)
;; (define-key global-map [(control ?:)] 'anything-migemo)

;;; Bug:

;; Simultaneous use of (candidates-in-buffer), (search
;; . migemo-forward) and (delayed) scrambles *anything* buffer. Maybe
;; because of collision of `migemo-process' and `run-with-idle-timer'

;;; History:

;; $Log: anything-migemo.el,v $
;; Revision 1.18  2009-06-07 17:52:22  rubikitch
;; New macro `anything-migemize-command'.
;;
;; Revision 1.17  2009/06/04 20:32:00  rubikitch
;; migemo is soft-required now; this file has no effect unless migemo is installed.
;;
;; Revision 1.16  2008/10/03 20:43:18  rubikitch
;; Use with anything-match-plugin.el
;;
;; Revision 1.15  2008/10/03 20:01:46  rubikitch
;; refactoring
;;
;; Revision 1.14  2008/08/25 08:29:02  rubikitch
;; `anything-migemo': anything-args
;;
;; Revision 1.13  2008/08/24 20:39:53  rubikitch
;; prevent the unit test from being byte-compiled.
;;
;; Revision 1.12  2008/08/24 18:01:25  rubikitch
;; *** empty log message ***
;;
;; Revision 1.11  2008/08/24 08:23:30  rubikitch
;; Rename `anything-candidates-buffer' -> `anything-candidate-buffer'
;;
;; Revision 1.10  2008/08/24 01:54:21  rubikitch
;; migemo attribute
;;
;; Revision 1.9  2008/08/19 21:38:09  rubikitch
;; match attribute bug fix
;;
;; Revision 1.8  2008/08/19 21:30:29  rubikitch
;; plug-in
;;
;; Revision 1.7  2008/08/10 22:45:02  rubikitch
;; Bug info
;;
;; Revision 1.6  2008/08/08 03:40:51  rubikitch
;; require migemo
;;
;; Revision 1.5  2008/08/08 03:38:34  rubikitch
;; add search attribute
;; unit tests
;;
;; Revision 1.4  2007/12/26 08:36:01  rubikitch
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

(eval-when-compile (require 'anything))
(require 'migemo nil t)
(require 'anything-match-plugin)
(defvar anything-use-migemo nil
  "[Internal] If non-nil, `anything' is migemo-ized.")
(defun anything-migemo (with-migemo &rest anything-args)
  "`anything' with migemo extension.
With prefix arugument, `anything-pattern' is migemo-ized, otherwise normal `anything'."
  (interactive "P")
  (let ((anything-use-migemo with-migemo))
    (apply 'anything anything-args)))

(defvar anything-previous-migemo-info '("" . "")
  "[Internal] Previous migemo query for anything-migemo.")
(defun* anything-string-match-with-migemo (str &optional (pattern anything-pattern))
  "Migemo version of `string-match'."
  (unless (string= pattern (car anything-previous-migemo-info))
    (setq anything-previous-migemo-info (cons pattern (migemo-get-pattern pattern))))
  (string-match (cdr anything-previous-migemo-info) str))

(defun* anything-mp-3migemo-match (str &optional (pattern anything-pattern))
  (loop for (pred . re) in (anything-mp-3-get-patterns pattern)
        always (funcall pred (anything-string-match-with-migemo str re))))
(defun anything-mp-3migemo-search (pattern &rest ignore)
  (anything-mp-3-search-base pattern 'migemo-forward 'migemo-forward))
(defun anything-mp-3migemo-search-backward (pattern &rest ignore)
  (anything-mp-3-search-base pattern 'migemo-backward 'migemo-backward))
;; (anything-string-match-with-migemo "日本語入力" "nihongo")
;; (anything-string-match-with-migemo "日本語入力" "nyuuryoku")
;; (anything-mp-3migemo-match "日本語入力" "nihongo nyuuryoku")
(defun anything-compile-source--migemo (source)
  (if (not (featurep 'migemo))
      source
    (let* ((match-identity-p 
            (or (assoc 'candidates-in-buffer source)
                (equal '(identity) (assoc-default 'match source))))
           (matcher 'anything-mp-3migemo-match)
           (searcher (if (assoc 'search-from-end source)
                         'anything-mp-3migemo-search-backward
                       'anything-mp-3migemo-search)))
      (cond (anything-use-migemo
             `((search ,@(assoc-default 'search source) ,searcher)
               ,(if match-identity-p
                    '(match identity)
                  `(match ,matcher
                          ,@(assoc-default 'match source)))
               ,@source))
            ((assoc 'migemo source)
             `((search ,searcher)
               ,(if match-identity-p
                    '(match identity)
                  `(match ,matcher))
               ,@source))
            (t source)))))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--migemo t)

(defvar anything-migemize-command-idle-delay 0.1
  "`anything-idle-delay' for migemized command.")
(defmacro anything-migemize-command (command)
  "Use migemo in COMMAND when selectiong candidate by `anything'.
Bind `anything-use-migemo' = t in COMMAND.
`anything-migemize-command-idle-delay' is used instead of  `anything-idle-delay'."
  `(defadvice ,command (around anything-use-migemo activate)
     (let ((anything-use-migemo t)
           (anything-idle-delay anything-migemize-command-idle-delay))
       ad-do-it)))

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "match")
      (expect '(("TEST" ("日本語")))
        (let ((anything-use-migemo t))
          (anything-test-candidates
           '(((name . "TEST")
              (candidates "日本語")))
           "nihongo"
           '(anything-compile-source--migemo))))
      (desc "migemo attribute")
      (expect '(("TEST" ("日本語")))
        (let ((anything-use-migemo nil))
          (anything-test-candidates
           '(((name . "TEST")
              (candidates "日本語")
              (migemo)))
           "nihongo"
           '(anything-compile-source--migemo))))

      (desc "with anything-match-plugin")
      (expect '(("FOO" ("日本語入力")))
        (let ((anything-use-migemo nil))
          (anything-test-candidates
           '(((name . "FOO")
              (init
               . (lambda ()
                   (with-current-buffer (anything-candidate-buffer 'global)
                     (insert "日本語会話\n日本語入力\n"))))
              (candidates-in-buffer)
              (migemo)))
           "nihongo nyuuryoku"
           '(anything-compile-source--candidates-in-buffer
             anything-compile-source--match-plugin
             anything-compile-source--migemo))))
      (expect '(("FOO" ("日本語入力")))
        (let ((anything-use-migemo t))
          (anything-test-candidates
           '(((name . "FOO")
              (init
               . (lambda ()
                   (with-current-buffer (anything-candidate-buffer 'global)
                     (insert "日本語会話\n日本語入力\n"))))
              (candidates-in-buffer)))
           "nihongo nyuuryoku"
           '(anything-compile-source--candidates-in-buffer
             anything-compile-source--match-plugin
             anything-compile-source--migemo))))
      (expect '(("FOO" ("日本語入力")))
        (let ((anything-use-migemo nil))
          (anything-test-candidates
           '(((name . "FOO")
              (init
               . (lambda ()
                   (with-current-buffer (anything-candidate-buffer 'global)
                     (insert "日本語会話\n日本語入力\n"))))
              (candidates-in-buffer)
              (search-from-end)
              (migemo)))
           "nihongo nyuuryoku"
           '(anything-compile-source--candidates-in-buffer
             anything-compile-source--match-plugin
             anything-compile-source--migemo))))
      (expect '(("FOO" ("日本語入力")))
        (let ((anything-use-migemo t))
          (anything-test-candidates
           '(((name . "FOO")
              (init
               . (lambda ()
                   (with-current-buffer (anything-candidate-buffer 'global)
                     (insert "日本語会話\n日本語入力\n"))))
              (candidates-in-buffer)
              (search-from-end)))
           "nihongo nyuuryoku"
           '(anything-compile-source--candidates-in-buffer
             anything-compile-source--match-plugin
             anything-compile-source--migemo))))
      (expect '(("TEST" ("日本語入力")))
        (let ((anything-use-migemo nil))
          (anything-test-candidates
           '(((name . "TEST")
              (migemo)
              (candidates "日本語入力")))
           "nihongo nyuuryoku"
           '(anything-compile-source--match-plugin anything-compile-source--migemo))))
      (expect '(("TEST" ("日本語入力")))
        (let ((anything-use-migemo t))
          (anything-test-candidates
           '(((name . "TEST")
              (candidates "日本語入力")))
           "nihongo nyuuryoku"
           '(anything-compile-source--match-plugin anything-compile-source--migemo))))
      )))

(provide 'anything-migemo)

;; Local Variables:
;; coding: utf-8
;; End:

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything-migemo.el"))
;;; anything-migemo.el ends here
