;;; helm-lib.el --- Helm routines. -*- lexical-binding: t -*-

;; Copyright (C) 2015  Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; URL: http://github.com/emacs-helm/helm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; All helm functions that don't require specific helm code should go here.

;;; Code:

(defmacro helm-with-gensyms (symbols &rest body)
  "Bind the SYMBOLS to fresh uninterned symbols and eval BODY."
  (declare (indent 1))
  `(let ,(mapcar (lambda (s)
                   ;; Use cl-gensym here instead of make-symbol
                   ;; to ensure a symbol that have a live that go
                   ;; beyond the live of its macro have different name.
                   ;; i.e symbols created with `with-helm-temp-hook'
                   ;; should have random names.
                   `(,s (cl-gensym (symbol-name ',s))))
                 symbols)
     ,@body))

(defun helm-iter-list (seq)
  "Return an iterator object from SEQ."
  (let ((lis seq))
    (lambda ()
      (let ((elm (car lis)))
        (setq lis (cdr lis))
        elm))))

(defun helm-iter-next (iterator)
  "Return next elm of ITERATOR."
  (funcall iterator))

(defun helm-make-actions (&rest args)
  "Build an alist with (NAME . ACTION) elements with each pairs in ARGS.
Where NAME is a string or a function returning a string or nil and ACTION
a function.
If NAME returns nil the pair is skipped.

\(fn NAME ACTION ...)"
  (cl-loop for i on args by #'cddr
           for name  = (car i)
           when (functionp name)
           do (setq name (funcall name))
           when name
           collect (cons name (cadr i))))

(defun helm-flatten-list (seq &optional omit-nulls)
  "Return a list of all single elements of sublists in SEQ."
  (let (result)
    (cl-labels ((flatten (seq)
                  (cl-loop
                        for elm in seq
                        if (and (or elm
                                    (null omit-nulls))
                                (or (atom elm)
                                    (functionp elm)
                                    (and (consp elm)
                                         (cdr elm)
                                         (atom (cdr elm)))))
                        do (push elm result)
                        else do (flatten elm))))
      (flatten seq))
    (nreverse result)))

(defun helm-mklist (obj)
  "If OBJ is a list \(but not lambda\), return itself.
Otherwise make a list with one element."
  (if (and (listp obj) (not (functionp obj)))
      obj
    (list obj)))

(defmacro helm-aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defun helm-current-line-contents ()
  "Current line string without properties."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun helm-stringify (str-or-sym)
  "Get string of STR-OR-SYM."
  (if (stringp str-or-sym)
      str-or-sym
    (symbol-name str-or-sym)))

(defun helm-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (if (symbolp str-or-sym)
      str-or-sym
    (intern str-or-sym)))

(defun helm-remove-if-not-match (regexp seq)
  "Remove all elements of SEQ that don't match REGEXP."
  (cl-loop for s in seq
           for str = (cond ((symbolp s)
                            (symbol-name s))
                           ((consp s)
                            (car s))
                           (t s))
           when (string-match-p regexp str)
           collect s))

(defun helm-remove-if-match (regexp seq)
  "Remove all elements of SEQ that match REGEXP."
  (cl-loop for s in seq
           for str = (cond ((symbolp s)
                            (symbol-name s))
                           ((consp s)
                            (car s))
                           (t s))
           unless (string-match-p regexp str)
           collect s))

(defun helm-transform-mapcar (function args)
  "`mapcar' for candidate-transformer.

ARGS is (cand1 cand2 ...) or ((disp1 . real1) (disp2 . real2) ...)

\(helm-transform-mapcar 'upcase '(\"foo\" \"bar\"))
=> (\"FOO\" \"BAR\")
\(helm-transform-mapcar 'upcase '((\"1st\" . \"foo\") (\"2nd\" . \"bar\")))
=> ((\"1st\" . \"FOO\") (\"2nd\" . \"BAR\"))
"
  (cl-loop for arg in args
        if (consp arg)
        collect (cons (car arg) (funcall function (cdr arg)))
        else
        collect (funcall function arg)))

(defsubst helm--mapconcat-pattern (pattern)
  "Transform string PATTERN in regexp for further fuzzy matching.
e.g helm.el$
    => \"[^h]*h[^e]*e[^l]*l[^m]*m[^.]*[.][^e]*e[^l]*l$\"
    ^helm.el$
    => \"helm[.]el$\"."
  (let ((ls (split-string-and-unquote pattern "")))
    (if (string= "^" (car ls))
        ;; Exact match.
        (mapconcat (lambda (c)
                     (if (and (string= c "$")
                              (string-match "$\\'" pattern))
                         c (regexp-quote c)))
                   (cdr ls) "")
        ;; Fuzzy match.
        (mapconcat (lambda (c)
                     (if (and (string= c "$")
                              (string-match "$\\'" pattern))
                         c (format "[^%s]*%s" c (regexp-quote c))))
                   ls ""))))

(defsubst helm--collect-pairs-in-string (string)
  (cl-loop for str on (split-string string "" t) by 'cdr
           when (cdr str)
           collect (list (car str) (cadr str))))

(defun helm-symbol-name (obj)
  (if (or (consp obj) (byte-code-function-p obj))
      "Anonymous"
      (symbol-name obj)))

(cl-defmacro helm-position (item seq &key (test 'eq) all)
  "A simple and faster replacement of CL `position'.
Return position of first occurence of ITEM found in SEQ.
Argument SEQ can be a string, in this case ITEM have to be a char.
Argument ALL, if non--nil specify to return a list of positions of
all ITEM found in SEQ."
  (let ((key (if (stringp seq) 'across 'in)))
    `(cl-loop for c ,key ,seq
           for index from 0
           when (funcall ,test c ,item)
           if ,all collect index into ls
           else return index
           finally return ls)))

(defun helm-substring (str width)
  "Return the substring of string STR from 0 to WIDTH.
Handle multibyte characters by moving by columns."
  (with-temp-buffer
    (save-excursion
      (insert str))
    (move-to-column width)
    (buffer-substring (point-at-bol) (point))))

(cl-defun helm-substring-by-width (str width &optional (endstr "..."))
  "Truncate string STR to end at column WIDTH.
Similar to `truncate-string-to-width'.
Add ENDSTR (default \"...\") at end of truncated STR.
Add spaces at end if needed to reach WIDTH when STR is shorter than WIDTH."
  (cl-loop for ini-str = str
        then (substring ini-str 0 (1- (length ini-str)))
        for sw = (string-width ini-str)
        when (<= sw width) return
        (concat ini-str endstr (make-string (- width sw) ? ))))

(defun helm-string-multibyte-p (str)
  "Check if string STR contains multibyte characters."
  (cl-loop for c across str
        thereis (> (char-width c) 1)))

(defun helm-get-pid-from-process-name (process-name)
  "Get pid from running process PROCESS-NAME."
  (cl-loop with process-list = (list-system-processes)
        for pid in process-list
        for process = (assoc-default 'comm (process-attributes pid))
        when (and process (string-match process-name process))
        return pid))

(defun helm-ff-find-printers ()
  "Return a list of available printers on Unix systems."
  (when (executable-find "lpstat")
    (let ((printer-list (with-temp-buffer
                          (call-process "lpstat" nil t nil "-a")
                          (split-string (buffer-string) "\n"))))
      (cl-loop for p in printer-list
            for printer = (car (split-string p))
            when printer
            collect printer))))

(defun helm-region-active-p ()
  (and transient-mark-mode mark-active (/= (mark) (point))))

(defun helm-skip-entries (seq regexp-list)
  "Remove entries which matches one of REGEXP-LIST from SEQ."
  (cl-loop for i in seq
        unless (cl-loop for regexp in regexp-list
                     thereis (and (stringp i)
                                  (string-match regexp i)))
        collect i))

(defun helm-shadow-entries (seq regexp-list)
  "Put shadow property on entries in SEQ matching a regexp in REGEXP-LIST."
  (let ((face 'italic))
    (cl-loop for i in seq
          if (cl-loop for regexp in regexp-list
                   thereis (and (stringp i)
                                (string-match regexp i)))
          collect (propertize i 'face face)
          else collect i)))

(defun helm-describe-function (func)
  "FUNC is symbol or string."
  (describe-function (helm-symbolify func))
  (message nil))

(defun helm-describe-variable (var)
  "VAR is symbol or string."
  (describe-variable (helm-symbolify var))
  (message nil))

(defun helm-find-function (func)
  "FUNC is symbol or string."
  (find-function (helm-symbolify func)))

(defun helm-find-variable (var)
  "VAR is symbol or string."
  (find-variable (helm-symbolify var)))

(defun helm-kill-new (candidate &optional replace)
  "CANDIDATE is symbol or string.
See `kill-new' for argument REPLACE."
  (kill-new (helm-stringify candidate) replace))

(cl-defun helm-fast-remove-dups (seq &key (test 'eq))
  "Remove duplicates elements in list SEQ.
This is same as `remove-duplicates' but with memoisation.
It is much faster, especially in large lists.
A test function can be provided with TEST argument key.
Default is `eq'."
  (cl-loop with cont = (make-hash-table :test test)
        for elm in seq
        unless (gethash elm cont)
        do (puthash elm elm cont)
        finally return
        (cl-loop for i being the hash-values in cont collect i)))

(defun helm-basename (fname &optional ext)
  "Print FNAME  with any  leading directory  components removed.
If specified, also remove filename extension EXT."
  (let ((non-essential t))
    (if (and ext (or (string= (file-name-extension fname) ext)
                     (string= (file-name-extension fname t) ext))
             (not (file-directory-p fname)))
        (file-name-sans-extension (file-name-nondirectory fname))
      (file-name-nondirectory (directory-file-name fname)))))

(defun helm-basedir (fname)
  "Return the base directory of filename."
  (helm-aif (and fname (file-name-directory fname))
      (file-name-as-directory it)))

(defun helm-current-directory ()
  "Return current-directory name at point.
Useful in dired buffers when there is inserted subdirs."
  (if (eq major-mode 'dired-mode)
      (dired-current-directory)
    default-directory))

(defun helm-w32-prepare-filename (file)
  "Convert filename FILE to something usable by external w32 executables."
  (replace-regexp-in-string ; For UNC paths
   "/" "\\"
   (replace-regexp-in-string ; Strip cygdrive paths
    "/cygdrive/\\(.\\)" "\\1:"
    file nil nil) nil t))

;;;###autoload
(defun helm-w32-shell-execute-open-file (file)
  (interactive "fOpen file:")
  (with-no-warnings
    (w32-shell-execute "open" (helm-w32-prepare-filename file))))

(provide 'helm-lib)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-lib ends here
