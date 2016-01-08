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

(require 'cl-lib)
(require 'dired)


;;; User vars.
;;
(defcustom helm-file-globstar t
  "Same as globstar bash shopt option.
When non--nil a pattern beginning with two stars will expand recursively.
Directories expansion is not supported yet."
  :group 'helm
  :type 'boolean)

(defcustom helm-yank-text-at-point-function nil
  "The function used to forward point with `helm-yank-text-at-point'.
With a nil value, fallback to default `forward-word'.
The function should take one arg, an integer like `forward-word'.
NOTE: Using `forward-symbol' here is not very useful as it is already
provided by \\<helm-map>\\[next-history-element]."
  :type  'function
  :group 'helm)

(defcustom helm-scroll-amount nil
  "Scroll amount when scrolling other window in a helm session.
It is used by `helm-scroll-other-window'
and `helm-scroll-other-window-down'.

If you prefer scrolling line by line, set this value to 1."
  :group 'helm
  :type 'integer)


;;; Internal vars
;;
(defvar helm-yank-point nil)
(defvar helm-pattern ""
  "The input pattern used to update the helm buffer.")
(defvar helm-buffer "*helm*"
  "Buffer showing completions.")
(defvar helm-current-buffer nil
  "Current buffer when `helm' is invoked.")
(defvar helm-suspend-update-flag nil)
(defvar helm-action-buffer "*helm action*"
  "Buffer showing actions.")

;;; Macros helper.
;;
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

;;; Iterators
;;
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

(defmacro helm-aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defun helm-current-line-contents ()
  "Current line string without properties."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

;;; Fuzzy matching routines
;;
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

;;; Help routines.
;;
(defun helm-help-internal (bufname insert-content-fn)
  "Show long message during `helm' session in BUFNAME.
INSERT-CONTENT-FN is the function that insert
text to be displayed in BUFNAME."
  (let ((winconf (current-frame-configuration)))
    (unwind-protect
         (progn
           (setq helm-suspend-update-flag t)
           (set-buffer (get-buffer-create bufname))
           (switch-to-buffer bufname)
           (delete-other-windows)
           (delete-region (point-min) (point-max))
           (org-mode)
           (save-excursion
             (funcall insert-content-fn))
           (setq cursor-type nil)
           (buffer-disable-undo)
           (helm-help-event-loop))
      (setq helm-suspend-update-flag nil)
      (set-frame-configuration winconf))))

(defun helm-help-scroll-up (amount)
  (condition-case _err
      (scroll-up-command amount)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun helm-help-scroll-down (amount)
  (condition-case _err
      (scroll-down-command amount)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun helm-help-event-loop ()
  (let ((prompt (propertize
                 "[SPC,C-v,down,next:NextPage  b,M-v,up,prior:PrevPage C-s/r:Isearch q:Quit]"
                 'face 'helm-helper))
        scroll-error-top-bottom)
    (cl-loop for event = (read-key prompt) do
             (cl-case event
               ((?\C-v ? down next) (helm-help-scroll-up helm-scroll-amount))
               ((?\M-v ?b up prior) (helm-help-scroll-down helm-scroll-amount))
               (?\C-s (isearch-forward))
               (?\C-r (isearch-backward))
               (?q (cl-return))
               (t (ignore))))))


;;; List processing
;;
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

(defun helm-skip-entries (seq black-regexp-list &optional white-regexp-list)
  "Remove entries which matches one of REGEXP-LIST from SEQ."
  (cl-loop for i in seq
           unless (and (cl-loop for re in black-regexp-list
                                thereis (and (stringp i)
                                             (string-match-p re i)))
                       (null
                        (cl-loop for re in white-regexp-list
                                thereis (and (stringp i)
                                             (string-match-p re i)))))
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

;;; Strings processing.
;;
(defun helm-stringify (str-or-sym)
  "Get string of STR-OR-SYM."
  (if (stringp str-or-sym)
      str-or-sym
    (symbol-name str-or-sym)))

(defun helm-substring (str width)
  "Return the substring of string STR from 0 to WIDTH.
Handle multibyte characters by moving by columns."
  (with-temp-buffer
    (save-excursion
      (insert str))
    (move-to-column width)
    (buffer-substring (point-at-bol) (point))))

(defun helm-substring-by-width (str width &optional endstr)
  "Truncate string STR to end at column WIDTH.
Similar to `truncate-string-to-width'.
Add ENDSTR at end of truncated STR.
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

(defun helm-quote-whitespace (candidate)
  "Quote whitespace, if some, in string CANDIDATE."
  (replace-regexp-in-string " " "\\\\ " candidate))


;;; Symbols routines
;;
(defun helm-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (if (symbolp str-or-sym)
      str-or-sym
    (intern str-or-sym)))

(defun helm-symbol-name (obj)
  (if (or (consp obj) (byte-code-function-p obj))
      "Anonymous"
      (symbol-name obj)))

(defun helm-describe-function (func)
  "FUNC is symbol or string."
  (describe-function (helm-symbolify func))
  (message nil))

(defun helm-describe-variable (var)
  "VAR is symbol or string."
  (describe-variable (helm-symbolify var))
  (message nil))

(defun helm-describe-face (face)
  "VAR is symbol or string."
  (describe-face (helm-symbolify face))
  (message nil))

(defun helm-find-function (func)
  "FUNC is symbol or string."
  (find-function (helm-symbolify func)))

(defun helm-find-variable (var)
  "VAR is symbol or string."
  (find-variable (helm-symbolify var)))

(defun helm-find-face-definition (face)
  "FACE is symbol or string."
  (find-face-definition (helm-symbolify face)))

(defun helm-kill-new (candidate &optional replace)
  "CANDIDATE is symbol or string.
See `kill-new' for argument REPLACE."
  (kill-new (helm-stringify candidate) replace))

;;; Files routines
;;
(defun helm-file-name-sans-extension (filename)
  "Same as `file-name-sans-extension' but remove all extensions."
  (helm-aif (file-name-sans-extension filename)
      (if (string-match "\\." it)
          (helm-file-name-sans-extension it)
          it)))

(defun helm-basename (fname &optional ext)
  "Print FNAME  with any  leading directory  components removed.
If specified, also remove filename extension EXT.
Arg EXT can be specified as a string with or without dot."
  (let ((non-essential t))
    (if (and ext (or (string= (file-name-extension fname) ext)
                     (string= (file-name-extension fname t) ext))
             (not (file-directory-p fname)))
        (file-name-sans-extension (file-name-nondirectory fname))
      (file-name-nondirectory (directory-file-name fname)))))

(defun helm-basedir (fname)
  "Return the base directory of filename ending by a slash."
  (helm-aif (and fname
                 (or (and (string= fname "~") "~")
                     (file-name-directory fname)))
      (file-name-as-directory it)))

(defun helm-current-directory ()
  "Return current-directory name at point.
Useful in dired buffers when there is inserted subdirs."
  (expand-file-name
   (if (eq major-mode 'dired-mode)
       (dired-current-directory)
       default-directory)))

(defun helm-w32-prepare-filename (file)
  "Convert filename FILE to something usable by external w32 executables."
  (replace-regexp-in-string ; For UNC paths
   "/" "\\"
   (replace-regexp-in-string ; Strip cygdrive paths
    "/cygdrive/\\(.\\)" "\\1:"
    file nil nil) nil t))

(defun helm-w32-shell-execute-open-file (file)
  (interactive "fOpen file:")
  (with-no-warnings
    (w32-shell-execute "open" (helm-w32-prepare-filename file))))

;; Same as `vc-directory-exclusion-list'.
(defvar helm-walk-ignore-directories
  '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr"
    "_MTN" "_darcs" "{arch}" ".gvfs"))

(cl-defun helm-walk-directory (directory &key (path 'basename)
                                           (directories t)
                                           match skip-subdirs)
  "Walk through DIRECTORY tree.
Argument PATH can be one of basename, relative, full, or a function
called on file name, default to basename.
Argument DIRECTORIES when non--nil (default) return also directories names,
otherwise skip directories names.
Argument MATCH can be a predicate or a regexp.
Argument SKIP-SUBDIRS when non--nil will skip `helm-walk-ignore-directories'
unless it is given as a list of directories, in this case this list will be used
instead of `helm-walk-ignore-directories'."
  (let* ((result '())
         (fn (cl-case path
               (basename 'file-name-nondirectory)
               (relative 'file-relative-name)
               (full     'identity)
               (t        path))))
    (cl-labels ((ls-rec (dir)
                  (unless (and skip-subdirs
                               (member (helm-basename dir)
                                       (if (listp skip-subdirs)
                                           skip-subdirs
                                         helm-walk-ignore-directories)))
                    (cl-loop with ls = (sort (file-name-all-completions "" dir)
                                             'string-lessp)
                          for f in ls
                          ;; Use `directory-file-name' to remove the final slash.
                          ;; Needed to avoid infloop on symlinks symlinking
                          ;; a directory inside it [1].
                          for file = (directory-file-name
                                      (expand-file-name f dir))
                          unless (member f '("./" "../"))
                          ;; A directory.
                          if (char-equal (aref f (1- (length f))) ?/)
                          do (progn (when directories
                                      (push (funcall fn file) result))
                                    ;; Don't recurse in symlinks.
                                    ;; `file-symlink-p' have to be called
                                    ;; on the directory with its final
                                    ;; slash removed [1].
                                    (and (not (file-symlink-p file))
                                         (ls-rec file)))
                          else do
                          (if match
                              (and (if (functionp match)
                                       (funcall match f)
                                     (and (stringp match)
                                          (string-match match f)))
                                   (push (funcall fn file) result))
                            (push (funcall fn file) result))))))
      (ls-rec directory)
      (nreverse result))))

(defun helm-file-expand-wildcards (pattern &optional full)
  "Same as `file-expand-wildcards' but allow recursion.
Recursion happen when PATTERN starts with two stars.
Directories expansion is not supported."
  (let ((bn (helm-basename pattern)))
    (if (and helm-file-globstar
             (string-match "\\`\\*\\{2\\}\\(.*\\)" bn))
        (helm-walk-directory (helm-basedir pattern)
                             :path (cl-case full
                                     (full 'full)
                                     (relative 'relative)
                                     ((basename nil) 'basename)
                                     (t 'full))
                             :directories nil
                             :match (wildcard-to-regexp bn)
                             :skip-subdirs t)
        (file-expand-wildcards pattern full))))

;;; helm internals
;;
(defun helm-set-pattern (pattern &optional noupdate)
  "Set minibuffer contents to PATTERN.
if optional NOUPDATE is non-nil, helm buffer is not changed."
  (with-selected-window (or (active-minibuffer-window) (minibuffer-window))
    (delete-minibuffer-contents)
    (insert pattern))
  (when noupdate
    (setq helm-pattern pattern)))

(defun helm-minibuffer-completion-contents ()
  "Return the user input in a minibuffer before point as a string.
That is what completion commands operate on."
  (buffer-substring (field-beginning) (point)))

(defmacro with-helm-buffer (&rest body)
  "Eval BODY inside `helm-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer (helm-buffer-get)
     ,@body))

(defmacro with-helm-current-buffer (&rest body)
  "Eval BODY inside `helm-current-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer (or (and (buffer-live-p helm-current-buffer)
                                 helm-current-buffer)
                            (setq helm-current-buffer
                                  (current-buffer)))
     ,@body))

(defun helm-buffer-get ()
  "Return `helm-action-buffer' if shown otherwise `helm-buffer'."
  (if (helm-action-window)
      helm-action-buffer
    helm-buffer))

(defun helm-window ()
  "Window of `helm-buffer'."
  (get-buffer-window (helm-buffer-get) 0))

(defun helm-action-window ()
  "Window of `helm-action-buffer'."
  (get-buffer-window helm-action-buffer 'visible))

(defmacro with-helm-window (&rest body)
  "Be sure BODY is excuted in the helm window."
  (declare (indent 0) (debug t))
  `(with-selected-window (helm-window)
     ,@body))


;; Yank text at point.
;;
;;
(defun helm-yank-text-at-point ()
  "Yank text at point in `helm-current-buffer' into minibuffer."
  (interactive)
  (with-helm-current-buffer
    (let ((fwd-fn (or helm-yank-text-at-point-function #'forward-word)))
      ;; Start to initial point if C-w have never been hit.
      (unless helm-yank-point (setq helm-yank-point (point)))
      (save-excursion
        (goto-char helm-yank-point)
        (funcall fwd-fn 1)
        (helm-set-pattern
         (concat
          helm-pattern (replace-regexp-in-string
                        "\\`\n" ""
                        (buffer-substring-no-properties
                         helm-yank-point (point)))))
        (setq helm-yank-point (point))))))

(defun helm-reset-yank-point ()
  (setq helm-yank-point nil))

(add-hook 'helm-cleanup-hook 'helm-reset-yank-point)
(add-hook 'helm-after-initialize-hook 'helm-reset-yank-point)

;;; Ansi
;;
;;
(defvar helm--ansi-color-regexp
  "\033\\[\\(K\\|[0-9;]*m\\)")
(defvar helm--ansi-color-drop-regexp
  "\033\\[\\([ABCDsuK]\\|[12][JK]\\|=[0-9]+[hI]\\|[0-9;]*[Hf]\\)")
(defun helm--ansi-color-apply (string)
  "A version of `ansi-color-apply' immune to upstream changes.

Similar to the emacs-24.5 version without support to `ansi-color-context'
which is buggy in emacs.

Modify also `ansi-color-regexp' by using own variable `helm--ansi-color-regexp'
that match whole STRING.

This is needed to provide compatibility for both emacs-25 and emacs-24.5
as emacs-25 version of `ansi-color-apply' is partially broken."
  (let ((start 0)
        codes end escape-sequence
        result colorized-substring)
    ;; Find the next escape sequence.
    (while (setq end (string-match helm--ansi-color-regexp string start))
      (setq escape-sequence (match-string 1 string))
      ;; Colorize the old block from start to end using old face.
      (when codes
        (put-text-property
         start end 'font-lock-face (ansi-color--find-face codes) string))
      (setq colorized-substring (substring string start end)
            start (match-end 0))
      ;; Eliminate unrecognized ANSI sequences.
      (while (string-match helm--ansi-color-drop-regexp colorized-substring)
        (setq colorized-substring
              (replace-match "" nil nil colorized-substring)))
      (push colorized-substring result)
      ;; Create new face, by applying escape sequence parameters.
      (setq codes (ansi-color-apply-sequence escape-sequence codes)))
    ;; If the rest of the string should have a face, put it there.
    (when codes
      (put-text-property
       start (length string)
       'font-lock-face (ansi-color--find-face codes) string))
    ;; Save the remainder of the string to the result.
    (if (string-match "\033" string start)
        (push (substring string start (match-beginning 0)) result)
        (push (substring string start) result))
    (apply 'concat (nreverse result))))

(provide 'helm-lib)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-lib ends here
