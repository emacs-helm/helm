;;; helm-lib.el --- Helm routines. -*- lexical-binding: t -*-

;; Copyright (C) 2015 ~ 2016  Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(defcustom helm-help-full-frame t
  "Display help window in full frame when non nil.

Even when `nil' probably the same result (full frame)
can be reach by tweaking `display-buffer-alist' but it is
much more convenient to use a simple boolean value here."
  :type 'boolean
  :group 'helm-help)


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


;;; Compatibility
;;
(defun helm-add-face-text-properties (beg end face &optional append object)
  "Add the face property to the text from START to END.
It is a compatibility function which behave exactly like
`add-face-text-property' if available otherwise like `add-text-properties'.
When only `add-text-properties' is available APPEND is ignored."
  (if (fboundp 'add-face-text-property)
      (add-face-text-property beg end face append object)
      (add-text-properties beg end `(face ,face) object)))

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

;;; Command loop helper
;;
(defun helm-this-command ()
  "Returns the actual command in action.
Like `this-command' but return the real command,
and not `exit-minibuffer' or other unwanted functions."
  (cl-loop with bl = '(helm-maybe-exit-minibuffer
                       helm-confirm-and-exit-minibuffer
                       helm-exit-minibuffer
                       exit-minibuffer)
           for count from 1 to 50
           for btf = (backtrace-frame count)
           for fn = (cl-second btf)
           if (and
               ;; In some case we may have in the way an
               ;; advice compiled resulting in byte-code,
               ;; ignore it (Issue #691).
               (symbolp fn)
               (commandp fn)
               (not (memq fn bl)))
           return fn
           else
           if (and (eq fn 'call-interactively)
                   (> (length btf) 2))
           return (cadr (cdr btf))))


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

;;; Anaphoric macros.
;;
(defmacro helm-aif (test-form then-form &rest else-forms)
  "Anaphoric version of `if'.
Like `if' but set the result of TEST-FORM in a temporary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro helm-awhile (sexp &rest body)
  "Anaphoric version of `while'.
Same usage as `while' except that SEXP is bound to
a temporary variable called `it' at each turn.
An implicit nil block is bound to the loop so usage
of `cl-return' is possible to exit the loop."
  (declare (indent 1) (debug t))
  (helm-with-gensyms (flag)
    `(let ((,flag t))
       (cl-block nil
         (while ,flag
           (helm-aif ,sexp
               (progn ,@body)
             (setq ,flag nil)))))))

(defmacro helm-acond (&rest clauses)
  "Anaphoric version of `cond'."
  (unless (null clauses)
    (helm-with-gensyms (sym)
      (let ((clause1 (car clauses)))
        `(let ((,sym ,(car clause1)))
           (helm-aif ,sym
               (progn ,@(cdr clause1))
             (helm-acond ,@(cdr clauses))))))))


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
           (when helm-help-full-frame (delete-other-windows))
           (delete-region (point-min) (point-max))
           (org-mode)
           (save-excursion
             (funcall insert-content-fn))
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

(defun helm-help-next-line ()
  (condition-case _err
      (call-interactively #'next-line)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun helm-help-previous-line ()
  (condition-case _err
      (call-interactively #'previous-line)
    (beginning-of-buffer nil)
    (end-of-buffer nil)))

(defun helm-help-toggle-mark ()
  (if (region-active-p)
      (deactivate-mark)
      (push-mark nil nil t)))

;; For movement of cursor in help buffer we need to call interactively
;; commands for impaired people using a synthetizer (#1347).
(defun helm-help-event-loop ()
  (let ((prompt (propertize
                 "[SPC,C-v,down,next:NextPage  b,M-v,up,prior:PrevPage C-s/r:Isearch q:Quit]"
                 'face 'helm-helper))
        scroll-error-top-bottom)
    (helm-awhile (read-key prompt)
      (cl-case it
        ((?\C-v ? down next) (helm-help-scroll-up helm-scroll-amount))
        ((?\M-v ?b up prior) (helm-help-scroll-down helm-scroll-amount))
        (?\C-s (isearch-forward))
        (?\C-r (isearch-backward))
        (?\C-a (call-interactively #'move-beginning-of-line))
        (?\C-e (call-interactively #'move-end-of-line))
        (?\C-f (call-interactively #'forward-char))
        (?\C-b (call-interactively #'backward-char))
        (?\C-n (helm-help-next-line))
        (?\C-p (helm-help-previous-line))
        (?\M-a (call-interactively #'backward-sentence))
        (?\M-e (call-interactively #'forward-sentence))
        (?\M-f (call-interactively #'forward-word))
        (?\M-b (call-interactively #'backward-word))
        (?\C-  (helm-help-toggle-mark))
        (?\M-w (copy-region-as-kill
                (region-beginning) (region-end))
               (deactivate-mark))
        (?q    (cl-return))
        (t     (ignore))))))


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
        collect (puthash elm elm cont)))

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

(defun helm-boring-directory-p (directory black-list)
  "Check if one regexp in BLACK-LIST match DIRECTORY."
  (helm-awhile (helm-basedir (directory-file-name
                              (expand-file-name directory)))
    (when (string= it "/") (cl-return nil))
    (when (cl-loop for r in black-list
                   thereis (string-match-p
                            r (directory-file-name directory)))
      (cl-return t))
    (setq directory it)))

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

(defun helm-append-at-nth (seq elm index)
  "Append ELM at INDEX in SEQ."
  (let ((len (length seq)))
    (cond ((> index len) (setq index len))
          ((< index 0) (setq index 0)))
    (if (zerop index)
        (append elm seq)
      (cl-loop for i in seq
               for count from 1 collect i
               when (= count index)
               if (listp elm) append elm
               else collect elm))))


;;; Strings processing.
;;
(defun helm-stringify (elm)
  "Return the representation of ELM as a string.
ELM can be a string, a number or a symbol."
  (cl-typecase elm
    (string elm)
    (number (number-to-string elm))
    (symbol (symbol-name elm))))

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

(defun helm-current-line-contents ()
  "Current line string without properties."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))


;;; Symbols routines
;;
(defun helm-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (if (symbolp str-or-sym)
      str-or-sym
    (intern str-or-sym)))

(defun helm-symbol-name (obj)
  (if (or (and (consp obj) (functionp obj))
          (byte-code-function-p obj))
      "Anonymous"
      (symbol-name obj)))

(defun helm-describe-function (func)
  "FUNC is symbol or string."
  (cl-letf (((symbol-function 'message) #'ignore))
    (describe-function (helm-symbolify func))))

(defun helm-describe-variable (var)
  "VAR is symbol or string."
  (cl-letf (((symbol-function 'message) #'ignore))
    (describe-variable (helm-symbolify var))))

(defun helm-describe-face (face)
  "FACE is symbol or string."
  (cl-letf (((symbol-function 'message) #'ignore))
    (describe-face (helm-symbolify face))))

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


;;; Modes
;;
(defun helm-same-major-mode-p (start-buffer alist)
  "Decide if current-buffer is related to START-BUFFER.
Argument ALIST is an alist of associated major modes."
  ;; START-BUFFER is the current-buffer where we start searching.
  ;; Determine the major-mode of START-BUFFER as `cur-maj-mode'.
  ;; Each time the loop go in another buffer we try from this buffer
  ;; to determine if its `major-mode' is:
  ;; - same as the `cur-maj-mode'
  ;; - derived from `cur-maj-mode' and from
  ;;   START-BUFFER if its mode is derived from the one in START-BUFFER. 
  ;; - have an assoc entry (major-mode . cur-maj-mode)
  ;; - have an rassoc entry (cur-maj-mode . major-mode)
  ;; - check if one of these entries inherit from another one in
  ;;   `alist'.
  (let* ((cur-maj-mode  (with-current-buffer start-buffer major-mode))
         (maj-mode      major-mode)
         (c-assoc-mode  (assq cur-maj-mode alist))
         (c-rassoc-mode (rassq cur-maj-mode alist))
         (o-assoc-mode  (assq major-mode alist))
         (o-rassoc-mode (rassq major-mode alist))
         (cdr-c-assoc-mode (cdr c-assoc-mode))
         (cdr-o-assoc-mode (cdr o-assoc-mode)))
    (or (eq major-mode cur-maj-mode)
        (derived-mode-p cur-maj-mode)
        (with-current-buffer start-buffer
          (derived-mode-p maj-mode))
        (or (eq cdr-c-assoc-mode major-mode)
            (eq (car c-rassoc-mode) major-mode)
            (eq (cdr (assq cdr-c-assoc-mode alist))
                major-mode)
            (eq (car (rassq cdr-c-assoc-mode alist))
                major-mode))
        (or (eq cdr-o-assoc-mode cur-maj-mode)
            (eq (car o-rassoc-mode) cur-maj-mode)
            (eq (cdr (assq cdr-o-assoc-mode alist))
                cur-maj-mode)
            (eq (car (rassq cdr-o-assoc-mode alist))
                cur-maj-mode)))))

;;; Files routines
;;
(defun helm-file-name-sans-extension (filename)
  "Same as `file-name-sans-extension' but remove all extensions."
  (helm-aif (file-name-sans-extension filename)
      ;; Start searching at index 1 for files beginning with a dot (#1335).
      (if (string-match "\\." (helm-basename it) 1)
          (helm-file-name-sans-extension it)
          it)))

(defun helm-basename (fname &optional ext)
  "Print FNAME  with any  leading directory  components removed.
If specified, also remove filename extension EXT.
Arg EXT can be specified as a string with or without dot,
in this case it should match file-name-extension.
It can also be non-nil (`t') in this case no checking
of file-name-extension is done and the extension is removed
unconditionally."
  (let ((non-essential t))
    (if (and ext (or (string= (file-name-extension fname) ext)
                     (string= (file-name-extension fname t) ext)
                     (eq ext t))
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
  (with-no-warnings
    (w32-shell-execute "open" (helm-w32-prepare-filename file))))

;; Same as `vc-directory-exclusion-list'.
(defvar helm-walk-ignore-directories
  '("SCCS/" "RCS/" "CVS/" "MCVS/" ".svn/" ".git/" ".hg/" ".bzr/"
    "_MTN/" "_darcs/" "{arch}/" ".gvfs/"))

(defsubst helm--dir-file-name (file dir)
  (expand-file-name
   (substring file 0 (1- (length file))) dir))

(defsubst helm--dir-name-p (str)
  (char-equal (aref str (1- (length str))) ?/))

(cl-defun helm-walk-directory (directory &key (path 'basename)
                                         directories
                                         match skip-subdirs)
  "Walk through DIRECTORY tree.

Argument PATH can be one of basename, relative, full, or a function
called on file name, default to basename.

Argument DIRECTORIES when non--nil (default) return also directories names,
otherwise skip directories names, with a value of 'only returns
only subdirectories, i.e files are skipped.

Argument MATCH is a regexp matching files or directories.

Argument SKIP-SUBDIRS when `t' will skip `helm-walk-ignore-directories'
otherwise if it is given as a list of directories, this list will be used
instead of `helm-walk-ignore-directories'."
  (let ((fn (cl-case path
               (basename 'file-name-nondirectory)
               (relative 'file-relative-name)
               (full     'identity)
               (t        path)))) ; A function.
    (setq skip-subdirs (if (listp skip-subdirs)
                           skip-subdirs
                           helm-walk-ignore-directories))
    (cl-labels ((ls-rec (dir)
                  (unless (file-symlink-p dir)
                    (cl-loop for f in (sort (file-name-all-completions "" dir)
                                            'string-lessp)
                             unless (member f '("./" "../"))
                             ;; A directory.
                             ;; Use `helm--dir-file-name' to remove the final slash.
                             ;; Needed to avoid infloop on directory symlinks.
                             if (and (helm--dir-name-p f)
                                     (helm--dir-file-name f dir))
                             nconc
                             (unless (member f skip-subdirs)
                               (if (and directories
                                        (or (null match)
                                            (string-match match f)))
                                   (nconc (list (concat (funcall fn it) "/"))
                                          (ls-rec it))
                                   (ls-rec it)))
                             ;; A regular file.
                             else nconc
                             (when (and (null (eq directories 'only))
                                        (or (null match) (string-match match f)))
                               (list (funcall fn (expand-file-name f dir))))))))
      (ls-rec directory))))

(defun helm-file-expand-wildcards (pattern &optional full)
  "Same as `file-expand-wildcards' but allow recursion.
Recursion happen when PATTERN starts with two stars.
Directories expansion is not supported."
  (let ((bn (helm-basename pattern))
        (case-fold-search nil))
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
