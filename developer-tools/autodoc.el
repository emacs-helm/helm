;;; autodoc.el --- Inline auto documentation of lisp sources files.

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Copyright (C) 2010 ~ 2012, Thierry Volpiatto, all rights reserved.

;; Compatibility: GNU Emacs 23/24+

;; This file is not part of GNU Emacs. 

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Autodoc documentation:
;;  ---------------------

;;  * Commands defined here are:
;; [EVAL] (autodoc-document-lisp-buffer :type 'command :prefix "autodoc" :docstring t)
;; `autodoc-insert-header'
;; Insert an auto documentation line of commented code to eval.

;;  * Macros defined here are:
;; [EVAL] (autodoc-document-lisp-buffer :type 'macro :prefix "autodoc" :docstring t)
;; `autodoc-document-lisp-buffer'
;; Auto document tool for lisp code.

;;  * Functions defined here are:
;; [EVAL] (autodoc-document-lisp-buffer :type 'function :prefix "autodoc" :docstring t)
;; `autodoc-find-readlines'
;; Return an alist of all the (num-line line) of a file or buffer BFILE matching REGEXP.
;; `autodoc-update-all'
;; Not documented.
;; `autodoc-document-default-prefix'
;; Return file name without extension as default prefix.
;; `autodoc-get-first-line-documentation'
;; Return first line documentation of symbol SYM.

;;  * User variables defined here
;; [EVAL] (autodoc-document-lisp-buffer :type 'internal-variable :prefix "autodoc" :var-value t)
;; `autodoc-truncate-long-lines'
;; Default Value: 80

;;  *** END auto-documentation

;;; Code:

(eval-when-compile (require 'cl))


(defsubst* autodoc-find-readlines (bfile regexp &key (insert-fn 'buffer))
  "Return an alist of all the (numline line)  matching REGEXP."
  (let ((fn (case insert-fn
              ('file 'insert-file-contents)
              ('buffer 'insert-buffer-substring))))
    (with-temp-buffer
      (funcall fn bfile) ; call insert function
      (goto-char (point-min))
      (loop
         with lines-list = (split-string (buffer-string) "\n")
         for i in lines-list for count from 0 when (string-match regexp i)
         collect (list count i) into lis
         finally return lis))))

(defvar autodoc-truncate-long-lines 80
  "* When set to nil don't truncate lines.")
(defmacro* autodoc-document-lisp-buffer (&key type prefix docstring any-sname var-value)
  "Auto document tool for lisp code.

TYPE can be one of:
    - command
    - nested-command
    - function
    - nested-function
    - macro
    - internal-variable
    - nested-variable
    - user-variable
    - faces
    - anything-source

PREFIX let you define a special name by regexp,
\(e.g match only function with PREFIX \"^autodoc-\")

Example: (autodoc-document-lisp-buffer :type 'function :prefix \"autodoc\").

It's better and more convenient to use `autodoc-insert-header'
to setup your headers. However if you write your headers manually,
don't forget to add this line at the end of your autodoc-documentation:

    ;;  *** END auto-documentation

DOCSTRING, if set to non--nil, will display first line of docstring.

Example: (autodoc-document-lisp-buffer :type 'function :prefix \"^autodoc-\" :docstring t).

See headers of traverselisp.el for example."
  `(let* ((boundary-regexp "^;; +\\*+ .*")
          (regexp          (case ,type
                             ('command           "^\(def\\(un\\|subst\\)")
                             ('nested-command    "^ +\(def\\(un\\|subst\\)")
                             ('function          "^\(def\\(un\\|subst\\|advice\\)")
                             ('nested-function   "^ +\(def\\(un\\|subst\\|advice\\)")
                             ('macro             "^\(defmacro")
                             ('internal-variable "^\(defvar")
                             ('nested-variable   "^ +\(defvar")
                             ('user-variable     "\(defcustom")
                             ('faces             "\(defface")
                             ('anything-source   "^\(defvar anything-c-source")
                             (t (error           "Unknow type"))))
          (fn-list         (autodoc-find-readlines
                            (current-buffer)
                            regexp
                            :insert-fn 'buffer))
          beg end)
     
     (flet ((maybe-insert-with-prefix (name)
              (let ((doc (or (autodoc-get-first-line-documentation (intern name))
                             "Not documented."))
                    (any-source-name (or (when (and ,any-sname (eq ,type 'anything-source))
                                           (concat
                                            " ("
                                            (assoc-default
                                             'name (symbol-value (intern name))) ")"))
                                         ""))
                    (var-def-value (or (when (and ,var-value (or (eq ,type 'internal-variable)
                                                                 (eq ,type 'nested-variable)
                                                                 (eq ,type 'user-variable)))
                                                  (concat "\n;; Default Value: "
                                                          (let ((it (replace-regexp-in-string
                                                                     "\n" " "
                                                                     (pp-to-string (symbol-value (intern name))))))
                                                            (if autodoc-truncate-long-lines
                                                                (truncate-string-to-width
                                                                 it
                                                                 autodoc-truncate-long-lines nil nil " [...]")
                                                              it))))
                                       "")))
                (cond ((and ,docstring ,prefix)
                       (when (string-match ,prefix name)
                         (insert (concat ";; \`" name "\'"
                                         var-def-value any-source-name "\n;; " doc "\n"))))
                      (,docstring
                       (insert (concat ";; \`" name "\'"
                                       var-def-value any-source-name "\n;; " doc "\n")))
                      (,prefix
                        (when (string-match ,prefix name)
                          (insert (concat ";; \`" name "\'"
                                          var-def-value any-source-name "\n"))))
                      (t (insert (concat ";; \`" name "\'"
                                         var-def-value any-source-name "\n")))))))
       (insert "\n") (setq beg (point))
       (save-excursion (when (re-search-forward boundary-regexp)
                         (forward-line -1) (setq end (point))))
       (delete-region beg end)
       (when (eq ,type 'anything-source) (setq regexp "\(defvar"))
       (dolist (i fn-list)
         (let* ((elm     (cadr i))
                (elm1    (replace-regexp-in-string "\*" "" elm))
                (elm-mod (replace-regexp-in-string regexp "" elm1))
                (elm-fin (replace-regexp-in-string "\(\\|\)" ""(car (split-string elm-mod)))))
           (case ,type
             ('command
              (when (commandp (intern elm-fin))
                (maybe-insert-with-prefix elm-fin)))
             ('nested-command
              (when (commandp (intern elm-fin))
                (maybe-insert-with-prefix elm-fin)))
             ('function
              (when (not (commandp (intern elm-fin)))
                (maybe-insert-with-prefix elm-fin)))
             ('nested-function
              (when (not (commandp (intern elm-fin)))
                (maybe-insert-with-prefix elm-fin)))
             ('internal-variable
              (unless (string-match "anything-c-source" elm-fin)
                (maybe-insert-with-prefix elm-fin)))
             (t
              (maybe-insert-with-prefix elm-fin)))))
       (setq end (point))
       (align-regexp beg end "\\(\\s-*\\)(" 1 1 nil))))

;;;###autoload
(defun autodoc-update-all ()
  "Eval all autodoc headers found."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "^;; +\\[EVAL\\]" nil t)
    (end-of-line)
    ;; Avoid infinite loop if one write an eval followed by autodoc-update-all.
    (unless (save-excursion (search-backward "autodoc-update-all" (point-at-bol) t))
      (eval-last-sexp t)
      (while (not (bolp)) (delete-char -1)))))


(defun autodoc-document-default-prefix ()
  "Return file name without extension as default prefix."
  (file-name-sans-extension (buffer-name (current-buffer))))


(defun autodoc-get-first-line-documentation (sym)
  "Return first line documentation of symbol SYM."
  (let ((doc (cond ((fboundp sym)
                    (documentation sym t))
                   ((boundp sym)
                    (documentation-property sym 'variable-documentation t))
                   ((facep sym)
                    (face-documentation sym))
                   (t nil))))
    (when doc
      (car (split-string doc "\n")))))

;;;###autoload
(defun autodoc-insert-header (title &optional docstring)
  "Insert an auto documentation line of commented code to eval.
With prefix arg insert also the docstring argument.
See headers of `autodoc.el' for example."
  (interactive "sTitle: \nP")
  (let* ((ttype      (anything-comp-read "Type: " '("command" "nested-command"
                                                    "function" "nested-function"
                                                    "macro" "internal-variable"
                                                    "user-variable" "nested-variable"
                                                    "faces" "anything-source")))
         (prefix     (read-string "Prefix: " (autodoc-document-default-prefix)))
         (prefix-arg (concat " :prefix " "\"" prefix "\"")))
    (insert (concat ";;  * " title "\n"
                    ";; [EVAL] (autodoc-document-lisp-buffer :type \'"
                    ttype
                    (unless (string= prefix "") prefix-arg)
                    (when (and (string= ttype "anything-source")
                               (y-or-n-p "Insert Anything Source Name?"))
                      " :any-sname t")
                    (when (and (or (string= ttype "user-variable")
                                   (string= ttype "nested-variable")
                                   (string= ttype "internal-variable"))
                               (y-or-n-p "Insert variable Default value?"))
                      " :var-value t")
                    (when docstring " :docstring t") ")")
            (if (save-excursion (search-forward "*** END auto-documentation" nil t))
                "" "\n\n\n;;  *** END auto-documentation"))))


;;; Provide
(provide 'autodoc)

;;; autodoc.el ends here.
