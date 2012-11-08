;;; helm-plugin.el --- Helm plugins

;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:

(require 'cl)
(require 'helm)

(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info.el" (filename nodename &optional no-going-back))

;;; Plug-in: `info-index'
;;
;;
(defun* helm-c-info-init (&optional (file (helm-attr 'info-file)))
  (let (result)
    (unless (helm-candidate-buffer)
      (save-window-excursion
        (info file)
        (let (Info-history
              (tobuf (helm-candidate-buffer 'global))
              (infobuf (current-buffer))
              s e)
          (dolist (node (or (helm-attr 'index-nodes) (Info-index-nodes)))
            (Info-goto-node node)
            (goto-char (point-min))
            (while (search-forward "\n* " nil t)
              (unless (search-forward "Menu:\n" (1+ (point-at-eol)) t)
                '(save-current-buffer (buffer-substring-no-properties (point-at-bol) (point-at-eol)) result)
                (setq s (point-at-bol)
                      e (point-at-eol))
                (with-current-buffer tobuf
                  (insert-buffer-substring infobuf s e)
                  (insert "\n"))))))))))

(defun helm-c-info-goto (node-line)
  (Info-goto-node (car node-line))
  (helm-goto-line (cdr node-line)))

(defun helm-c-info-display-to-real (line)
  (and (string-match
        ;; This regexp is stolen from Info-apropos-matches
        "\\* +\\([^\n]*.+[^\n]*\\):[ \t]+\\([^\n]*\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?" line)
       (cons (format "(%s)%s" (helm-attr 'info-file) (match-string 2 line))
             (string-to-number (or (match-string 3 line) "1")))))

(defun helm-c-make-info-source (source file)
  `(,@source
    (name . ,(concat "Info Index: " file))
    (info-file . ,file)
    (init . helm-c-info-init)
    (display-to-real . helm-c-info-display-to-real)
    (get-line . buffer-substring)
    (candidates-in-buffer)
    (action ("Goto node" . helm-c-info-goto))))

(defun helm-compile-source--info-index (source)
  (helm-aif (helm-interpret-value (assoc-default 'info-index source))
      (helm-c-make-info-source source it)
    source))

(add-to-list 'helm-compile-source-functions 'helm-compile-source--info-index)

(helm-document-attribute 'info-index "info-index plugin"
  "Create a source of info index very easily.

ex. (defvar helm-c-source-info-wget '((info-index . \"wget\"))")

(helm-document-attribute 'index-nodes "info-index plugin (optional)"
  "Index nodes of info file.

If it is omitted, `Info-index-nodes' is used to collect index nodes.
Some info files are missing index specification.

ex. See `helm-c-source-info-screen'.")

;;; Plug-in: `candidates-file'
;;
;; List all lines in a file.
(defun helm-compile-source--candidates-file (source)
  (if (assoc-default 'candidates-file source)
      `((init helm-p-candidates-file-init
              ,@(let ((orig-init (assoc-default 'init source)))
                     (cond ((null orig-init) nil)
                           ((functionp orig-init) (list orig-init))
                           (t orig-init))))
        (candidates-in-buffer)
        ,@source)
      source))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--candidates-file)

(defun helm-p-candidates-file-init ()
  (destructuring-bind (file &optional updating)
      (helm-mklist (helm-attr 'candidates-file))
    (setq file (helm-interpret-value file))
    (with-current-buffer (helm-candidate-buffer (find-file-noselect file))
      (when updating
        (buffer-disable-undo)
        (font-lock-mode -1)
        (auto-revert-mode 1)))))

(helm-document-attribute 'candidates-file "candidates-file plugin"
  "Use a file as the candidates buffer.

1st argument is a filename, string or function name or variable name.
If optional 2nd argument is non-nil, the file is opened with
`auto-revert-mode' enabled.

e.g

\(defvar helm-c-source-test-file
  '((name . \"test1\")
    (candidates-file \"~/.emacs.el\" t)))

Will list all lines in .emacs.el.")

;;; Plug-in: `headline'
;;
;;
;; Le Wang: Note on how `helm-head-line-get-candidates' works with a list
;; of regexps.
;;
;;   1. Create list of ((title . start-of-match) . hiearchy)
;;   2. Sort this list by start-of-match.
;;   3. Go through sorted list and return titles that reflect full hiearchy.
;;
;; It's quite brilliantly written.
;;
(defun helm-compile-source--helm-headline (source)
  (if (assoc-default 'headline source)
      (append '((init . helm-headline-init)
                (get-line . buffer-substring)
                (type . line))
              source
              '((candidates-in-buffer)
                (persistent-help . "Show this line")))
      source))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--helm-headline)

(defun helm-headline-init ()
  (when (and (helm-current-buffer-is-modified)
             (with-helm-current-buffer
               (eval (or (helm-attr 'condition) t))))
    (helm-headline-make-candidate-buffer
     (helm-interpret-value (helm-attr 'headline))
     (helm-interpret-value (helm-attr 'subexp)))))

(helm-document-attribute 'headline "Headline plug-in"
  "Regexp string for helm-headline to scan.")
(helm-document-attribute 'condition "Headline plug-in"
  "A sexp representing the condition to use helm-headline.")
(helm-document-attribute 'subexp "Headline plug-in"
  "Display (match-string-no-properties subexp).")

(defun helm-headline-get-candidates (regexp subexp)
  (with-helm-current-buffer
    (save-excursion
      (goto-char (point-min))
      (if (functionp regexp) (setq regexp (funcall regexp)))
      (let (hierarchy curhead)
        (flet ((matched ()
                 (if (numberp subexp)
                     (cons (match-string-no-properties subexp) (match-beginning subexp))
                     (cons (buffer-substring (point-at-bol) (point-at-eol))
                           (point-at-bol))))
               (hierarchies (headlines)
                 (1+ (loop for (_ . hierarchy) in headlines
                           maximize hierarchy)))
               (vector-0-n (v n)
                 (loop for i from 0 to hierarchy
                       collecting (aref curhead i)))
               (arrange (headlines)
                 (unless (null headlines) ; FIX headlines empty bug!
                   (loop with curhead = (make-vector (hierarchies headlines) "")
                         for ((str . pt) . hierarchy) in headlines
                         do (aset curhead hierarchy str)
                         collecting
                         (cons
                          (format "H%d:%s" (1+ hierarchy)
                                  (mapconcat 'identity (vector-0-n curhead hierarchy) " / "))
                          pt)))))
          (if (listp regexp)
              (arrange
               (sort
                (loop for re in regexp
                      for hierarchy from 0
                      do (goto-char (point-min))
                      appending
                      (loop
                            while (re-search-forward re nil t)
                            collect (cons (matched) hierarchy)))
                (lambda (a b) (> (cdar b) (cdar a)))))
              (loop while (re-search-forward regexp nil t)
                    collect (matched))))))))


(defun helm-headline-make-candidate-buffer (regexp subexp)
  (with-current-buffer (helm-candidate-buffer 'local)
    (loop for (content . pos) in (helm-headline-get-candidates regexp subexp)
          do (insert
              (format "%5d:%s\n"
                      (with-helm-current-buffer
                        (line-number-at-pos pos))
                      content)))))

(defun helm-headline-goto-position (pos recenter)
  (goto-char pos)
  (unless recenter
    (set-window-start (get-buffer-window helm-current-buffer) (point))))


;;; Plug-in: `persistent-help'
;;
;; Add help about persistent action in `helm-buffer' header.
(defun helm-compile-source--persistent-help (source)
  (append source '((header-line . helm-persistent-help-string))))
(add-to-list 'helm-compile-source-functions 'helm-compile-source--persistent-help)

(defun helm-persistent-help-string ()
  (substitute-command-keys
   (concat "\\<helm-map>\\[helm-execute-persistent-action]: "
           (or (helm-interpret-value (helm-attr 'persistent-help))
               (helm-aif (or (assoc-default
                              'persistent-action
                              (helm-get-current-source))
                             (assoc-default
                              'action (helm-get-current-source)))
                   (cond ((symbolp it)
                          (symbol-name it))
                         ((listp it)
                          (or (ignore-errors (caar it))  ""))))
               "")
           " (keeping session)")))

(helm-document-attribute 'persistent-help "persistent-help plug-in"
  "A string to explain persistent-action of this source.
It also accepts a function or a variable name.")


;;; Plug-in: Type `customize'
;;
;;
(defvar helm-additional-type-attributes nil)

(defun helm-c-uniq-list (lst)
  "Like `remove-duplicates' in CL.
But cut deeper duplicates and test by `equal'. "
  (reverse (remove-duplicates (reverse lst) :test 'equal)))

(defun helm-c-arrange-type-attribute (type spec)
  "Override type attributes by `define-helm-type-attribute'.

The SPEC is like source. The symbol `REST' is replaced
with original attribute value.

 Example: Set `play-sound-file' as default action
   (helm-c-arrange-type-attribute 'file
      '((action (\"Play sound\" . play-sound-file)
         REST ;; Rest of actions (find-file, find-file-other-window, etc...)."
  (add-to-list 'helm-additional-type-attributes
               (cons type
                     (loop with typeattr = (assoc-default
                                            type helm-type-attributes)
                           for (attr . value) in spec
                           if (listp value)
                           collect (cons attr
                                         (helm-c-uniq-list
                                          (loop for v in value
                                                if (eq v 'REST)
                                                append
                                                (assoc-default attr typeattr)
                                                else
                                                collect v)))
                           else
                           collect (cons attr value)))))
(put 'helm-c-arrange-type-attribute 'lisp-indent-function 1)

(defun helm-compile-source--type-customize (source)
  (helm-aif (assoc-default (assoc-default 'type source)
                           helm-additional-type-attributes)
      (append it source)
    source))

(add-to-list 'helm-compile-source-functions
             'helm-compile-source--type-customize t)

;;; Plug-in: `default-action'
;;
;;
(defun helm-compile-source--default-action (source)
  (helm-aif (assoc-default 'default-action source)
      (append `((action ,it ,@(remove it (assoc-default 'action source))))
              source)
    source))
(add-to-list 'helm-compile-source-functions
             'helm-compile-source--default-action t)

(helm-document-attribute 'default-action "default-action plug-in"
  "Default action.")
(helm-document-attribute 'default-directory "type . file-line"
  "`default-directory' to interpret file.")
(helm-document-attribute 'before-jump-hook "type . file-line / line"
  "Function to call before jumping to the target location.")
(helm-document-attribute 'after-jump-hook "type . file-line / line"
  "Function to call after jumping to the target location.")
(helm-document-attribute 'adjust "type . file-line"
  "Search around line matching line contents.")
(helm-document-attribute 'recenter "type . file-line / line"
  "`recenter' after jumping.")
(helm-document-attribute 'target-file "type . line"
  "Goto line of target-file.")

(provide 'helm-plugin)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-plugin ends here
