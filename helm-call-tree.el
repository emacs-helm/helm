;;; helm-call-tree.el --- Helm interface of `simple-call-tree.el'.

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

;; <http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el>

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)

(declare-function simple-call-tree-analyze "ext:simple-call-tree.el" (&optional test))

;; Function is called by
(defvar helm-c-source-simple-call-tree-functions-callers
  '((name . "Function is called by")
    (init . helm-c-simple-call-tree-functions-callers-init)
    (multiline)
    (candidates . helm-c-simple-call-tree-candidates)
    (persistent-action . helm-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             helm-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defvar helm-c-simple-call-tree-tick nil)
(make-variable-buffer-local 'helm-c-simple-call-tree-tick)
(defun helm-c-simple-call-tree-analyze-maybe ()
  (unless (eq (buffer-chars-modified-tick) helm-c-simple-call-tree-tick)
    (simple-call-tree-analyze)
    (setq helm-c-simple-call-tree-tick (buffer-chars-modified-tick))))

(defun helm-c-simple-call-tree-init-base (function message)
  (require 'simple-call-tree)
  (with-no-warnings
    (when (helm-current-buffer-is-modified)
      (helm-c-simple-call-tree-analyze-maybe)
      (let ((list (funcall function simple-call-tree-alist)))
        (with-current-buffer (helm-candidate-buffer 'local)
          (dolist (entry list)
            (let ((funcs (concat "  " (mapconcat #'identity (cdr entry) "\n  "))))
              (insert (car entry) message
                      (if (string= funcs "  ")
                          "  no functions."
                          funcs)
                      "\n\n"))))))))

(defun helm-c-simple-call-tree-functions-callers-init ()
  (helm-c-simple-call-tree-init-base 'simple-call-tree-invert
                                     " is called by\n"))

(defun helm-c-simple-call-tree-candidates ()
  (with-current-buffer (helm-candidate-buffer)
    (split-string (buffer-string) "\n\n")))

(defvar helm-c-simple-call-tree-related-functions nil)
(defvar helm-c-simple-call-tree-function-index 0)
(defun helm-c-simple-call-tree-persistent-action (candidate)
  (unless (eq last-command 'helm-execute-persistent-action)
    (setq helm-c-simple-call-tree-related-functions
          (delete "no functions."
                  (split-string
                   (replace-regexp-in-string "  \\| is called by\\| calls "
                                             "" candidate)
                   "\n")))
    (setq helm-c-simple-call-tree-function-index -1))
  (incf helm-c-simple-call-tree-function-index)
  (helm-c-simple-call-tree-find-definition candidate))

(defun helm-c-simple-call-tree-find-definition (candidate)
  (find-function
   (intern
    (nth (mod helm-c-simple-call-tree-function-index
              (length helm-c-simple-call-tree-related-functions))
         helm-c-simple-call-tree-related-functions))))


;;; Function calls
(defvar helm-c-source-simple-call-tree-callers-functions
  '((name . "Function calls")
    (init . helm-c-simple-call-tree-callers-functions-init)
    (multiline)
    (candidates . helm-c-simple-call-tree-candidates)
    (persistent-action . helm-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             helm-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defun helm-c-simple-call-tree-callers-functions-init ()
  (helm-c-simple-call-tree-init-base 'identity " calls \n"))

;;;###autoload
(defun helm-simple-call-tree ()
  "Preconfigured `helm' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el"
  (interactive)
  (helm-other-buffer
   '(helm-c-source-simple-call-tree-functions-callers
     helm-c-source-simple-call-tree-callers-functions)
   "*helm simple-call-tree*"))

(provide 'helm-call-tree)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-call-tree.el ends here
