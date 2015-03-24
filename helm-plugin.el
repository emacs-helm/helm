;;; helm-plugin.el --- Helm plugins -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(require 'cl-lib)
(require 'helm)
(require 'helm-utils)


;;; Plug-in: `info-index'
;;
;;
(defun helm-make-info-source (source file)
  `(,@source
    (name . ,(concat "Info Index: " file))
    (info-file . ,file)
    (init . helm-info-init)
    (display-to-real . helm-info-display-to-real)
    (get-line . buffer-substring)
    (candidates-in-buffer)
    (action ("Goto node" . helm-info-goto))))

(defun helm-compile-source--info-index (source)
  (helm-aif (helm-interpret-value (assoc-default 'info-index source))
      (helm-make-info-source source it)
    source))

(add-to-list 'helm-compile-source-functions 'helm-compile-source--info-index)

(helm-document-attribute 'info-index "info-index plugin"
  "  Create a source of info index very easily.

  Example:

  (defvar helm-source-info-wget '((info-index . \"wget\"))")


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
  (cl-destructuring-bind (file &optional updating)
      (helm-mklist (helm-attr 'candidates-file))
    (setq file (helm-interpret-value file))
    (with-current-buffer (helm-candidate-buffer 'global)
      (insert-file-contents file)
      (when updating
        (buffer-disable-undo)
        (font-lock-mode -1)
        (auto-revert-mode 1)))))

(helm-document-attribute 'candidates-file "candidates-file plugin"
  "  Use a file as the candidates buffer.

  1st argument is a filename, string or function name or variable
  name. If optional 2nd argument is non-nil, the file is opened with
  `auto-revert-mode' enabled.

  Example:

  \(defvar helm-source-test-file
    '((name . \"test1\")
      (candidates-file \"~/.emacs.el\" t)))

  Will list all lines in .emacs.el.")


;;; Plug-in: `persistent-help'
;;
;; Add help about persistent action in `helm-buffer' header.
(defun helm-compile-source--persistent-help (source)
  (if (assoc 'header-line source)
      source
      (append source '((header-line . helm-persistent-help-string)))))
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


;;; Document new attributes
;;
;;
(helm-document-attribute 'persistent-help "persistent-help plug-in"
  "  A string to explain persistent-action of this source. It also
  accepts a function or a variable name.")


(provide 'helm-plugin)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-plugin ends here
