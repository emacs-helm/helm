;;; helm-plugin.el --- Helm plugins -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info.el" (filename nodename &optional no-going-back))


;;; Plug-in: `info-index'
;;
;;
(defvar Info-history)
(cl-defun helm-info-init (&optional (file (helm-attr 'info-file)))
  ;; Allow reinit candidate buffer when using edebug.
  (helm-aif (and debug-on-error
                 (helm-candidate-buffer))
      (kill-buffer it))
  (unless (helm-candidate-buffer)
    (save-window-excursion
      (info file)
      (let (Info-history
            (tobuf (helm-candidate-buffer 'global))
            (infobuf (current-buffer))
            s e
            (nodes (or (helm-attr 'index-nodes) (Info-index-nodes))))
        (cl-dolist (node nodes)
          (Info-goto-node node)
          (goto-char (point-min))
          (while (search-forward "\n* " nil t)
            (unless (search-forward "Menu:\n" (1+ (point-at-eol)) t)
              (save-current-buffer (buffer-substring-no-properties
                                    (point-at-bol) (point-at-eol)))
              (setq s (point-at-bol)
                    e (point-at-eol))
              (with-current-buffer tobuf
                (insert-buffer-substring infobuf s e)
                (insert "\n")))))))))

(defun helm-info-goto (node-line)
  (Info-goto-node (car node-line))
  (helm-goto-line (cdr node-line)))

(defun helm-info-display-to-real (line)
  (and (string-match
        ;; This regexp is stolen from Info-apropos-matches
        "\\* +\\([^\n]*.+[^\n]*\\):[ \t]+\\([^\n]*\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?" line)
       (cons (format "(%s)%s" (helm-attr 'info-file) (match-string 2 line))
             (string-to-number (or (match-string 3 line) "1")))))

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

(helm-document-attribute 'index-nodes "info-index plugin (optional)"
  "  Index nodes of info file.

  If it is omitted, `Info-index-nodes' is used to collect index
  nodes. Some info files are missing index specification.

  See `helm-source-info-screen'.")


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

(defun helm-display-to-real-numbered-line (candidate)
  "This is used to display a line in occur style in helm sources.
e.g \"    12:some_text\".
It is used with type attribute 'line'."
  (if (string-match "^ *\\([0-9]+\\):\\(.*\\)$" candidate)
      (list (string-to-number (match-string 1 candidate))
            (match-string 2 candidate))
    (error "Line number not found")))


;;; Type attributes
;;
;;
(define-helm-type-attribute 'line
    '((display-to-real . helm-display-to-real-numbered-line)
      (action ("Go to Line" . helm-action-line-goto)))
  "LINENO:CONTENT string, eg. \"  16:foo\".

Optional `target-file' attribute is a name of target file.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.")

(define-helm-type-attribute 'file-line
    `((filtered-candidate-transformer helm-filtered-candidate-transformer-file-line)
      (multiline)
      (action ("Go to" . helm-action-file-line-goto)))
  "FILENAME:LINENO:CONTENT string, eg. \"~/.emacs:16:;; comment\".

Optional `default-directory' attribute is a default-directory
FILENAME is interpreted.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.")


;;; Document new attributes
;;
;;
(helm-document-attribute 'persistent-help "persistent-help plug-in"
  "  A string to explain persistent-action of this source. It also
  accepts a function or a variable name.")

(helm-document-attribute 'default-directory "type . file-line"
  "  `default-directory' to interpret file.")

(helm-document-attribute 'before-jump-hook "type . file-line / line"
  "  Function to call before jumping to the target location.")

(helm-document-attribute 'after-jump-hook "type . file-line / line"
  "  Function to call after jumping to the target location.")

(helm-document-attribute 'adjust "type . file-line"
  "  Search around line matching line contents.")

(helm-document-attribute 'recenter "type . file-line / line"
  "  `recenter' after jumping.")

(helm-document-attribute 'target-file "type . line"
  "  Goto line of target-file.")

(provide 'helm-plugin)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-plugin ends here
