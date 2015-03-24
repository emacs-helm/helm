;;; helm-info.el --- Browse info index with helm -*- lexical-binding: t -*-

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
(require 'helm-plugin)

(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info.el" (filename nodename &optional no-going-back))
(defvar Info-history)


(defgroup helm-info nil
  "Info related Applications and libraries for Helm."
  :group 'helm)

;;; Build info-index sources with `helm-info-source' class.
;;
;;
(cl-defun helm-info-init (&optional (file (helm-attr 'info-file)))
  ;; Allow reinit candidate buffer when using edebug.
  (helm-aif (and debug-on-error
                 (helm-candidate-buffer))
      (kill-buffer it))
  (unless (helm-candidate-buffer)
    (save-window-excursion
      (info file)
      (let ((tobuf (helm-candidate-buffer 'global))
            (infobuf (current-buffer))
            Info-history
            start end)
        (cl-dolist (node (Info-index-nodes))
          (Info-goto-node node)
          (goto-char (point-min))
          (while (search-forward "\n* " nil t)
            (unless (search-forward "Menu:\n" (1+ (point-at-eol)) t)
              (setq start (point-at-bol)
                    end (point-at-eol))
              (with-current-buffer tobuf
                (insert-buffer-substring infobuf start end)
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

(defclass helm-info-source (helm-source-in-buffer)
  ((info-file :initarg :info-file
              :initform nil
              :custom 'string)
   (init :initform #'helm-info-init)
   (display-to-real :initform #'helm-info-display-to-real)
   (get-line :initform #'buffer-substring)
   (action :initform '(("Goto node" . helm-info-goto)))))

(defmacro helm-build-info-source (fname &rest args)
  `(helm-make-source (concat "Info Index: " ,fname) 'helm-info-source
     :info-file ,fname ,@args))

(defun helm-build-info-index-command (name doc source buffer)
  "Define an helm command NAME with documentation DOC.
Arg SOURCE will be an existing helm source named
`helm-source-info-<NAME>' and BUFFER a string buffer name."
  (defalias (intern (concat "helm-info-" name))
      (lambda ()
        (interactive)
        (helm :sources source
              :buffer buffer
              :candidate-number-limit 1000))
    doc))

(defun helm-define-info-index-sources (var-value &optional commands)
  "Define helm sources named helm-source-info-<NAME>.
Sources are generated for all entries of `helm-default-info-index-list'.
If COMMANDS arg is non--nil build also commands named `helm-info<NAME>'.
Where NAME is one of `helm-default-info-index-list'."
  (cl-loop for str in var-value
           for sym = (intern (concat "helm-source-info-" str))
           do (set sym (helm-build-info-source str))
           when commands
           do (helm-build-info-index-command
               str (format "Predefined helm for %s info." str)
               sym (format "*helm info %s*" str))))

(defun helm-info-index-set (var value)
  (set var value)
  (helm-define-info-index-sources value t))

(defcustom helm-default-info-index-list
  '("elisp" "cl" "org" "gnus" "tramp" "ratpoison"
    "zsh" "bash" "coreutils" "fileutils"
    "find" "sh-utils" "textutils" "libc"
    "make" "automake" "autoconf" "eintr"
    "emacs" "elib" "eieio" "gauche-refe" "guile"
    "guile-tut" "goops" "screen" "latex" "gawk"
    "sed" "m4" "wget" "binutils" "as" "bfd" "gprof"
    "ld" "diff" "flex" "grep" "gzip" "libtool"
    "texinfo" "info" "gdb" "stabs" "cvsbook" "cvs"
    "bison" "id-utils" "global")
  "Info Manual entries to use for building helm info index commands."
  :group 'helm-info
  :type  '(repeat (choice string))
  :set   'helm-info-index-set)

(defcustom helm-info-default-sources
  '(helm-source-info-elisp
    helm-source-info-cl
    helm-source-info-eieio
    helm-source-info-pages)
  "The default sources to use in `helm-info-at-point'."
  :group 'helm-info
  :type '(repeat (choice symbol)))


;;; Info pages
(defvar helm-info--pages-cache nil
  "Cache for all info pages on system.")

(defun helm-info-pages-init ()
  "Collect candidates for initial Info node Top."
  (if helm-info--pages-cache
      helm-info--pages-cache
    (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
          topics)
      (require 'info)
      (with-temp-buffer
        (Info-find-node "dir" "top")
        (goto-char (point-min))
        (while (re-search-forward info-topic-regexp nil t)
          (push (match-string-no-properties 1) topics))
        (kill-buffer))
      (setq helm-info--pages-cache topics))))

(defvar helm-source-info-pages
  (helm-build-sync-source "Info Pages"
    :init #'helm-info-pages-init
    :candidates (lambda () helm-info--pages-cache)
    :action '(("Show with Info" .(lambda (node-str)
                                  (info (replace-regexp-in-string
                                         "^[^:]+: " "" node-str)))))
    :requires-pattern 2))

;;;###autoload
(defun helm-info-at-point ()
  "Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point."
  (interactive)
  (helm :sources helm-info-default-sources
        :buffer "*helm info*"))

(provide 'helm-info)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-info.el ends here
