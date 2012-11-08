;;; helm-info.el --- Browse info index with helm

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

(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-plugin)
(require 'helm-net)

(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info.el" (filename nodename &optional no-going-back))


(defgroup helm-info nil
  "Info related Applications and libraries for Helm."
  :group 'helm)

;;; Build info-index sources with info-index plug-in.
;;
;;
(defun helm-c-build-info-index-command (name doc source buffer)
  "Define an helm command NAME with documentation DOC.
Arg SOURCE will be an existing helm source named
`helm-c-source-info-<NAME>' and BUFFER a string buffer name."
  (eval (list 'defun name nil doc
              (list 'interactive)
              (list 'helm
                    :sources source
                    :buffer buffer
                    :candidate-number-limit 1000))))

(defun helm-c-define-info-index-sources (var-value &optional commands)
  "Define helm sources named helm-c-source-info-<NAME>.
Sources are generated for all entries of `helm-c-default-info-index-list'.
If COMMANDS arg is non--nil build also commands named `helm-info<NAME>'.
Where NAME is one of `helm-c-default-info-index-list'."
  (loop with symbols = (loop for str in var-value
                             collect
                             (intern (concat "helm-c-source-info-" str)))
        for sym in symbols
        for str in var-value
        do (set sym (list (cons 'name (format "Info index: %s" str))
                          (cons 'info-index str)))
        when commands
        do (let ((com (intern (concat "helm-info-" str))))
             (helm-c-build-info-index-command
              com (format "Predefined helm for %s info." str)
              sym (format "*helm info %s*" str)))))

(defun helm-info-index-set (var value)
  (set var value)
  (helm-c-define-info-index-sources value t))

(defcustom helm-c-default-info-index-list
  '("elisp" "cl" "org" "gnus" "tramp" "ratpoison"
    "zsh" "bash" "coreutils" "fileutils"
    "find" "sh-utils" "textutils" "libc"
    "make" "automake" "autoconf" "emacs-lisp-intro"
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


;;; Info pages
(defvar helm-c-info-pages nil
  "All info pages on system.
Will be calculated the first time you invoke helm with this
source.")

(defun helm-c-info-pages-init ()
  "Collect candidates for initial Info node Top."
  (if helm-c-info-pages
      helm-c-info-pages
      (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
            topics)
        (require 'info)
        (with-temp-buffer
          (Info-find-node "dir" "top")
          (goto-char (point-min))
          (while (re-search-forward info-topic-regexp nil t)
            (push (match-string-no-properties 1) topics))
          (kill-buffer))
        (setq helm-c-info-pages topics))))

(defvar helm-c-source-info-pages
  `((name . "Info Pages")
    (init . helm-c-info-pages-init)
    (candidates . helm-c-info-pages)
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string
                                           "^[^:]+: " "" node-str))))))
    (requires-pattern . 2)))

;;;###autoload
(defun helm-info-at-point (arg)
  "Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point."
  (interactive "P")
  (let ((helm-c-google-suggest-default-function
         'helm-c-google-suggest-emacs-lisp))
    (helm :sources '(helm-c-source-info-elisp
                     helm-c-source-info-cl
                     helm-c-source-info-pages
                     helm-c-source-google-suggest)
          :input (and arg (thing-at-point 'symbol))
          :buffer "*helm info*")))

(provide 'helm-info)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-info.el ends here
