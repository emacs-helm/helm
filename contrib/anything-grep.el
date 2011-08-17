;;; anything-grep.el --- search refinement of grep result with anything
;; $Id: anything-grep.el,v 1.27 2010-03-21 11:31:04 rubikitch Exp $

;; Copyright (C) 2008, 2009, 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: convenience, unix
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-grep.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Do grep in anything buffer. When we search information with grep,
;; we often narrow the candidates. Let's use `anything' to do it.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-grep'
;;    Run grep in `anything' buffer to narrow results.
;;  `anything-grep-by-name'
;;    Do `anything-grep' from predefined location.
;;  `anything-grep-by-name-reversed'
;;    Do `anything-grep' from predefined location.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; `anything-grep' is simple interface to grep a query. It asks
;; directory to grep. The grep process is synchronous process. You may
;; have to wait when you grep the target for the first time. But once
;; the target is on the disk cache, queries are grepped at lightning
;; speed. Even if older Pentium4 computer, grepping from 180MB takes
;; only 0.2s! GNU grep is amazingly fast.

;; `anything-grep-by-name' asks query and predefined location. It is
;; good idea to have ack (ack-grep), grep implemented in Perl, to
;; exclude unneeded files. Such as RCS, .svn and so on.

;; ack -- better than grep, a power search tool for programmers
;;  http://petdance.com/ack/


   
;;; History:

;; $Log: anything-grep.el,v $
;; Revision 1.27  2010-03-21 11:31:04  rubikitch
;; Resume bug fix
;;
;; Revision 1.26  2010/03/21 11:13:30  rubikitch
;; `anything-grep' works asynchronously
;;
;; Revision 1.25  2010/03/21 06:34:25  rubikitch
;; New function: `anything-grep-by-name-reversed'
;;
;; Revision 1.24  2010/03/21 06:28:42  rubikitch
;; update copyright
;;
;; Revision 1.23  2010/03/21 06:28:32  rubikitch
;; refactoring
;;
;; Revision 1.22  2009/12/28 08:56:56  rubikitch
;; `anything-grep-by-name': INCOMPATIBLE!!! swap optional arguments
;; `anything-grep-by-name' can utilize `repeat-complex-command'.
;;
;; Revision 1.21  2009/12/18 11:01:11  rubikitch
;; `agrep-real-to-display': erase "nil" message
;;
;; Revision 1.20  2009/06/25 03:36:38  rubikitch
;; `agrep-real-to-display': avoid error
;; auto-document
;;
;; Revision 1.19  2009/02/03 21:06:49  rubikitch
;; fontify file name and line number.
;; New variable: `anything-grep-fontify-file-name'
;;
;; Revision 1.18  2009/02/03 20:48:12  rubikitch
;; multi-line support.
;; New variable: `anything-grep-multiline'
;;
;; Revision 1.17  2009/02/03 20:35:03  rubikitch
;; Use `anything-quit-if-no-candidate' not to open *anything* buffer when no matches found.
;;
;; Revision 1.16  2009/01/20 09:56:19  rubikitch
;; New variable: `anything-grep-filter-command'
;;
;; Revision 1.15  2009/01/03 07:04:30  rubikitch
;; copyright
;;
;; Revision 1.14  2009/01/02 16:00:07  rubikitch
;; * Fixed invalid value of `anything-grep-alist'.
;; * Implemented functionality to search all buffers with `buffer-file-name'.
;;   See `anything-grep-alist'.
;;
;; Revision 1.13  2008/12/29 09:43:59  rubikitch
;; Rename variables:
;;  `agrep-goto-hook' => `anything-grep-goto-hook'
;;  `agrep-find-file-function' => `anything-grep-find-file-function'
;;
;; Revision 1.12  2008/12/29 09:40:23  rubikitch
;; document
;;
;; Revision 1.11  2008/12/29 07:58:37  rubikitch
;; refactoring
;;
;; Revision 1.10  2008/10/21 18:02:02  rubikitch
;; use *anything grep* buffer instead.
;;
;; Revision 1.9  2008/10/12 17:17:23  rubikitch
;; `anything-grep-by-name': swapped query order
;;
;; Revision 1.8  2008/10/09 00:33:40  rubikitch
;; New variable: `anything-grep-save-buffers-before-grep'
;;
;; Revision 1.7  2008/10/09 00:26:00  rubikitch
;; `anything-grep-by-name': nil argument
;;
;; Revision 1.6  2008/10/05 15:43:09  rubikitch
;; changed spec: `anything-grep-alist'
;;
;; Revision 1.5  2008/10/02 18:27:55  rubikitch
;; Use original fontify code instead of font-lock.
;; New variable: `agrep-find-file-function'
;;
;; Revision 1.4  2008/10/01 18:18:18  rubikitch
;; use ack-grep command to select files for search.
;;
;; Revision 1.3  2008/10/01 17:18:59  rubikitch
;; silence byte compiler
;;
;; Revision 1.2  2008/10/01 17:17:59  rubikitch
;; many bug fix
;; New command: `anything-grep-by-name'
;;
;; Revision 1.1  2008/10/01 10:58:59  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-grep-version "$Id: anything-grep.el,v 1.27 2010-03-21 11:31:04 rubikitch Exp $")
(require 'anything)
(require 'grep)

(defvar anything-grep-save-buffers-before-grep nil
  "Do `save-some-buffers' before performing `anything-grep'.")

(defvar anything-grep-goto-hook nil
  "List of functions to be called after `agrep-goto' opens file.")

(defvar anything-grep-find-file-function 'find-file
  "Function to visit a file with.
It takes one argument, a file name to visit.")

(defvar anything-grep-multiline t
  "If non-nil, use multi-line display. It is prettier.
Use anything.el v1.147 or newer.")

(defvar anything-grep-fontify-file-name t
  "If non-nil, fontify file name and line number of matches.")

(defvar anything-grep-alist
  '(("buffers" ("egrep -Hin %s $buffers" "/"))
    ("memo" ("ack-grep -af | xargs egrep -Hin %s" "~/memo"))
    ("PostgreSQL" ("egrep -Hin %s *.txt" "~/doc/postgresql-74/"))
    ("~/bin and ~/ruby"
     ("ack-grep -afG 'rb$' | xargs egrep -Hin %s" "~/ruby")
     ("ack-grep -af | xargs egrep -Hin %s" "~/bin")))
  "Mapping of location and command/pwd used by `anything-grep-by-name'.
The command is grep command line. Note that %s is replaced by query.
The command is typically \"ack-grep -af | xargs egrep -Hin %s\", which means
regexp/case-insensitive search for all files (including subdirectories)
except unneeded files.
The occurrence of $file in command is replaced with `buffer-file-name' of
all buffers.

The pwd is current directory to grep.

The format is:

  ((LOCATION1
     (COMMAND1-1 PWD1-1)
     (COMMAND1-2 PWD1-2)
     ...)
   (LOCATION2
     (COMMAND2-1 PWD2-1)
     (COMMAND2-2 PWD2-2)
     ...)
   ...)
")

(defvar anything-grep-filter-command nil
  "If non-nil, filter the result of grep command.

For example, normalizing many Japanese encodings to EUC-JP,
set this variable to \"ruby -rkconv -pe '$_.replace $_.toeuc'\".
The command is converting standard input to EUC-JP line by line. ")


;; (@* "core")
(defvar anything-grep-sources nil
  "`anything-sources' for last invoked `anything-grep'.")
(defvar anything-grep-buffer-name nil)
(defun anything-grep-base (sources &optional bufname)
  "Invoke `anything' for `anything-grep'."
  (and anything-grep-save-buffers-before-grep
       (save-some-buffers (not compilation-ask-about-save) nil))
  (setq anything-grep-sources sources)
  (setq anything-grep-buffer-name (or bufname "*anything grep*"))
  (let ((anything-quit-if-no-candidate t)
        (anything-compile-source-functions
         (cons 'anything-compile-source--agrep-init anything-compile-source-functions)))
    (anything sources nil nil nil nil bufname)))

;; (anything (list (agrep-source "grep -Hin agrep anything-grep.el" default-directory) (agrep-source "grep -Hin pwd anything-grep.el" default-directory)))

(defun agrep-source (command pwd)
  "Anything Source of `anything-grep'."
  `((command . ,command)
    (pwd . ,pwd)
    (name . ,(format "%s [%s]" command pwd))
    (action . agrep-goto)
    (anything-grep)
    (candidate-number-limit . 9999)
    (migemo)
    ;; to inherit faces
    (candidates-in-buffer)
    (get-line . buffer-substring)
    ,@(when anything-grep-multiline
        '((multiline)
          (real-to-display . agrep-real-to-display)))))

(defun anything-compile-source--agrep-init (source)
  (if (assq 'anything-grep source)
      (append '((init . agrep-init)
                (candidates)) source)
    source))

(defun agrep-init ()
  (agrep-create-buffer (anything-attr 'command)  (anything-attr 'pwd)))

(defun agrep-real-to-display (file-line-content)
  (if (string-match ":\\([0-9]+\\):" file-line-content)
      (format "%s:%s\n %s"
              (substring file-line-content 0 (match-beginning 0))
              (match-string 1 file-line-content)
              (substring file-line-content (match-end 0)))
    file-line-content))

(defvar agrep-source-local nil)
(defvar agrep-waiting-source nil
  "`anything' sources to get together in `agrep-sentinel'.")
(defun agrep-do-grep (command pwd)
  "Insert result of COMMAND. The current directory is PWD.
GNU grep is expected for COMMAND. The grep result is colorized."
  (let ((process-environment process-environment))
    (when (eq grep-highlight-matches t)
      ;; Modify `process-environment' locally bound in `call-process-shell-command'.
      (setenv "GREP_OPTIONS" (concat (getenv "GREP_OPTIONS") " --color=always"))
      ;; for GNU grep 2.5.1
      (setenv "GREP_COLOR" "01;31")
      ;; for GNU grep 2.5.1-cvs
      (setenv "GREP_COLORS" "mt=01;31:fn=:ln=:bn=:se=:ml=:cx=:ne"))
    (set (make-local-variable 'agrep-source-local) (anything-get-current-source))
    (add-to-list 'agrep-waiting-source agrep-source-local)
    (set-process-sentinel
     (start-process-shell-command "anything-grep" (current-buffer)
                                  (format "cd %s; %s" pwd command))
     'agrep-sentinel)))

(defvar agrep-do-after-minibuffer-exit nil)
(defun agrep-minibuffer-exit-hook ()
  (when agrep-do-after-minibuffer-exit
    (run-at-time 1 nil agrep-do-after-minibuffer-exit)
    (setq agrep-do-after-minibuffer-exit nil)))
(add-hook 'minibuffer-exit-hook 'agrep-minibuffer-exit-hook)

(defun agrep-show (func)
  (if (active-minibuffer-window)
      (setq agrep-do-after-minibuffer-exit func)
    (funcall func)))
;; (anything-grep "sleep 1; grep -Hin grep anything-grep.el" "~/src/anything-config/extensions/")

(defun agrep-sentinel (proc stat)
  (with-current-buffer (process-buffer proc)
    (setq agrep-waiting-source (delete agrep-source-local agrep-waiting-source))
    (agrep-fontify))
  (unless agrep-waiting-source
    ;; call anything
    (agrep-show
     (lambda ()
       (let ((anything-quit-if-no-candidate (lambda () (message "No matches"))))
         (anything anything-grep-sources nil nil nil nil anything-grep-buffer-name))))))

(defun agrep-fontify ()
  "Fontify the result of `agrep-do-grep'."
  ;; Color matches.
  (goto-char 1)
  (while (re-search-forward "\\(\033\\[01;31m\\)\\(.*?\\)\\(\033\\[[0-9]*m\\)" nil t)
    (put-text-property (match-beginning 2) (match-end 2) 'face  grep-match-face)
    (replace-match "" t t nil 1)
    (replace-match "" t t nil 3))
  ;; Delete other escape sequences.
  (goto-char 1)
  (while (re-search-forward "\\(\033\\[[0-9;]*[mK]\\)" nil t)
    (replace-match "" t t nil 0))
  (when anything-grep-fontify-file-name
    (goto-char 1)
    (while (re-search-forward ":\\([0-9]+\\):" nil t)
      (put-text-property (point-at-bol) (match-beginning 0) 'face compilation-info-face)
      (put-text-property (match-beginning 1) (match-end 1) 'face compilation-line-face)
      (forward-line 1))))
;; (anything-grep "grep -n grep *.el" "~/emacs/init.d")

(defun agrep-create-buffer (command pwd)
  "Create candidate buffer for `anything-grep'.
Its contents is fontified grep result."
  (with-current-buffer (anything-candidate-buffer 'global)
    (setq default-directory pwd)
    (agrep-do-grep command pwd)
    (current-buffer)))
;; (display-buffer (agrep-create-buffer "grep --color=always -Hin agrep anything-grep.el" default-directory))
;; (anything '(((name . "test") (init . (lambda () (anything-candidate-buffer (get-buffer " *anything grep:grep --color=always -Hin agrep anything-grep.el*")) )) (candidates-in-buffer) (get-line . buffer-substring))))

(defun agrep-goto  (file-line-content)
  "Visit the source for the grep result at point."
  (string-match ":\\([0-9]+\\):" file-line-content)
  (save-match-data
    (funcall anything-grep-find-file-function
             (expand-file-name (substring file-line-content
                                          0 (match-beginning 0))
                               (anything-attr 'pwd))))
  (goto-line (string-to-number (match-string 1 file-line-content)))
  (run-hooks 'anything-grep-goto-hook))

;; (@* "simple grep interface")
(defun anything-grep (command pwd)
  "Run grep in `anything' buffer to narrow results.
It asks COMMAND for grep command line and PWD for current directory."
  (interactive
   (progn
     (grep-compute-defaults)
     (let ((default (grep-default-command)))
       (list (read-from-minibuffer "Run grep (like this): "
				   (if current-prefix-arg
				       default grep-command)
				   nil nil 'grep-history
				   (if current-prefix-arg nil default))
             (read-directory-name "Directory: " default-directory default-directory t)))))
  (anything-grep-base (list (agrep-source (agrep-preprocess-command command) pwd))
                      (format "*anything grep:%s [%s]*" command (abbreviate-file-name pwd))))
;; (anything-grep "grep -Hin agrep anything-grep.el" default-directory)

(defun agrep-preprocess-command (command)
  (with-temp-buffer
    (insert command)
    (goto-char 1)
    (when (search-forward "$buffers" nil t)
      (delete-region (match-beginning 0) (match-end 0))
      (insert (mapconcat 'shell-quote-argument
                         (delq nil (mapcar 'buffer-file-name (buffer-list))) " ")))
    (when anything-grep-filter-command
      (goto-char (point-max))
      (insert "|" anything-grep-filter-command))
    (buffer-string)))

;; (@* "grep in predefined files")
(defvar agbn-last-name nil
  "The last used name by `anything-grep-by-name'.")

(defun agrep-by-name-read-info (&rest kinds)
  (let* ((default (or (thing-at-point 'symbol) ""))
         (result (mapcar (lambda (kind)
                           (case kind
                             ('query (read-string
                                      (format "Grep query (default:%s): " default)
                                      nil nil default))
                             ('name (completing-read
                                     "Grep by name: "
                                     anything-grep-alist
                                     nil t nil nil agbn-last-name))))
                         kinds)))
    (if (cdr result)                    ; length >= 1
        result
      (car result))))

(defun anything-grep-by-name (&optional query name)
  "Do `anything-grep' from predefined location.
It asks NAME for location name and QUERY."
  (interactive (agrep-by-name-read-info 'query 'name))
  (setq query (or query (agrep-by-name-read-info 'query)))
  (setq name (or name (agrep-by-name-read-info 'name)))
  (setq agbn-last-name name)
  (anything-aif (assoc-default name anything-grep-alist)
      (progn
        (grep-compute-defaults)
        (anything-grep-base
         (mapcar (lambda (args)
                   (destructuring-bind (cmd dir) args
                     (agrep-source (format (agrep-preprocess-command cmd)
                                           (shell-quote-argument query)) dir)))
                 it)
         (format "*anything grep:%s [%s]" query name)))
    (error "no such name %s" name)))

(defun anything-grep-by-name-reversed (&optional name query)
  "Do `anything-grep' from predefined location.
It asks QUERY and NAME for location name.

Difference with `anything-grep-by-name' is prompt order."
  (interactive (agrep-by-name-read-info (quote name) (quote query)))
  (anything-grep-by-name query name))

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "agrep-by-name-read-info")
      (expect "query1"
        (stub read-string => "query1")
        (agrep-by-name-read-info 'query))
      (expect "elinit"
        (stub completing-read => "elinit")
        (agrep-by-name-read-info 'name))
      (expect '("query1" "elinit")
        (stub read-string => "query1")
        (stub completing-read => "elinit")
        (agrep-by-name-read-info 'query 'name))
      (expect '("elinit" "query1")
        (stub read-string => "query1")
        (stub completing-read => "elinit")
        (agrep-by-name-read-info 'name 'query))
      )))

(provide 'anything-grep)

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything-grep.el"))
;;; anything-grep.el ends here
