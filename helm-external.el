;;; helm-external.el --- Run Externals commands within Emacs with helm completion. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2016 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-help)
(require 'helm-net)


(defgroup helm-external nil
  "External related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-raise-command nil
  "A shell command to jump to a window running specific program.
Need external program wmctrl.
This will be use with `format', so use something like \"wmctrl -xa %s\"."
  :type 'string
  :group 'helm-external)

(defcustom helm-external-programs-associations nil
  "Alist to store externals programs associated with file extension.
This variable overhide setting in .mailcap file.
e.g : '\(\(\"jpg\" . \"gqview\"\) (\"pdf\" . \"xpdf\"\)\) "
  :type '(alist :key-type string :value-type string)
  :group 'helm-external)

(defcustom helm-default-external-file-browser "nautilus"
  "Default external file browser for your system.
Directories will be opened externally with it when
opening file externally in `helm-find-files'.
Set to nil if you do not have external file browser
or do not want to use it.
Windows users should set that to \"explorer.exe\"."
  :group 'helm-external
  :type  'string)


;;; Internals
(defvar helm-external-command-history nil)

(defvar helm-external-commands-list nil
  "A list of all user-executable external commands.
If this variable is not set by the user, it will be calculated
automatically.")

(defun helm-external-commands-list-1 (&optional sort)
  "Return a list of all user-executable external commands.
Commands are path-qualified, e.g. \"/usr/bin/xdg-open\".

If `helm-external-commands-list' is non-nil, use its values.
Else, find all external commands in the user's path and set
`helm-external-commands-list'."
  (helm-aif helm-external-commands-list
      it
    (setq helm-external-commands-list
          (cl-loop for dir in (parse-colon-path (getenv "PATH"))
                   when (file-accessible-directory-p dir)
                   for lsdir = (cl-loop for i in (directory-files dir t)
                                        when (and (not (file-directory-p i))
                                                  (file-executable-p i)
                                                  (not (member i completions)))
                                        collect i)
                   append lsdir into completions
                   finally return
                   (if sort (sort completions 'string-lessp) completions)))))

(defun helm-run-or-raise (exe &optional file)
  "Run command EXE asynchronously.

If EXE is already running and `helm-raise-command' is non-nil,
jump to the process window.

If FILE is provided, run EXE with FILE as \"EXE FILE\"."
  (let* ((proc     (if file (concat exe " " file) exe))
         process-connection-type)
    (if (get-process proc)
        (if helm-raise-command
            (shell-command  (format helm-raise-command exe))
          (error "Error: %s is already running" exe))
      (message "Starting %s..." exe)
      (if file
          (start-process-shell-command
           proc nil (concat exe " " (shell-quote-argument
                                     (if (eq system-type 'windows-nt)
                                         (helm-w32-prepare-filename file)
                                       file))))
        (start-process-shell-command proc nil exe))
      (set-process-sentinel
       (get-process proc)
       (lambda (process event)
         (when (and (string= event "finished\n")
                    helm-raise-command
                    (not (helm-get-pid-from-process-name exe)))
           (shell-command  (format helm-raise-command "emacs")))
         (message "%s process...Finished." process)))
      (setq helm-external-commands-list
            (cons exe (delete exe helm-external-commands-list))))))

(defun helm-get-mailcap-for-file (filename)
  "Get the command to use for FILENAME from mailcap files."
  (mailcap-parse-mailcaps)
  (let* ((ext  (file-name-extension filename))
         (mime (when ext (mailcap-extension-to-mime ext)))
         (result (when mime (mailcap-mime-info mime))))
    ;; If elisp file have no associations in .mailcap
    ;; `mailcap-maybe-eval' is returned, in this case just return nil.
    (when (stringp result)
      (replace-regexp-in-string "\\s-%s" "" result))))

(defun helm-get-default-program-for-file (filename)
  "Return default program to open FILENAME, or nil if none.

Look for command first in `helm-external-programs-associations',
then in mailcap file."
  (let* ((ext      (file-name-extension filename))
         (def-prog (assoc-default ext helm-external-programs-associations)))
    (cond ((and def-prog (not (string= def-prog "")))
           def-prog)
          ((and helm-default-external-file-browser
                (file-directory-p filename))
           helm-default-external-file-browser)
          (t (helm-get-mailcap-for-file filename)))))

(defun helm-open-file-externally (file)
  "Open FILE with an external program.
Try to guess which program to use with `helm-get-default-program-for-file'.
If not found or a prefix arg is given query the user which tool to use."
  (let* ((fname          (expand-file-name file))
         (collection     (helm-external-commands-list-1 'sort))
         (default-program       (helm-get-default-program-for-file fname))
         (program (if (or helm-current-prefix-arg (not default-program))
                             ;; Prefix arg or no default program.
                             (prog1
                                 (helm-comp-read
                                  "Program: " collection
                                  :must-match t
                                  :name "Open file Externally"
                                  :del-input nil
                                  :history helm-external-command-history)
                               ;; Always prompt to set this program as default.
                               (setq default-program nil))
                           ;; No prefix arg or default program exists.
                           default-program)))
    (unless (or default-program ; Association exists, no need to record it.
                ;; Don't try to record non--filenames associations (e.g urls).
                (not (file-exists-p fname)))
      (when
          (y-or-n-p
           (format
            "Do you want to make `%s' the default program for this kind of files? "
            program))
        (helm-aif (assoc (file-name-extension fname)
                         helm-external-programs-associations)
            (setq helm-external-programs-associations
                  (delete it helm-external-programs-associations)))
        (push (cons (file-name-extension fname)
                    (helm-read-string
                     "Program (Add args maybe and confirm): " program))
              helm-external-programs-associations)
        (customize-save-variable 'helm-external-programs-associations
                                 helm-external-programs-associations)))
    (helm-run-or-raise program file)
    (setq helm-external-command-history
          (cons program
                (delete program
                        (cl-loop for i in helm-external-command-history
                              when (executable-find i) collect i))))))

;;;###autoload
(defun helm-run-external-command (program)
  "Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-external-commands-list'."
  (interactive (list
                (helm-comp-read
                 "RunProgram: "
                 (delete-dups (mapcar #'file-name-nondirectory
                                      (helm-external-commands-list-1 'sort)))
                 :must-match t
                 :del-input nil
                 :name "External Commands"
                 :history (mapcar #'file-name-nondirectory
                                  helm-external-command-history))))
  (helm-run-or-raise program)
  (setq helm-external-command-history
        (cons program (delete program
                              (cl-loop for i in helm-external-command-history
                                    when (executable-find i) collect i)))))


(provide 'helm-external)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-external ends here
