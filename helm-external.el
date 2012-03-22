;;; helm-external.el --- Run Externals commands within Emacs with helm completion.

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

(defvar helm-external-command-history nil)

(defun helm-c-external-commands-list-1 (&optional sort)
  "Returns a list of all external commands the user can execute.
If `helm-c-external-commands-list' is non-nil it will
return its contents.  Else it calculates all external commands
and sets `helm-c-external-commands-list'."
  (if helm-c-external-commands-list
      helm-c-external-commands-list
      (setq helm-c-external-commands-list
            (loop
                  with paths = (split-string (getenv "PATH") path-separator)
                  with completions = ()
                  for dir in paths
                  when (and (file-exists-p dir) (file-accessible-directory-p dir))
                  for lsdir = (loop for i in (directory-files dir t)
                                    for bn = (file-name-nondirectory i)
                                    when (and (not (member bn completions))
                                              (not (file-directory-p i))
                                              (file-executable-p i))
                                    collect bn)
                  append lsdir into completions
                  finally return (if sort (sort completions 'string-lessp) completions)))))

(defun helm-run-or-raise (exe &optional file)
  "Generic command that run asynchronously EXE.
If EXE is already running just jump to his window if `helm-raise-command'
is non--nil.
When FILE argument is provided run EXE with FILE.
In this case EXE must be provided as \"EXE %s\"."
  (lexical-let* ((real-com (car (split-string (replace-regexp-in-string
                                               "%s" "" exe))))
                 (proc     (if file (concat real-com " " file) real-com)))
    (if (get-process proc)
        (if helm-raise-command
            (shell-command  (format helm-raise-command real-com))
            (error "Error: %s is already running" real-com))
        (when (loop for i in helm-c-external-commands-list thereis real-com)
          (message "Starting %s..." real-com)
          (if file
              (start-process-shell-command
               proc nil (format exe (shell-quote-argument
                                     (if (eq system-type 'windows-nt)
                                         (helm-w32-prepare-filename file)
                                         file))))
              (start-process-shell-command proc nil real-com))
          (set-process-sentinel
           (get-process proc)
           #'(lambda (process event)
               (when (and (string= event "finished\n")
                          helm-raise-command
                          (not (helm-c-get-pid-from-process-name real-com)))
                 (shell-command  (format helm-raise-command "emacs")))
               (message "%s process...Finished." process))))
        (setq helm-c-external-commands-list
              (cons real-com
                    (delete real-com helm-c-external-commands-list))))))

;;;###autoload
(defun helm-c-run-external-command (program)
  "Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-c-external-commands-list'."
  (interactive (list
                (helm-comp-read
                 "RunProgram: "
                 (helm-c-external-commands-list-1 'sort)
                 :must-match t
                 :name "External Commands"
                 :history helm-external-command-history)))
  (helm-run-or-raise program)
  (setq helm-external-command-history
        (cons program (delete program
                              (loop for i in helm-external-command-history
                                    when (executable-find i) collect i)))))


(provide 'helm-external)

;;; helm-external ends here
