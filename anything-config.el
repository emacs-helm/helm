;;; anything-config.el --- predefined configurations for anything

;; Copyright (C) 2007 Tamas Patrovics

;; Maintainer: Tassilo Horn <tassilo@member.fsf.org>

;; Contributors:
;;     Tamas Patrovics
;;     Tassilo Horn <tassilo@member.fsf.org>
;;     Vagn Johansen <gonz808@hotmail.com>
;;     Mathias Dahl <mathias.dahl@gmail.com>
;;     Bill Clementson <billclem@gmail.com>
;;     Stefan Kamphausen <ska@skamphausen.de>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; This package provides predefined configurations for anything.el
;; You can pick the ones you like and use in your own configuration
;; like this:
;;
;;   (require 'anything-config) ; loads anything.el too
;;
;;   (setq anything-sources (list anything-source-buffers
;;                                anything-source-emacs-commands
;;                                anything-source-locate ...))
;;
;;   (setq anything-type-actions (list anything-actions-buffer
;;                                     anything-actions-file
;;                                     anything-actions-command
;;                                     anything-actions-function
;;                                     anything-actions-sexp ...))
;;
;;   (setq anything-action-transformers
;;         '((buffer   . anything-transform-buffer-actions)
;;           (file     . anything-transform-file-actions)
;;           (command  . anything-transform-command-actions)
;;           (function . anything-transform-function-actions)
;;           (sexp     . anything-transform-sexp-actions)))
;;

;;; Startup

;; Require anything after this file has been loaded. This allows overwriting
;; defvars of anything.el here.
(eval-after-load
    (buffer-file-name)
    (require 'anything))

;;; Version

(defvar anything-config-version "<2007-07-27 Fri 02:12>"
  "The version of anything-config.el, or better the date of the
last change.")

;;; Sources

;;;; Buffers

(defun anything-buffer-list ()
  "Return the list of names of buffers with the `anything-buffer'
and hidden buffers filtered out. The first buffer in the list
will be the last recently used buffer that is not the current
buffer."
  (let ((buffers (remove-if (lambda (name)
                              (or (equal name anything-buffer)
                                  (eq ?\  (aref name 0))))
                            (mapcar 'buffer-name (buffer-list)))))
     (append (cdr buffers) (list (car buffers)))))

(defvar anything-source-buffers
  '((name . "Buffers")
    (candidates . anything-buffer-list)
    (type . buffer)))

;;;; File name history

(defvar anything-source-file-name-history
  '((name . "File Name History")
    (candidates . file-name-history)
    (type . file)))

;;;; Recentf files

(defvar anything-source-recentf
  '((name . "Recentf")
    (candidates . recentf-list)
    (type . file))
  "See (info \"(emacs)File Conveniences\").")

;;;; Files in current dir

(defvar anything-source-files-in-current-dir
  '((name . "Files from Current Directory")
    (init-func . (lambda ()
                   (setq anything-default-directory
                         default-directory)))
    (candidates . (lambda ()
                    (directory-files
                     anything-default-directory)))
    (type . file)))

;;;; Man Pages

(defvar anything-source-man-pages
  `((name . "Manual Pages")
    (candidates . ,(progn
                     (when (require 'woman nil t)
                       (woman-file-name "")
                       (sort (mapcar 'car
                                     woman-topic-all-completions)
                             'string-lessp))))
    (action . (("Show with Woman" . woman)))
    (requires-pattern . 2)))

;;;; Info pages

(defvar anything-source-info-pages
  `((name . "Info Pages")
    (candidates . ,(save-window-excursion
                     (save-excursion
                       (require 'info)
                       (Info-find-node "dir" "top")
                       (goto-char (point-min))
                       (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
                             topics)
                         (while (re-search-forward info-topic-regexp nil t)
                           (add-to-list 'topics (match-string-no-properties 1)))
                         (goto-char (point-min))
                         topics))))
    (action . (("Show with Info" .(lambda (node-str)
                                   (info (replace-regexp-in-string "^[^:]+: "
                                                                   ""
                                                                   node-str))))))
    (requires-pattern . 2)))

;;;; Complex command history

(defvar anything-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda ()
                    (mapcar 'prin1-to-string
                            command-history)))
    (type . sexp)))

;;;; Emacs commands

(defvar anything-source-emacs-commands
  '((name . "Emacs Commands")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (commandp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (type . command)
    (requires-pattern . 2))
  "Source for completing and invoking Emacs commands. A command
is a function with interactive spec that can be invoked with
`M-x'.

To get non-interactive functions listed, use
`anything-source-emacs-functions'.")

;;;; Emacs functions

(defvar anything-source-emacs-functions
  '((name . "Emacs Functions")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (functionp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (type . function)
    (requires-pattern . 2))
  "Source for completing Emacs functions.")

;;;; Bookmarks

(defvar anything-source-bookmarks
  '((name . "Bookmarks")
    (candidates . bookmark-all-names)
    (action . (("Jump to Bookmark" . bookmark-jump))))
  "See (info \"(emacs)Bookmarks\").")


;;;; Picklist

(defvar anything-source-picklist
  '((name . "Picklist")
    (candidates . (lambda ()
                    (mapcar 'car picklist-list)))
      (type . file)))

;;;; Locate

(defvar anything-source-locate
  '((name . "Locate")
    (candidates . (lambda ()
                    (start-process "locate-process" nil
                                   "locate" "-i" "-r"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with locate.")

;;;; Tracker desktop search

(defvar anything-source-tracker-search
  '((name . "Tracker Search")
    (candidates . (lambda ()
                    (start-process "tracker-search-process" nil
                                   "tracker-search"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")

;;;; Spotlight (MacOS X desktop search)

(defvar anything-source-mac-spotlight
  '((name . "mdfind")
    (candidates . (lambda ()
                    (start-process "mdfind-process" nil
                                   "mdfind" anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")

;;; Types

;;;; Type Actions

;;;;; Extension macros and functions

(defun anything-source-p (source)
  "Return non-nil if SOURCE is a valid anything-source."
  (and (assq 'name       source)
       (assq 'candidates source)
       (or (assq 'action source)
           (assq 'type   source))))

(defmacro anything-add-to-actions (var action)
  "Add the given ACTION to the list of actions defined in the
given VAR, which has to be an anything-source-FOO or
anything-actions-TYPE. Here're two examples:

    ;; Add an action which puts the selected file's path onto the
    ;; kill ring to the default actions for type file.
    (anything-add-to-actions anything-actions-file
                             (\"Put Path on Kill Ring\" . kill-new))

    ;; Add an action which opens the selected manual page with the
    ;; `man' command to the actions defined in the source for manual
    ;; pages.
    (anything-add-to-actions anything-source-man-pages
                             (\"Open with man\" . man))

This must be done *before* you set `anything-type-actions' or
`anything-sources' in your ~/.emacs.

The purpose of this function is to allow users to extend the list
of type actions or source actions with actions that won't go into
anything-config.el, because they're specific to a platform or a
user."
  (if (anything-source-p (symbol-value var))
      ;; Add action to a anything-source-FOO
      (let ((action-list (cdr (assq 'action (symbol-value var)))))
        `(unless (member ',action ',action-list)
           (setq ,var (delete (assq 'action ,var) ,var))
           (push (cons 'action (append ',action-list (list ',action))) ,var)))
    ;; Add action to a anything-actions-FOO variable
    `(unless (member ',action (cdr ,var))
       (setq ,var
             (cons (car ,var)
                   (append (cdr ,var)
                           (list ',action)))))))

;;;;; Buffers

(defvar anything-actions-buffer
  '(buffer . (("Switch to Buffer" . switch-to-buffer)
              ("Switch to Buffer other Window" . switch-to-buffer-other-window)
              ("Switch to Buffer other Frame" . switch-to-buffer-other-frame)
              ("Display Buffer"   . display-buffer)
              ("Kill Buffer"      . kill-buffer)))
  "Actions for type `buffer'.")

;;;;; Files

(defvar anything-external-commands-list nil
  "A list of all external commands the user can execute. If this
variable is not set by the user, it will be calculated
automatically.")

(defun anything-external-commands-list-1 ()
  "Returns a list of all external commands the user can execute.

If `anything-external-commands-list' is non-nil it will return
its contents.  Else it calculates all external commands and sets
`anything-external-commands-list'.

The code is ripped out of `eshell-complete-commands-list'."
  (if anything-external-commands-list
      anything-external-commands-list
    (setq anything-external-commands-list
          (let* ((paths (split-string (getenv "PATH") path-separator))
                 (cwd (file-name-as-directory
                       (expand-file-name default-directory)))
                 (path "") (comps-in-path ())
                 (file "") (filepath "") (completions ()))
            ;; Go thru each path in the search path, finding completions.
            (while paths
              (setq path (file-name-as-directory
                          (expand-file-name (or (car paths) ".")))
                    comps-in-path
                    (and (file-accessible-directory-p path)
                         (file-name-all-completions "" path)))
              ;; Go thru each completion found, to see whether it should be
              ;; used, e.g. see if it's executable.
              (while comps-in-path
                (setq file (car comps-in-path)
                      filepath (concat path file))
                (if (and (not (member file completions))
                         (or (string-equal path cwd)
                             (not (file-directory-p filepath)))
                         (file-executable-p filepath))
                    (setq completions (cons file completions)))
                (setq comps-in-path (cdr comps-in-path)))
              (setq paths (cdr paths)))
            completions))))

(defun anything-file-buffers (filename)
  "Returns a list of those buffer names which correspond to the
file given by FILENAME."
  (let (name ret)
    (dolist (buf (buffer-list) ret)
      (let ((bfn (buffer-file-name buf)))
        (when (and bfn
                   (string= filename bfn))
          (push (buffer-name buf) ret)))
    ret)))

(defun anything-delete-file (file)
  "Deletes the given file after querying the user. Asks to kill
buffers associated with that file, too."
  (if (y-or-n-p (format "Really delete file %s? " file))
      (progn
        (let ((buffers (anything-file-buffers file)))
          (delete-file file)
          (dolist (buf buffers)
            (when (y-or-n-p (format "Kill buffer %s, too? " buf))
              (kill-buffer buf)))))
    (message "Nothing deleted.")))

(defvar anything-actions-file
  '(file . (("Find File" . find-file)
            ("Find File other Window" . find-file-other-window)
            ("Find File other Frame" . find-file-other-frame)
            ("Open Dired in File's Directory" . (lambda (filename)
                                                  (dired (file-name-directory filename))
                                                  (dired-goto-file filename)))
            ("Delete File" . anything-delete-file)
            ("Open File with external Tool" .
             (lambda (file)
               (start-process "anything-open-file-externally"
                              nil
                              (completing-read "Program: "
                                               (anything-external-commands-list-1))
                              file))))))

;;;;; Commands

(defvar anything-actions-command
  '(command . (("Call Interactively" . (lambda (command-name)
                                         (call-interactively (intern command-name))))
               ("Describe Command" . (lambda (command-name)
                                       (describe-function (intern command-name))))
               ("Add Command to the Kill Ring" . kill-new)
               ("Go to the Command's Definition" . (lambda (command-name)
                                                     (find-function (intern command-name)))))))

;;;;; Functions

(defvar anything-actions-function
  '(function . (("Describe Command" . (lambda (function-name)
                                        (describe-function (intern function-name))))
                ("Add Command to the Kill Ring" . kill-new)
                ("Go to the Function's Definition" . (lambda (function-name)
                                                       (find-function
                                                        (intern function-name)))))))

;;;;; S-Expressions

(defvar anything-actions-sexp
  '(sexp . (("Eval S-Expression" . (lambda (c)
                                     (eval (read c))))
            ("Add S-Expression to the Kill Ring" . kill-new))))


;;;; Action Transformers

;;;;; Buffers

(defvar anything-action-transformers-buffer nil
  "A List of transformer functions for buffers. See
`anything-transform-buffer-actions' for details.")

(defun anything-transform-buffer-actions (actions candidate)
  "Calls any function in `anything-action-transformers-buffer'
with the current list of ACTIONS and the function CANDIDATE
modifying the list of actions for this function dynamically."
  (dolist (trans anything-action-transformers-function)
    (setq actions (funcall trans actions candidate)))
  actions)

;;;;; Files

(defun anything-transform-file-open-system-specific (actions candidate)
  "Add action to open the file CANDIDATE with the default
application on that platform, e.g. `open' on Macs and `xdg-open'
on GNU/Linux systems."
  (let (tool)
    (cond ((eq system-type 'gnu/linux)
           (setq tool "xdg-open"))
          ((or (eq system-type 'darwin)  ;; Mac OS X
               (eq system-type 'macos))  ;; Mac OS 9
           (setq tool "open")))
    (append actions `(("Open with platform-specific default tool"
                       .
                       (lambda (file)
                         (start-process "anything-open-file-with-default-tool"
                              nil ,tool file)))))))

(defun anything-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file. Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "el")
          (string= (file-name-extension candidate) "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
    actions))

(defun anything-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file. Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "htm")
          (string= (file-name-extension candidate) "html"))
      (append actions '(("Browse with Browser" . browse-url)))
    actions))

(defvar anything-action-transformers-file
  '(anything-transform-file-load-el
    anything-transform-file-browse-url
    anything-transform-file-open-system-specific)
  "A List of transformer functions for files. See
`anything-transform-file-actions' for details.")

(defun anything-transform-file-actions (actions candidate)
  "Calls any function in `anything-action-transformers-file' with
the current list of ACTIONS and the file CANDIDATE modifying the
list of actions for this file dynamically."
  (dolist (trans anything-action-transformers-file)
    (setq actions (funcall trans actions candidate)))
  actions)

;;;;; Commands

(defvar anything-action-transformers-command nil
  "A List of transformer functions for commands. See
`anything-transform-command-actions' for details.")

(defun anything-transform-command-actions (actions candidate)
  "Calls any function in `anything-action-transformers-command'
with the current list of ACTIONS and the command CANDIDATE
modifying the list of actions for this command dynamically."
  (dolist (trans anything-action-transformers-command)
    (setq actions (funcall trans actions candidate)))
  actions)

;;;;; Function

(defun anything-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command. Else return ACTIONS unmodified."
  (if (commandp (intern candidate))
      (append actions '(("Call Interactively"
                         .
                         (lambda (c)
                           (call-interactively (intern c))))))
    actions))

(defvar anything-action-transformers-function
  '(anything-transform-function-call-interactively)
  "A List of transformer functions for functions. See
`anything-transform-function-actions' for details.")

(defun anything-transform-function-actions (actions candidate)
  "Calls any function in `anything-action-transformers-function'
with the current list of ACTIONS and the function CANDIDATE
modifying the list of actions for this function dynamically."
  (dolist (trans anything-action-transformers-function)
    (setq actions (funcall trans actions candidate)))
  actions)

;;;;; S-Expressions

(defun anything-transform-sexp-eval-command-sexp (actions candidate)
  "If CANDIDATE's `car' in a command, then add an action to
evaluate it and put it onto the `command-history'."
  (if (commandp (car (read candidate)))
      ;; Make it first entry
      (cons '("Eval and put onto command-history" . (lambda (sexp)
                                                      (let ((sym (read sexp)))
                                                        (eval sym)
                                                        (setq command-history
                                                              (cons sym command-history)))))
            actions)
    actions))

(defvar anything-action-transformers-sexp
  '(anything-transform-sexp-eval-command-sexp)
  "A List of transformer functions for sexps. See
`anything-transform-sexp-actions' for details.")

(defun anything-transform-sexp-actions (actions candidate)
  "Calls any function in `anything-action-transformers-sexp' with
the current list of ACTIONS and the sexp CANDIDATE modifying the
list of actions for this sexp dynamically."
  (dolist (trans anything-action-transformers-sexp)
    (setq actions (funcall trans actions candidate)))
  actions)

;;; Provide anything-config

(provide 'anything-config)

;; Local Variables:
;; mode: outline-minor
;; End:

;;; anything-config.el ends here
