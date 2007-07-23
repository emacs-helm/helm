;;; anything-config.el --- predefined configurations for anything

;; Copyright (C) 2007 Tamas Patrovics

;; Authors:
;;     Tamas Patrovics
;;     Tassilo Horn <tassilo@member.fsf.org>

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
;;   (setq anything-sources (list anything-source-emacs-commands
;;                                anything-source-locate ...)
;;
;;   (setq anything-type-actions (list anything-actions-buffer
;;                                     anything-actions-file ...)
;;

;;; Startup

;; Require anything after this file has been loaded. This allows overwriting
;; defvars of anything.el here.
(eval-after-load
    (buffer-file-name)
    (require 'anything))

;;; Predefined Sources

;;;; Buffers

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
    (type . file)))

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
    (action . woman)
    (requires-pattern . 2)))

;;;; Info pages

(defvar anything-source-info-pages
  `((name . "Info Pages")
    (candidates . ,(save-window-excursion
                     (save-excursion
                       (require 'info)
                       (Info-find-node "dir" "top")
                       (goto-char (point-min))
                       (let (topics)
                         (while (not (eobp))
                           (let ((face (get-text-property (point) 'font-lock-face))
                                 (next-change
                                  (or (next-property-change (point) (current-buffer))
                                      (point-max))))
                             (when (or (eq face 'info-xref)
                                       (eq face 'info-xref-visited))
                               ;; Add items in the form "Title ==> (node)subtopic"
                               (add-to-list 'topics
                                            (concat
                                             (buffer-substring-no-properties (point)
                                                                             next-change)
                                             " ==> "
                                             (substring (get-text-property (point)
                                                                           'help-echo)
                                                        15))))
                             (goto-char next-change)))
                         topics))))
    (action . (lambda (node-str)
                (info (replace-regexp-in-string "^.*==> " "" node-str))))
    (requires-pattern . 2)))

;;;; Complex command history

(defvar anything-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda ()
                    (mapcar 'prin1-to-string
                            command-history)))
    (action . (lambda (c)
                (eval (read c))))))

;;;; Emacs commands

(defvar anything-source-emacs-commands
  `((name . "Emacs Commands")
    (candidates . ,(let (commands)
                     (mapatoms (lambda (a)
                                 (if (commandp a)
                                     (push (symbol-name a)
                                           commands))))
                     (sort commands 'string-lessp)))
    (action . (lambda (command-name)
                (call-interactively (intern command-name)))))
  "Source for completing and invoking Emacs commands.")

;;;; Locate

(defvar anything-source-locate
  '((name . "Locate")
    (candidates . (lambda ()
                    (start-process "locate-process" nil
                                   "locate" "-i" "-r"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3))
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
    (requires-pattern . 3))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")

;;;; Spotlight (MacOS X desktop search)

(defvar anything-source-mac-spotlight
  '((name . "mdfind")
    (candidates . (lambda ()
                    (start-process "mdfind-process" nil
                                   "mdfind" anything-pattern)))
    (type . file)
    (requires-pattern . 3))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")

;;; Predefined Type Actions

;;;; Buffers

(defvar anything-actions-buffer
  '(buffer . (("Switch to Buffer" . switch-to-buffer)
              ("Pop to Buffer"    . pop-to-buffer)
              ("Display Buffer"   . display-buffer)
              ("Kill Buffer"      . kill-buffer)))
  "Actions for type `buffer'.")

;;;; Files

(defvar anything-external-commands-list nil
  "A list of all external commands the user can execute. If this
variable is not set by the user, it will be calculated
automatically.")

(defun anything-external-commands-list-1 ()
  "Returns a list of all external commands the user can execute.

If `anything-external-commands-list' is non-nil it will return
its contents. Else it calculates all external commands and sets
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

;;; Provide anything-config

(provide 'anything-config)

;;; anything-config.el.el ends here
