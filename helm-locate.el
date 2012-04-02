;;; helm-locate.el --- helm interface for locate.

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

;; NOTE for WINDOZE users:
;; You have to install Everything with his command line interface here:
;; http://www.voidtools.com/download.php

;;; Code:

(eval-when-compile (require 'cl))
(require 'helm)
;(require 'helm-mode)


(defgroup helm-locate nil
  "Locate related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-locate-db-file-regexp "m?locate\.db$"
  "Default regexp to match locate database.
If nil Search in all files."
  :type  'string
  :group 'helm-locate)

(defcustom helm-ff-locate-db-filename "locate.db"
  "The basename of the locatedb file you use locally in your directories.
When this is set and `helm' find such a file in the directory from
where you launch locate, it will use this file and will not prompt you
for a db file.
Note that this happen only when locate is launched with a prefix arg."
  :group 'helm-locate
  :type 'string)

(defcustom helm-c-locate-command nil
  "A list of arguments for locate program.
If nil it will be calculated when `helm-locate' startup
with these default values for different systems:

Gnu/linux: \"locate -i -r %s\"
berkeley-unix: \"locate -i %s\"
windows-nt: \"es -i -r %s\"
Others: \"locate %s\"

This string will be passed to format so it should end with `%s'.
The \"-r\" option must be the last option."
  :type 'string
  :group 'helm-locate)

(defvar helm-generic-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-g s")   'helm-ff-run-grep)
    (define-key map (kbd "M-g z")   'helm-ff-run-zgrep)
    (define-key map (kbd "M-g p")   'helm-ff-run-pdfgrep)
    (define-key map (kbd "M-D")     'helm-ff-run-delete-file)
    (define-key map (kbd "C-=")     'helm-ff-run-ediff-file)
    (define-key map (kbd "C-c =")   'helm-ff-run-ediff-merge-file)
    (define-key map (kbd "C-c o")   'helm-ff-run-switch-other-window)
    (define-key map (kbd "M-i")     'helm-ff-properties-persistent)
    (define-key map (kbd "C-c C-x") 'helm-ff-run-open-file-externally)
    (define-key map (kbd "C-w")     'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")   'helm-generic-file-help)
    map)
  "Generic Keymap for files.")



(defun helm-ff-find-locatedb (&optional from-ff)
  "Try to find if a local locatedb file is available.
The search is done in `helm-ff-default-directory' or
fall back to `default-directory' if FROM-FF is nil."
  (when helm-ff-locate-db-filename
    (cond ((and helm-ff-default-directory
                from-ff
                (file-exists-p (expand-file-name
                                helm-ff-locate-db-filename
                                helm-ff-default-directory))
                (expand-file-name
                 helm-ff-locate-db-filename
                 helm-ff-default-directory)))
          ((and (not from-ff)
                (file-exists-p (expand-file-name
                                helm-ff-locate-db-filename
                                default-directory))
                (expand-file-name
                 helm-ff-locate-db-filename
                 default-directory))))))

(defun helm-locate-1 (&optional localdb init from-ff)
  "Generic function to run Locate.
if LOCALDB is non--nil search and use a local locate db file.
INIT is a string to use as initial input in prompt.
See `helm-locate-with-db' and `helm-locate'."
  (helm-locate-with-db
   (and localdb
        (or (helm-ff-find-locatedb from-ff)
            (helm-c-read-file-name
             "LocateDBFiles: "
             :initial-input (or helm-ff-default-directory
                                default-directory)
             :marked-candidates t
             :preselect helm-locate-db-file-regexp
             :test #'(lambda (x)
                       (if helm-locate-db-file-regexp
                           ;; Select only locate db files and directories
                           ;; to allow navigation.
                           (or (string-match
                                helm-locate-db-file-regexp x)
                               (file-directory-p x))
                           x)))))
   init))

(defun helm-locate-set-command ()
  "Setup `helm-c-locate-command' if not already defined."
  (unless helm-c-locate-command
    (setq helm-c-locate-command
          (case system-type
            ('gnu/linux "locate -i -r %s")
            ('berkeley-unix "locate -i %s")
            ('windows-nt "es -i -r %s")
            (t "locate %s")))))

(defun helm-locate-with-db (&optional db initial-input)
  "Run locate -d DB.
If DB is not given or nil use locate without -d option.
Argument DB can be given as a string or list of db files.
Argument INITIAL-INPUT is a string to use as initial-input.
See also `helm-locate'."
  (when (and db (stringp db)) (setq db (list db)))
  (helm-locate-set-command)
  (let ((helm-c-locate-command
         (if db
             (replace-regexp-in-string
              "locate"
              (format "locate -d %s"
                      (mapconcat 'identity
                                 ;; Remove eventually
                                 ;; marked directories by error.
                                 (loop for i in db
                                       unless (file-directory-p i)
                                       collect i) ":"))
              helm-c-locate-command)
             helm-c-locate-command)))
    (helm :sources 'helm-c-source-locate
          :buffer "*helm locate*"
          :input initial-input
          :keymap helm-generic-files-map)))

(defun helm-c-locate-init ()
  "Initialize async locate process for `helm-c-source-locate'."
  (setq mode-line-format
        '(" " mode-line-buffer-identification " "
          (line-number-mode "%l") " "
          (:eval (propertize "(Locate Process Running) "
                  'face '((:foreground "red"))))))
  (prog1
      (start-process-shell-command "locate-process" nil
                                   (format helm-c-locate-command
                                           helm-pattern))
    (set-process-sentinel (get-process "locate-process")
                          #'(lambda (process event)
                              (when (string= event "finished\n")
                                (with-helm-window
                                  (force-mode-line-update nil)
                                  (helm-update-move-first-line)))))))

(defvar helm-c-source-locate
  `((name . "Locate")
    (init . helm-locate-set-command)
    (candidates . helm-c-locate-init)
    (type . file)
    (requires-pattern . 3)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (candidate-number-limit . 9999)
    (mode-line . helm-generic-file-mode-line-string)
    (delayed))
  "Find files matching the current input pattern with locate.")

(defun helm-c-locate-read-file-name (prompt &optional init)
  "Search a file with locate and return it's filename.
Use argument PROMPT and INIT for `helm' arguments
prompt and input."
  (helm :sources
        '((name . "Locate")
          (candidates . helm-c-locate-init)
          (action . identity)
          (requires-pattern . 3)
          (candidate-number-limit . 9999)
          (mode-line . helm-generic-file-mode-line-string)
          (delayed))
        :prompt prompt
        :input init
        :buffer "*helm locate rfn*"))

;;;###autoload
(defun helm-locate (arg)
  "Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options.

You can specify a specific database with prefix argument ARG \(C-u\).
Many databases can be used: navigate and mark them.
See also `helm-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'."
  (interactive "P")
  (setq helm-ff-default-directory default-directory)
  (helm-locate-1 arg))

(provide 'helm-locate)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-locate.el ends here
