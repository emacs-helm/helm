;;; helm-locate.el --- helm interface for locate. -*- lexical-binding: t -*-

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

;; NOTE for WINDOZE users:
;; You have to install Everything with his command line interface here:
;; http://www.voidtools.com/download.php

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-types)
(require 'helm-help)


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

(defcustom helm-locate-command nil
  "A list of arguments for locate program.
Normally the default value should work on any system.

If nil it will be calculated when `helm-locate' startup
with these default values for different systems:

Gnu/linux: \"locate %s -e --regex %s\"
berkeley-unix: \"locate %s %s\"
windows-nt: \"es %s %s\"
Others: \"locate %s %s\"

This string will be passed to format so it should end with `%s'.
The first format spec is used for the \"-i\" value of locate/es,
So don't set it directly but use `helm-locate-case-fold-search'
for this.
The \"-r\" option must be the last option, however if not specified you will
be able to specify it during helm invocation by prefixing the pattern
you enter with \"-r\"."
  :type 'string
  :group 'helm-locate)

(defcustom helm-locate-create-db-command
  "updatedb -l 0 -o %s -U %s"
  "Command used to create a locale locate db file."
  :type 'string
  :group 'helm-locate)

(defcustom helm-locate-case-fold-search helm-case-fold-search
  "It have the same meaning as `helm-case-fold-search'.
The -i option of locate will be used depending of value of
`helm-pattern' when this is set to 'smart.
When nil \"-i\" will not be used at all.
and when non--nil it will always be used.
NOTE: the -i option of the \"es\" command used on windows does
the opposite of \"locate\" command."
  :group 'helm-locate
  :type 'symbol)

(defcustom helm-locate-fuzzy-match nil
  "Enable fuzzy matching in `helm-locate'."
  :group 'helm-locate
  :type 'boolean)

(defcustom helm-locate-project-list nil
  "A list of directories, your projects.
When set, allow browsing recursively files in all
directories of this list with `helm-projects-find-files'."
  :group 'helm-locate
  :type '(repeat string))


(defvar helm-generic-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-]")     'helm-ff-run-toggle-basename)
    (define-key map (kbd "C-s")     'helm-ff-run-grep)
    (define-key map (kbd "M-g s")   'helm-ff-run-grep)
    (define-key map (kbd "M-g z")   'helm-ff-run-zgrep)
    (define-key map (kbd "M-g p")   'helm-ff-run-pdfgrep)
    (define-key map (kbd "C-c g")   'helm-ff-run-gid)
    (define-key map (kbd "M-R")     'helm-ff-run-rename-file)
    (define-key map (kbd "M-C")     'helm-ff-run-copy-file)
    (define-key map (kbd "M-B")     'helm-ff-run-byte-compile-file)
    (define-key map (kbd "M-L")     'helm-ff-run-load-file)
    (define-key map (kbd "M-S")     'helm-ff-run-symlink-file)
    (define-key map (kbd "M-H")     'helm-ff-run-hardlink-file)
    (define-key map (kbd "M-D")     'helm-ff-run-delete-file)
    (define-key map (kbd "C-=")     'helm-ff-run-ediff-file)
    (define-key map (kbd "C-c =")   'helm-ff-run-ediff-merge-file)
    (define-key map (kbd "C-c o")   'helm-ff-run-switch-other-window)
    (define-key map (kbd "C-c C-o") 'helm-ff-run-switch-other-frame)
    (define-key map (kbd "M-i")     'helm-ff-properties-persistent)
    (define-key map (kbd "C-c C-x") 'helm-ff-run-open-file-externally)
    (define-key map (kbd "C-c X")   'helm-ff-run-open-file-with-default-tool)
    (define-key map (kbd "M-.")     'helm-ff-run-etags)
    (define-key map (kbd "C-w")     'helm-yank-text-at-point)
    (define-key map (kbd "C-c @")   'helm-ff-run-insert-org-link)
    map)
  "Generic Keymap for files.")


(defface helm-locate-finish
    '((t (:foreground "Green")))
  "Face used in mode line when locate process is finish."
  :group 'helm-locate)


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


(defun helm-locate-create-db-default-function (db-name directory)
  "Default function used to create a locale locate db file.
Argument DB-NAME name of the db file.
Argument DIRECTORY root of file system subtree to scan."
  (format helm-locate-create-db-command db-name directory))

(defvar helm-locate-create-db-function
  #'helm-locate-create-db-default-function
  "Function used to create a locale locate db file.
It should receive the same arguments as
`helm-locate-create-db-default-function'.")

(defun helm-locate-1 (&optional localdb init from-ff default)
  "Generic function to run Locate.
Prefix arg LOCALDB when (4) search and use a local locate db file when it
exists or create it, when (16) force update of existing db file
even if exists.
It have no effect when locate command is 'es'.
INIT is a string to use as initial input in prompt.
See `helm-locate-with-db' and `helm-locate'."
  (require 'helm-mode)
  (helm-locate-set-command)
  (let ((pfn (lambda (candidate)
                 (if (file-directory-p candidate)
                     (message "Error: The locate Db should be a file")
                   (if (= (shell-command
                           (funcall helm-locate-create-db-function
                                    candidate
                                    helm-ff-default-directory))
                          0)
                       (message "New locatedb file `%s' created" candidate)
                     (error "Failed to create locatedb file `%s'" candidate)))))
        (locdb (and localdb
                    (not (string-match "^es" helm-locate-command))
                    (or (and (equal '(4) localdb)
                             (helm-ff-find-locatedb from-ff))
                        (helm-read-file-name
                         "Create Locate Db file: "
                         :initial-input (expand-file-name "locate.db"
                                                          (or helm-ff-default-directory
                                                              default-directory))
                         :preselect helm-locate-db-file-regexp
                         :test (lambda (x)
                                   (if helm-locate-db-file-regexp
                                       ;; Select only locate db files and directories
                                       ;; to allow navigation.
                                       (or (string-match
                                            helm-locate-db-file-regexp x)
                                           (file-directory-p x))
                                     x)))))))
    (when (and locdb (or (equal localdb '(16))
                         (not (file-exists-p locdb))))
      (funcall pfn locdb))
    (helm-locate-with-db (and localdb locdb) init default)))

(defun helm-locate-set-command ()
  "Setup `helm-locate-command' if not already defined."
  (unless helm-locate-command
    (setq helm-locate-command
          (cl-case system-type
            (gnu/linux "locate %s -e --regex %s")
            (berkeley-unix "locate %s %s")
            (windows-nt "es %s %s")
            (t "locate %s %s")))))

(defvar helm-file-name-history nil)
(defun helm-locate-with-db (&optional db initial-input default)
  "Run locate -d DB.
If DB is not given or nil use locate without -d option.
Argument DB can be given as a string or list of db files.
Argument INITIAL-INPUT is a string to use as initial-input.
See also `helm-locate'."
  (require 'helm-files)
  (when (and db (stringp db)) (setq db (list db)))
  (helm-locate-set-command)
  (let ((helm-locate-command
         (if db
             (replace-regexp-in-string
              "locate"
              (format "locate -d %s"
                      (mapconcat 'identity
                                 ;; Remove eventually
                                 ;; marked directories by error.
                                 (cl-loop for i in db
                                       unless (file-directory-p i)
                                       collect i) ":"))
              helm-locate-command)
           helm-locate-command)))
    (setq helm-file-name-history (mapcar 'helm-basename file-name-history))
    (helm :sources 'helm-source-locate
          :buffer " *helm locate*"
          :ff-transformer-show-only-basename nil
          :input initial-input
          :default default
          :history 'helm-file-name-history)))

(defun helm-locate-init ()
  "Initialize async locate process for `helm-source-locate'."
  (let* ((locate-is-es (string-match "\\`es" helm-locate-command))
         (real-locate (string-match "\\`locate" helm-locate-command))
         (case-sensitive-flag (if locate-is-es "-i" ""))
         (ignore-case-flag (if (or locate-is-es
                                   (not real-locate)) "" "-i"))
         (args (split-string helm-pattern " "))
         (cmd (format helm-locate-command
                      (cl-case helm-locate-case-fold-search
                        (smart (let ((case-fold-search nil))
                                 (if (string-match "[[:upper:]]" helm-pattern)
                                     case-sensitive-flag
                                     ignore-case-flag)))
                        (t (if helm-locate-case-fold-search
                               ignore-case-flag
                               case-sensitive-flag)))
                      (concat
                       ;; The pattern itself.
                       (shell-quote-argument (car args)) " "
                       ;; Possible locate args added
                       ;; after pattern, don't quote them.
                       (mapconcat 'identity (cdr args) " ")))))
    (helm-log "Starting helm-locate process")
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd 'face 'font-lock-comment-face) "\n\n"))
    (prog1
        (start-process-shell-command
         "locate-process" helm-buffer
         cmd)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (let* ((err (process-exit-status process))
                (noresult (= err 1)))
           (cond (noresult
                  (with-helm-buffer
                    (unless (cdr helm-sources)
                      (insert (concat "* Exit with code 1, no result found,"
                                      " command line was:\n\n "
                                      cmd)))))
                 ((string= event "finished\n")
                  (with-helm-window
                    (setq mode-line-format
                          '(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format "[Locate process finished - (%s results)]"
                                            (max (1- (count-lines
                                                      (point-min) (point-max)))
                                                 0))
                                    'face 'helm-locate-finish))))
                    (force-mode-line-update)))
                 (t
                  (helm-log "Error: Locate %s"
                            (replace-regexp-in-string "\n" "" event))))))))))

(defclass helm-locate-source (helm-source-async helm-type-file)
  ((init :initform 'helm-locate-set-command)
   (candidates-process :initform 'helm-locate-init)
   (requires-pattern :initform 3)
   (history :initform 'helm-file-name-history)
   (persistent-action :initform 'helm-ff-kill-or-find-buffer-fname)
   (candidate-number-limit :initform 9999)))

(defvar helm-source-locate
  (helm-make-source "Locate" 'helm-locate-source
    :pattern-transformer 'helm-locate-pattern-transformer))

(defun helm-locate-pattern-transformer (pattern)
  (if helm-locate-fuzzy-match
      (cond ((string-match
              " " (replace-regexp-in-string " -b" "" pattern)) pattern)
            ((string-match "\\([^ ]*\\) -b" pattern)
             (concat (helm--mapconcat-pattern
                      (match-string 1 pattern)) " -b"))
            (t (helm--mapconcat-pattern pattern)))
      pattern))

(defun helm-locate-find-dbs-in-projects (&optional update)
  (let* ((pfn (lambda (candidate directory)
                (unless (= (shell-command
                            (funcall helm-locate-create-db-function
                                     candidate
                                     directory))
                           0)
                  (error "Failed to create locatedb file `%s'" candidate)))))
    (cl-loop for p in helm-locate-project-list
             for db = (expand-file-name
                       helm-ff-locate-db-filename
                       (file-name-as-directory p))
             if (and (null update) (file-exists-p db))
             collect db
             else do (funcall pfn db p)
             and collect db)))

;;;###autoload
(defun helm-projects-find-files (update)
  "Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project."
  (interactive "P")
  (helm-locate-set-command)
  (cl-assert (and (string-match-p "\\`locate" helm-locate-command)
                  (executable-find "updatedb"))
             nil "Unsupported locate version")
  (let ((dbs (helm-locate-find-dbs-in-projects update)))
    (if dbs
        (helm-locate-with-db dbs)
        (user-error "No projects found, please setup `helm-locate-project-list'"))))

;;;###autoload
(defun helm-locate-read-file-name (prompt)
  (let* ((src `((name . "Locate read fname")
                (init . helm-locate-set-command)
                (candidates-process . helm-locate-init)
                (action . identity)
                (requires-pattern . 3)
                (history . ,'helm-file-name-history)
                (candidate-transformer . (helm-skip-boring-files
                                          helm-highlight-files))
                (candidate-number-limit . 9999)
                (no-matchplugin))))
    (or (helm :sources src
              :ff-transformer-show-only-basename nil
              :prompt prompt
              :buffer " *helm locate read fname*"
              :resume 'noresume)
        (keyboard-quit))))

;;;###autoload
(defun helm-locate (arg)
  "Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it
if it doesn't exists.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'."
  (interactive "P")
  (setq helm-ff-default-directory default-directory)
  (helm-locate-1 arg nil nil (thing-at-point 'filename)))

(provide 'helm-locate)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-locate.el ends here
