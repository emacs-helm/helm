;;; helm-id-utils.el --- Helm interface for id-utils. -*- lexical-binding: t -*-

;; Copyright (C) 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(require 'helm-grep)

(defgroup helm-id-utils nil
  "ID-Utils related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-gid-db-file-name "ID"
  "Name of a database file created by `mkid' command from `ID-utils'."
  :group 'helm-id-utils
  :type 'string)

(defclass helm-gid-source (helm-source-async)
  ((header-name
    :initform
    (lambda (name)
      (concat name " [" (helm-attr 'db-dir) "]")))
   (db-dir :initarg :db-dir
           :initform nil
           :custom string
           :documentation " Location of ID file.")
   (candidates-process
    :initform
    (lambda ()
      (let ((proc (start-process
                   "gid" nil "gid"
                   "-r" helm-pattern)))
        (set (make-local-variable 'helm-grep-last-cmd-line)
             (format "gid -r %s" helm-pattern))
        (prog1 proc
          (set-process-sentinel
           proc (lambda (_process event)
                  (when (string= event "finished\n")
                    (with-helm-window
                      (setq mode-line-format
                            '(" " mode-line-buffer-identification " "
                              (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                              (:eval (propertize
                                      (format "[Helm Gid process finished - (%s results)]" 
                                              (max (1- (count-lines
                                                        (point-min) (point-max)))
                                                   0))
                                      'face 'helm-locate-finish))))
                      (force-mode-line-update))
                    (helm-log "Error: Gid %s"
                              (replace-regexp-in-string "\n" "" event)))))))))
  (filter-one-by-one :initform 'helm-grep-filter-one-by-one)
  (candidate-number-limit :initform 99999)
  (action :initform (helm-make-actions
                     "Find File" 'helm-grep-action
                     "Find file other frame" 'helm-grep-other-frame
                     (lambda () (and (locate-library "elscreen")
                                     "Find file in Elscreen"))
                     'helm-grep-jump-elscreen
                     "Save results in grep buffer" 'helm-grep-save-results
                     "Find file other window" 'helm-grep-other-window))
  (persistent-action :initform 'helm-grep-persistent-action)
  (history :initform 'helm-grep-history)
  (nohighlight :initform t)
  (requires-pattern :initform 2)))

;;;###autoload
(defun helm-gid ()
  "Helm UI for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid'
above `default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc...
See <https://www.gnu.org/software/idutils/>."
  (interactive)
  (let* ((db (locate-dominating-file
              default-directory
              helm-gid-db-file-name))
         (helm-grep-default-directory-fn
          (lambda () default-directory)))
    (cl-assert db nil "No DataBase found, create one with `mkid'")
    (helm :sources (helm-make-source "Gid" 'helm-gid-source
                     :db-dir db)
          :buffer "*helm gid*"
          :keymap helm-grep-map
          :truncate-lines t)))

(provide 'helm-id-utils)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-id-utils ends here
