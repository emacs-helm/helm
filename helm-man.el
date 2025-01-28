;;; helm-man.el --- Man and woman UI -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2025 Thierry Volpiatto

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

(defvar woman-topic-all-completions)
(defvar woman-manpath)
(defvar woman-path)
(defvar woman-expanded-directory-path)
(declare-function woman-file-name "woman.el" (topic &optional re-cache))
(declare-function woman-file-name-all-completions "woman.el" (topic))
(declare-function Man-getpage-in-background "man.el" (topic))
(declare-function woman-expand-directory-path "woman.el" (path-dirs path-regexps))
(declare-function woman-topic-all-completions "woman.el" (path))
(declare-function helm-generic-sort-fn "helm-utils.el" (S1 S2))
(declare-function helm-comp-read "helm-mode")

(defgroup helm-man nil
  "Man and Woman applications for Helm."
  :group 'helm)

(defcustom helm-man-or-woman-function 'Man-getpage-in-background
  "Default command to display a man page."
  :group 'helm-man
  :type '(radio :tag "Preferred command to display a man page"
          (const :tag "Man" Man-getpage-in-background)
          (const :tag "Woman" woman)))

(defcustom helm-man-format-switches (cl-case system-type
                                      ((darwin macos) "%s")
                                      (t "-l %s"))
  "Arguments to pass to the `manual-entry' function.
Arguments are passed to `manual-entry' with `format.'"
  :group 'helm-man
  :type 'string)

;; Internal
(defvar helm-man--pages nil
  "All man pages on system.
Will be calculated the first time you invoke Helm with this
source.")

(defvar helm-source-man-pages nil)

(defun helm-man-default-action (candidate)
  "Default action for jumping to a woman or man page from Helm."
  (let ((wfiles (mapcar #'car (woman-file-name-all-completions candidate))))
    (condition-case nil
        (let ((file (if (cdr wfiles)
                        (helm-comp-read "ManFile: " wfiles :must-match t)
                      (car wfiles))))
          (if (eq helm-man-or-woman-function 'Man-getpage-in-background)
              (manual-entry (format helm-man-format-switches file))
            (condition-case nil
                (woman-find-file file)
              ;; If woman is unable to format correctly
              ;; try Man instead.
              (error (kill-buffer)
                     (manual-entry (format helm-man-format-switches file))))))
      ;; If even Man failed with file as argument, try again with Man
      ;; but using Topic candidate instead of the file calculated by
      ;; woman.
      (error (kill-buffer)
             (Man-getpage-in-background candidate)))))

(defun helm-man--init ()
  (require 'woman)
  (require 'helm-utils)
  (unless helm-man--pages
    (setq woman-expanded-directory-path
          (woman-expand-directory-path woman-manpath woman-path))
    (setq woman-topic-all-completions
          (woman-topic-all-completions woman-expanded-directory-path))
    (setq helm-man--pages (mapcar 'car woman-topic-all-completions)))
  (helm-init-candidates-in-buffer 'global helm-man--pages))

(defun helm-man-popup-info (candidate)
  (let ((output (shell-command-to-string (format "man -f '%s'" candidate))))
    (when (string-match (format "\\(%s ?([^(]+)\\) *- ?\\(.*\\)\n" candidate)
                        output)
      (match-string 2 output))))

(defclass helm-man-pages-class (helm-source-in-buffer)
  ((popup-info :initform #'helm-man-popup-info)))

;;;###autoload
(defun helm-man-woman (arg)
  "Preconfigured `helm' for Man and Woman pages.
With a prefix ARG reinitialize the cache.  To have a popup
showing a basic description of selected candidate, turn on
`helm-popup-tip-mode'."
  (interactive "P")
  (when arg (setq helm-man--pages nil))
  (unless helm-source-man-pages
    (setq helm-source-man-pages
          (helm-make-source "Manual Pages" 'helm-man-pages-class
            :init #'helm-man--init
            :persistent-action #'ignore
            :filtered-candidate-transformer
            (lambda (candidates _source)
              (sort candidates #'helm-generic-sort-fn))
            :action  '(("Display Man page" . helm-man-default-action))
            :group 'helm-man)))
  (helm :sources 'helm-source-man-pages
        :buffer "*helm man woman*"))

(provide 'helm-man)

;;; helm-man.el ends here
