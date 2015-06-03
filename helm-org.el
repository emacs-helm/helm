;;; helm-org.el --- Helm for org headlines and keywords completion -*- lexical-binding: t -*-

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

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'org)

(defgroup helm-org nil
  "Org related functions for helm."
  :group 'helm)

(defcustom helm-org-headings-fontify nil
  "Fontify org buffers before parsing them.
This reflect fontification in helm-buffer when non--nil.
NOTE: This will be slow on large org buffers."
  :group 'helm-org
  :type 'boolean)

;; Internal
(defvar helm-org-headings--nofilename nil)

;;; Org capture templates
;;
;;
(defvar org-capture-templates)
(defun helm-source-org-capture-templates ()
  (helm-build-sync-source "Org Capture Templates:"
    :candidates (cl-loop for template in org-capture-templates
                         collect `(,(nth 1 template) . ,(nth 0 template)))
    :action '(("Do capture" . (lambda (template-shortcut)
                                (org-capture nil template-shortcut))))))

;;; Org headings
;;
;;
(defun helm-org-goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context)
  (org-show-entry))

(cl-defun helm-source-org-headings-for-files (filenames
                                              &optional (min-depth 1) (max-depth 8))
  (helm-build-sync-source "Org Headings"
    :candidates (helm-org-get-candidates filenames min-depth max-depth)
    :action '(("Go to line" . helm-org-goto-marker)
              ("Refile to this heading" . helm-org-heading-refile)
              ("Insert link to this heading"
               . helm-org-insert-link-to-heading-at-marker))))

(defun helm-org-insert-link-to-heading-at-marker (marker)
  (with-current-buffer (marker-buffer marker)
    (let ((heading-name (save-excursion (goto-char (marker-position marker))
                                        (nth 4 (org-heading-components))))
          (file-name (buffer-file-name)))
      (with-helm-current-buffer
        (org-insert-link
         file-name (concat "file:" file-name "::*" heading-name))))))

(defun helm-org-heading-refile (marker)
  (with-helm-current-buffer
    (org-cut-subtree))
  (let ((target-level (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (org-current-level))))
    (helm-org-goto-marker marker)
    (org-end-of-subtree t t)
    (org-paste-subtree (+ target-level 1))))

(defun helm-org-get-candidates (filenames min-depth max-depth)
  (apply #'append
   (mapcar (lambda (filename)
             (helm-get-org-candidates-in-file
              filename min-depth max-depth
              helm-org-headings-fontify
              helm-org-headings--nofilename))
           filenames)))

(defun helm-get-org-candidates-in-file (filename min-depth max-depth
                                        &optional fontify nofname)
  (with-current-buffer (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename)))
    (and fontify (jit-lock-fontify-now))
    (let ((match-fn (if fontify 'match-string 'match-string-no-properties)))
      (save-excursion
        (goto-char (point-min))
        (cl-loop with width = (window-width)
                 while (re-search-forward org-complex-heading-regexp nil t)
                 if (let ((num-stars (length (match-string-no-properties 1))))
                      (and (>= num-stars min-depth) (<= num-stars max-depth)))
                 collect `(,(let ((heading (funcall match-fn 4))
                                  (file (unless nofname
                                          (concat (helm-basename filename) ":")))
                                  (level (length (match-string-no-properties 1))))
                              (org-format-outline-path
                               (append (org-get-outline-path t level heading)
                                       (list heading)) width file))
                           . ,(point-marker)))))))

;;;###autoload
(defun helm-org-agenda-files-headings ()
  "Preconfigured helm for org files headings."
  (interactive)
  (helm :sources (helm-source-org-headings-for-files (org-agenda-files))
        :candidate-number-limit 99999
        :buffer "*helm org headings*"))

;;;###autoload
(defun helm-org-in-buffer-headings ()
  "Preconfigured helm for org buffer headings."
  (interactive)
  (let ((helm-org-headings--nofilename t))
    (helm :sources (helm-source-org-headings-for-files
                    (list (current-buffer)))
          :candidate-number-limit 99999
          :buffer "*helm org inbuffer*")))

;;;###autoload
(defun helm-org-capture-templates ()
  "Preconfigured helm for org templates."
  (interactive)
  (helm :sources (helm-source-org-capture-templates)
        :candidate-number-limit 99999
        :buffer "*helm org capture templates*"))


(provide 'helm-org)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-org.el ends here
