;;; helm-org.el --- Helm for org headlines and keywords completion -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-plugin)
(require 'org)

;;; Org headings
;;
;;
(defun helm-org-goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context))

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
    (goto-char (marker-position marker))
    (let ((heading-name (nth 4 (org-heading-components)))
          (file-name buffer-file-name))
      (message heading-name)
      (message file-name)
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
              filename min-depth max-depth))
           filenames)))

(defun helm-get-org-candidates-in-file (filename min-depth max-depth)
  (with-current-buffer (find-file-noselect filename)
    (save-excursion
      (goto-char (point-min))
      (cl-loop while (re-search-forward org-complex-heading-regexp nil t)
               if (let ((num-stars (length (match-string-no-properties 1))))
                    (and (>= num-stars min-depth) (<= num-stars max-depth)))
               collect `(,(match-string-no-properties 0) . ,(point-marker))))))

;;;###autoload
(defun helm-org-agenda-files-headings ()
  (interactive)
  (helm :sources (helm-source-org-headings-for-files (org-agenda-files))
        :candidate-number-limit 99999
        :buffer "*helm org headings*"))

;;;###autoload
(defun helm-org-in-buffer-headings ()
  (interactive)
  (helm :sources (helm-source-org-headings-for-files
                  (list (buffer-file-name (current-buffer))))
        :candidate-number-limit 99999
        :buffer "*helm org inbuffer*"))

;; (defvar helm-documentation-filecache nil)
;; (defvar helm-documentation-directory "~/.emacs.d/helm-documentation/")
;; (defun helm-documentation ()
;;   (interactive)
;;   (unless helm-documentation-filecache
;;     (setq helm-documentation-filecache
;;           (directory-files helm-documentation-directory t "\\.org\\'")))
;;   (helm :sources (helm-source-org-headings-for-files
;;                   helm-documentation-filecache)
;;         :candidate-number-limit 99999
;;         :buffer "*helm org doc*"))

(provide 'helm-org)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-org.el ends here
