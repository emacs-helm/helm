;;; helm-outshine.el --- Helm for Outshine headlines -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Commentary:

;; This file provides commands and supporting functions to navigate a
;; list of Outshine headings in a Helm buffer.

;; Outshine <https://github.com/tj64/outshine> is a package that lets
;; you organize your non-org-mode buffers with commented Org headings
;; and other Org features, especially useful for any kind source code,
;; or any file format which supports comments.

;;; Code:

;;;; Requirements

(require 'helm-org)
(require 'outshine)

;;;; Customization

(defgroup helm-outshine nil
  "Settings for `helm-outshine'."
  :group 'helm)

(defvar helm-outshine-fontify t
  "Fontify Outshine headings in Helm results.")

;;;; Functions

;;;;; Commands

;;;###autoload
(defalias 'helm-outshine 'helm-outshine-in-buffer-headings)

;;;###autoload
(defun helm-outshine-in-buffer-headings ()
  "Preconfigured Helm for Outshine headings in current buffer."
  (interactive)
  (helm :sources (helm-source--outshine-headings-for-files
                  (list (current-buffer)))
        :preselect (helm-outshine--in-buffer-preselect)
        :buffer "*Helm Outshine in-buffer*"))

;;;;; Support functions

(defun helm-outshine--goto-marker (marker)
  "Switch to MARKER's buffer and go to it."
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker)))

(defun helm-source--outshine-headings-for-files (filenames &optional parents)
  "Return helm-sync-source for Outshine headings in current buffer."
  (helm-build-sync-source " Outshine headings in-buffer"
    :candidates (apply #'append (mapcar 'helm-outshine--get-candidates-in-file filenames))
    :action '(("Go to heading" . helm-outshine--goto-marker))))

(defun helm-outshine--get-candidates-in-file (filename &optional regexp)
  "Return Outshine heading candidates in FILENAME.
FILENAME may be a path or a buffer."
  (with-current-buffer (pcase filename
                         ((pred bufferp) (buffer-name filename))
                         ((pred stringp) (find-file-noselect filename)))
    (let* ((heading-regexp (or regexp
                               (concat "^\\("
                                       (mapconcat (lambda (s)
                                                    (s-trim (car s)))
                                                  outline-promotion-headings
                                                  "\\|")
                                       "\\)"
                                       "\s+\\(.*\\)$")))
           (match-fn (if helm-outshine-fontify
                         #'match-string
                       #'match-string-no-properties))
           (search-fn (lambda ()
                        (re-search-forward heading-regexp nil t))))
      (save-excursion
        (save-restriction
          (goto-char (point-min))
          (cl-loop with width = (window-width (helm-window))
                   while (funcall search-fn)
                   for beg = (point-at-bol)
                   for end = (point-at-eol)
                   when (and helm-outshine-fontify
                             (null (text-property-any
                                    beg end 'fontified t)))
                   do (jit-lock-fontify-now beg end)
                   for level = (length (match-string-no-properties 1))
                   for heading = (if regexp
                                     (funcall match-fn 0)
                                   (concat (match-string 1) " " (funcall match-fn 2)))
                   if (or regexp
                          (and (>= level helm-org-headings-min-depth)
                               (<= level helm-org-headings-max-depth)))
                   collect `(,heading . ,(point-marker))))))))

(defun helm-outshine--in-buffer-preselect ()
  "Return string containing current or previous visible heading for preselecting in Helm buffer."
  (if (outline-on-heading-p)
      (buffer-substring-no-properties (point-at-bol) (point-at-eol))
    (save-excursion
      (outline-previous-visible-heading 1)
      (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))

;;;; Footer

(provide 'helm-outshine)

;;; helm-outshine.el ends here
