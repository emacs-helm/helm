;;; helm-org.el --- Helm for org headlines and keywords completion -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2016 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-utils)
(require 'org)

(declare-function org-agenda-switch-to "org-agenda.el")

(defgroup helm-org nil
  "Org related functions for helm."
  :group 'helm)

(defcustom helm-org-headings-fontify nil
  "Fontify org buffers before parsing them.
This reflect fontification in helm-buffer when non--nil.
NOTE: This will be slow on large org buffers."
  :group 'helm-org
  :type 'boolean)

(defcustom helm-org-format-outline-path nil
  "Show all org level as path."
  :group 'helm-org
  :type 'boolean)

(defcustom helm-org-show-filename nil
  "Show org filenames in `helm-org-agenda-files-headings' when non--nil.
Note this have no effect in `helm-org-in-buffer-headings'."
  :group 'helm-org
  :type 'boolean)

(defcustom helm-org-headings-min-depth 1
  "Minimum depth of org headings to start with."
  :group 'helm-org
  :type 'integer)

(defcustom helm-org-headings-max-depth 8
  "Go down to this maximum depth of org headings."
  :group 'helm-org
  :type 'integer)

(defcustom helm-org-headings-actions
  '(("Go to heading" . helm-org-goto-marker)
    ("Open in indirect buffer `C-RET'" . helm-org--open-heading-in-indirect-buffer)
    ("Refile to this heading `C-w`''" . helm-org-heading-refile)
    ("Insert link to this heading `C-l`''" . helm-org-insert-link-to-heading-at-marker))
  "Default actions alist for
  `helm-source-org-headings-for-files'."
  :group 'helm-org
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-truncate-lines t
  "Truncate org-header-lines when non-nil"
  :type 'boolean
  :group 'helm-org)

;;; Org capture templates
;;
;;
(defvar org-capture-templates)
(defun helm-source-org-capture-templates ()
  (helm-build-sync-source "Org Capture Templates:"
    :candidates (cl-loop for template in org-capture-templates
                         collect (cons (nth 1 template) (nth 0 template)))
    :action '(("Do capture" . (lambda (template-shortcut)
                                (org-capture nil template-shortcut))))))

;;; Org headings
;;
;;
(defun helm-org-goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context)
  (re-search-backward "^\\*+ " nil t)
  (org-show-entry))

(defun helm-org--open-heading-in-indirect-buffer (marker)
  (helm-org-goto-marker marker)
  (org-tree-to-indirect-buffer)

  ;; Put the non-indirect buffer at the bottom of the prev-buffers
  ;; list so it won't be selected when the indirect buffer is killed
  (set-window-prev-buffers nil (append (cdr (window-prev-buffers))
                                       (car (window-prev-buffers)))))

(defun helm-org--run-open-heading-in-indirect-buffer ()
  "Open selected Org heading in an indirect buffer."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-org--open-heading-in-indirect-buffer)))
(put 'helm-org--run-open-heading-in-indirect-buffer 'helm-only t)

(defvar helm-org-headings-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-return>") 'helm-org--run-open-heading-in-indirect-buffer)
    (define-key map (kbd "C-w") 'helm-org-heading-refile)
    (define-key map (kbd "C-l") 'helm-org-insert-link-to-heading-at-marker)
    map)
  "Keymap for `helm-source-org-headings-for-files'.")

(defclass helm-org-headings-class (helm-source-sync)
  ((parents
    :initarg :parents
    :initform nil
    :custom boolean)
   (match :initform
          (lambda (candidate)
            (string-match
             helm-pattern
             (helm-aif (get-text-property 0 'helm-real-display candidate)
                 it
               candidate))))
   (action :initform 'helm-org-headings-actions)
   (keymap :initform 'helm-org-headings-map)))

(defmethod helm--setup-source :after ((source helm-org-headings-class))
  (let ((parents (slot-value source 'parents)))
    (set-slot-value source 'candidate-transformer
                    (lambda (candidates)
                      (let ((cands (helm-org-get-candidates candidates parents)))
                        (if parents (nreverse cands) cands))))))

(defun helm-source-org-headings-for-files (filenames &optional parents)
  (helm-make-source "Org Headings" 'helm-org-headings-class
    :parents parents
    :candidates filenames))

(defun helm-org-get-candidates (filenames &optional parents)
  (apply #'append
         (mapcar (lambda (filename)
                   (helm-org--get-candidates-in-file
                    filename
                    helm-org-headings-fontify
                    (or parents (null helm-org-show-filename))
                    parents))
                 filenames)))

(defun helm-org--get-candidates-in-file (filename &optional fontify nofname parents)
  (with-current-buffer (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename)))
    (let ((match-fn (if fontify
                        #'match-string
                      #'match-string-no-properties))
          (search-fn (lambda ()
                       (re-search-forward
                        org-complex-heading-regexp nil t)))
          (file (unless nofname
                  (concat (helm-basename filename) ":"))))
      (when parents
        (add-function :around (var search-fn)
                      (lambda (old-fn &rest args)
                                (when (org-up-heading-safe)
                                  (apply old-fn args)))))
      (save-excursion
        (save-restriction
          (widen)
          (unless parents (goto-char (point-min)))
          (cl-loop with width = (window-width (helm-window))
                   while (funcall search-fn)
                   for beg = (point-at-bol)
                   for end = (point-at-eol)
                   when (and fontify
                             (null (text-property-any
                                    beg end 'fontified t)))
                   do (jit-lock-fontify-now beg end)
                   for level = (length (match-string-no-properties 1))
                   for heading = (funcall match-fn 4)
                   if (and (>= level helm-org-headings-min-depth)
                           (<= level helm-org-headings-max-depth))
                   collect `(,(propertize
                               (if helm-org-format-outline-path
                                   (org-format-outline-path
                                    (append (apply #'org-get-outline-path
                                                   (unless parents
                                                     (list t level heading)))
                                            (list heading))
                                    width file)
                                   (if file
                                       (concat file (funcall match-fn  0))
                                       (funcall match-fn  0)))
                               'helm-real-display heading)
                              . ,(point-marker))))))))

(defun helm-org-insert-link-to-heading-at-marker (marker)
  (with-current-buffer (marker-buffer marker)
    (let ((heading-name (save-excursion (goto-char (marker-position marker))
                                        (nth 4 (org-heading-components))))
          (file-name (buffer-file-name)))
      (with-helm-current-buffer
        (org-insert-link
         file-name (concat "file:" file-name "::*" heading-name))))))

(defun helm-org-heading-refile (marker)
  (save-selected-window
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-switch-to))
    (org-cut-subtree)
    (let ((target-level (with-current-buffer (marker-buffer marker)
                          (goto-char (marker-position marker))
                          (org-current-level))))
      (helm-org-goto-marker marker)
      (org-end-of-subtree t t)
      (org-paste-subtree (+ target-level 1)))))

;;;###autoload
(defun helm-org-agenda-files-headings ()
  "Preconfigured helm for org files headings."
  (interactive)
  (helm :sources (helm-source-org-headings-for-files (org-agenda-files))
        :candidate-number-limit 99999
        :truncate-lines helm-org-truncate-lines
        :buffer "*helm org headings*"))

;;;###autoload
(defun helm-org-in-buffer-headings ()
  "Preconfigured helm for org buffer headings."
  (interactive)
  (let ((helm-org-show-filename nil))
    (helm :sources (helm-source-org-headings-for-files
                    (list (current-buffer)))
          :candidate-number-limit 99999
          :truncate-lines helm-org-truncate-lines
          :buffer "*helm org inbuffer*")))

;;;###autoload
(defun helm-org-parent-headings ()
  "Preconfigured helm for org headings that are parents of the
current heading."
  (interactive)
  ;; Use a large max-depth to ensure all parents are displayed.
  (let ((helm-org-headings-min-depth 1)
        (helm-org-headings-max-depth  50))
    (helm :sources (helm-source-org-headings-for-files
                    (list (current-buffer)) t)
          :candidate-number-limit 99999
          :truncate-lines helm-org-truncate-lines
          :buffer "*helm org parent headings*")))

;;;###autoload
(defun helm-org-capture-templates ()
  "Preconfigured helm for org templates."
  (interactive)
  (helm :sources (helm-source-org-capture-templates)
        :candidate-number-limit 99999
        :truncate-lines helm-org-truncate-lines
        :buffer "*helm org capture templates*"))


(provide 'helm-org)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-org.el ends here
