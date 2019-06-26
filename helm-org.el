;;; helm-org.el --- Helm for Org headline and keyword completion -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2019 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;; Load org-with-point-at macro when compiling
(eval-when-compile
  (require 'org-macs))


(defgroup helm-org nil
  "Org related functions for Helm."
  :group 'helm)

(defcustom helm-org-headings-fontify nil
  "Whether to fontify buffers before parsing them.
NOTE: This will be slow on large buffers."
  :type 'boolean)

(defcustom helm-org-format-outline-path nil
  "Whether to show the headline's ancestors as path."
  :type 'boolean)

(defcustom helm-org-show-filename nil
  "Whether to show filenames in `helm-org-agenda-files-headings'.
NOTE: This has no effect in `helm-org-in-buffer-headings'."
  :type 'boolean)

(defcustom helm-org-headings-actions
  '(("Go to heading" . helm-org-goto-marker)
    ("Open in indirect buffer `C-c i'" . helm-org--open-heading-in-indirect-buffer)
    ("Refile heading(s) (marked-to-selected|current-to-selected) `C-c w'" . helm-org--refile-heading-to)
    ("Insert link to this heading `C-c l'" . helm-org-insert-link-to-heading-at-marker))
  "Default actions alist for `helm-source-org-headings-for-files'."
  :type '(alist :key-type string :value-type function))

(defcustom helm-org-truncate-lines t
  "Truncate lines when non-nil."
  :type 'boolean)

(defcustom helm-org-ignore-autosaves nil
  "Whether to ignore auto-save files when starting
`helm-org-agenda-files-headings'."
  :type 'boolean)

(defvar helm-org-headings-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c i") 'helm-org-run-open-heading-in-indirect-buffer)
    (define-key map (kbd "C-c w") 'helm-org-run-refile-heading-to)
    (define-key map (kbd "C-c l") 'helm-org-run-insert-link-to-heading-at-marker)
    map)
  "Keymap for `helm-source-org-headings-for-files'.")


;;; Org capture templates
;;
;;

(defvar org-capture-templates)
(defvar org-capture-templates-contexts)
(declare-function org-capture-upgrade-templates "org-capture" (templates))

(defun helm-source-org-capture-templates ()
  (helm-build-sync-source "Org Capture Templates"
    :candidates (cl-loop for template in (org-contextualize-keys
                                          (org-capture-upgrade-templates org-capture-templates)
                                          org-capture-templates-contexts)
                         collect (cons (nth 1 template) (nth 0 template)))
    :action '(("Select template" . (lambda (template-shortcut)
                                     (org-capture nil template-shortcut))))))


;;; Org headings
;;
;;

(defun helm-org-format-heading ()
  "Format the candidate's display conditionally."
  (cond ((and helm-org-headings-fontify
              helm-org-format-outline-path)
         (org-format-outline-path (org-get-outline-path t t)))
        (helm-org-format-outline-path
         (mapconcat #'identity
                    (org-get-outline-path t t) "/"))
        (helm-org-headings-fontify
         (match-string 0))
        (t
         (match-string-no-properties 0))))

(defun helm-org-get-parent-headings ()
  "Return all parent headings of the current heading.
Also return their position in the buffer as marker objects."
  (let (candidates)
    (save-excursion
      (while (org-up-heading-safe)
        (push (cons (when (looking-at org-complex-heading-regexp)
                      (helm-org-format-heading))
                    (point-marker))
              candidates)))
    candidates))

(defun helm-org--get-candidates-in-file ()
  "Return a list of candidates and their position in the buffer."
  (let (candidates)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-complex-heading-regexp nil t)
	(when helm-org-headings-fontify
          (jit-lock-fontify-now (line-beginning-position)
                                (line-end-position)))
	(push (cons (helm-org-format-heading)
		    (save-excursion
		      (forward-line 0)
		      (point-marker)))
	      candidates))
      (nreverse candidates))))

(defun helm-org-get-candidates (filenames)
  "Return a list of candidates in FILENAMES."
  (apply #'append
         (mapcar (lambda (filename)
		   (with-current-buffer
		       (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename t)))
		     (helm-org--get-candidates-in-file)))
                 filenames)))

(defun helm-org-candidate-transformer (candidates)
  "Get the CANDIDATES' filenames and display them conditionally.
That is, display them only if `helm-org-show-filename' is non-nil and
when called from the `helm-org-agenda-files-headings' command."
  (cl-loop for i in candidates
           for heading = (car i)
           for marker  = (cdr i)
           when (and helm-org-show-filename
                     (eq this-command 'helm-org-agenda-files-headings))
           for filename = (helm-basename
                           (buffer-file-name (marker-buffer marker)))
           for display = (concat filename heading)
           collect (cons display marker)))

(defun helm-org-in-buffer-preselect ()
  "Preselect the heading at point."
  (if (org-at-heading-p)
      (buffer-substring-no-properties (point-at-bol) (point-at-eol))
    (save-excursion
      (outline-previous-visible-heading 1)
      (buffer-substring-no-properties (point-at-bol) (point-at-eol)))))

(defun helm-org-goto-marker (marker)
  "Go to MARKER showing the entry's context, body and subheadings."
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context)
  (re-search-backward "^\\*+ " nil t)
  (org-show-entry)
  (org-show-children))

(defun helm-org--open-heading-in-indirect-buffer (marker)
  "Go to MARKER and create an indirect buffer of the current subtree."
  (switch-to-buffer (marker-buffer marker))
  (goto-char (marker-position marker))
  (org-show-context)
  (org-tree-to-indirect-buffer)
  ;; Put the non-indirect buffer at the bottom of the prev-buffers
  ;; list so it won't be selected when the indirect buffer is killed
  (set-window-prev-buffers nil (append (cdr (window-prev-buffers))
                                       (car (window-prev-buffers)))))

(defun helm-org-run-open-heading-in-indirect-buffer ()
  "Open selected subtree in an indirect buffer."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action #'helm-org--open-heading-in-indirect-buffer)))
(put 'helm-org-run-open-heading-in-indirect-buffer 'helm-only t)

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
   (help-message :initform 'helm-org-headings-help-message)
   (action :initform 'helm-org-headings-actions)
   (keymap :initform 'helm-org-headings-map)
   (group :initform 'helm-org)))

(defmethod helm--setup-source :after ((source helm-org-headings-class))
  (let ((parents (slot-value source 'parents)))
    (setf (slot-value source 'candidate-transformer)
          (lambda (candidates)
            (let ((cands (helm-org-get-candidates candidates parents)))
              (if parents (nreverse cands) cands))))))

(defun helm-source-org-headings-for-files (filenames &optional parents)
  (helm-make-source "Org Headings" 'helm-org-headings-class
    :filtered-candidate-transformer 'helm-org-startup-visibility
    :parents parents
    :candidates filenames))
(defun helm-org--get-candidates-in-file (filename &optional fontify nofname parents)
  (with-current-buffer (pcase filename
                         ((pred bufferp) filename)
                         ((pred stringp) (find-file-noselect filename t)))
    (let ((match-fn (if fontify
                        #'match-string
                      #'match-string-no-properties))
          (search-fn (lambda ()
                       (re-search-forward
                        org-complex-heading-regexp nil t)))
          (file (unless (or (bufferp filename) nofname)
                  (concat (helm-basename filename) ":"))))
      (when parents
        (add-function :around (var search-fn)
                      (lambda (old-fn &rest args)
                                (when (org-up-heading-safe)
                                  (apply old-fn args)))))
      (save-excursion
        (save-restriction
          (unless (and (bufferp filename)
                       (buffer-base-buffer filename))
            ;; Only widen direct buffers, not indirect ones.
            (widen))
          (unless parents (goto-char (point-min)))
          ;; clear cache for new version of org-get-outline-path
          (and (boundp 'org-outline-path-cache)
               (setq org-outline-path-cache nil))
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
                                    ;; org-get-outline-path changed in signature and behaviour since org's
                                    ;; commit 105a4466971. Let's fall-back to the new version in case
                                    ;; of wrong-number-of-arguments error.
                                    (condition-case nil
                                        (append (apply #'org-get-outline-path
                                                       (unless parents
                                                         (list t level heading)))
                                                (list heading))
                                      (wrong-number-of-arguments
                                       (org-get-outline-path t t)))
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

(defun helm-org-run-insert-link-to-heading-at-marker ()
  "Insert link with the selected heading as its target."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     'helm-org-insert-link-to-heading-at-marker)))

(defun helm-org--refile-heading-to (marker)
  "Refile headings to heading at MARKER.
If multiple candidates are marked in the Helm session, they will
all be refiled.  If no headings are marked, the selected heading
will be refiled."
  (let* ((victims (with-helm-buffer (helm-marked-candidates)))
         (buffer (marker-buffer marker))
         (filename (buffer-file-name buffer))
         (rfloc (list nil filename nil marker)))
    (when (and (= 1 (length victims))
               (equal (helm-get-selection) (car victims)))
      ;; No candidates are marked; we are refiling the entry at point
      ;; to the selected heading
      (setq victims (list (point))))
    ;; Probably best to check that everything returned a value
    (when (and victims buffer filename rfloc)
      (cl-loop for victim in victims
               do (org-with-point-at victim
                    (org-refile nil nil rfloc))))))

(defun helm-org-run-refile-heading-to ()
  "Refile one or more entries to the selected heading."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org--refile-heading-to)))
(put 'helm-org-run-refile-heading-to 'helm-only t)

;;;###autoload
(defun helm-org-agenda-files-headings ()
  "Preconfigured Helm for agenda files."
  (interactive)
  (let ((autosaves (cl-loop for f in (org-agenda-files)
                            when (file-exists-p
                                  (expand-file-name
                                   (concat "#" (helm-basename f) "#")
                                   (helm-basedir f)))
                            collect (helm-basename f))))
    (when (or (null autosaves)
              helm-org-ignore-autosaves
              (y-or-n-p (format "%s have auto save data, continue?"
                                (mapconcat 'identity autosaves ", "))))
      (helm :sources (helm-source-org-headings-for-files (org-agenda-files))
            :candidate-number-limit 99999
            :truncate-lines helm-org-truncate-lines
            :buffer "*helm org headings*"))))

;;;###autoload
(defun helm-org-in-buffer-headings ()
  "Preconfigured Helm for buffer headings."
  (interactive)
  (let (helm-org-show-filename)
    (helm :sources (helm-source-org-headings-for-files
                    (list (current-buffer)))
          :candidate-number-limit 99999
          :preselect (helm-org-in-buffer-preselect)
          :truncate-lines helm-org-truncate-lines
          :buffer "*helm org inbuffer*")))

;;;###autoload
(defun helm-org-parent-headings ()
  "Preconfigured Helm for parent headings of the current heading."
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
  "Preconfigured Helm for capture templates."
  (interactive)
  (require 'org-capture)
  (helm :sources (helm-source-org-capture-templates)
        :candidate-number-limit 99999
        :truncate-lines helm-org-truncate-lines
        :buffer "*helm org capture templates*"))


;;; Org tag completion
;;
;; Based on code from Anders Johansson posted on 3 Mar 2016 at
;; <https://groups.google.com/d/msg/emacs-helm/tA6cn6TUdRY/G1S3TIdzBwAJ>

(defvar crm-separator)

;;;###autoload
(defun helm-org-completing-read-tags (prompt collection pred req initial
                                      hist def inherit-input-method _name _buffer)
  "Completing read function for Org tags.

This function is used as a `completing-read' function in
`helm-completing-read-handlers-alist' by `org-set-tags' and
`org-capture'.

NOTE: Org tag completion will work only if Org's fast tag selection is
disabled. See (info \"(org) setting tags\")."
  (if (not (string= "Tags: " prompt))
      ;; Not a tags prompt.  Use normal completion by calling
      ;; `org-icompleting-read' again without this function in
      ;; `helm-completing-read-handlers-alist'
      (let ((helm-completing-read-handlers-alist
             (rassq-delete-all
              'helm-org-completing-read-tags
              (copy-alist helm-completing-read-handlers-alist))))
        (org-icompleting-read
         prompt collection pred req initial hist def inherit-input-method))
    ;; Tags prompt
    (let* ((curr (and (stringp initial)
                      (not (string= initial ""))
                      (org-split-string initial ":")))
           (table   (delete curr
                            (org-uniquify
                             (mapcar 'car org-last-tags-completion-table))))
           (crm-separator ":\\|,\\|\\s-"))
      (cl-letf (((symbol-function 'crm-complete-word)
                 'self-insert-command))
        (mapconcat 'identity
                   (completing-read-multiple
                    prompt table pred nil initial hist def)
                   ":")))))

(provide 'helm-org)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-org.el ends here
