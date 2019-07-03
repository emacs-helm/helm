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
    ("Change state `C-c C-t'" . helm-org-change-state)
    ("Set tags `C-c C-c'" . helm-org-set-tags)
    ("Archive subtree `C-c C-x C-a'" . helm-org-archive-subtree)
    ("Open in indirect buffer `C-c C-i'" . helm-org--open-heading-in-indirect-buffer)
    ("Refile heading(s) `C-c C-w'" . helm-org--refile-heading-to)
    ("Insert link to this heading `C-c C-l'" . helm-org-insert-link-to-heading-at-marker)
    ("Schedule item `C-c C-s, C-u to remove'" . helm-org-schedule)
    ("Set deadline `C-c C-d, C-u to remove'" . helm-org-deadline))
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
    (define-key map (kbd "C-c C-t") 'helm-org-run-change-state)
    (define-key map (kbd "C-c C-c") 'helm-org-run-set-tags)
    (define-key map (kbd "C-c C-x C-a") 'helm-org-run-archive-subtree)
    (define-key map (kbd "C-c C-i") 'helm-org-run-open-heading-in-indirect-buffer)
    (define-key map (kbd "C-c C-w") 'helm-org-run-refile-heading-to)
    (define-key map (kbd "C-c C-l") 'helm-org-run-insert-link-to-heading-at-marker)
    (define-key map (kbd "C-c C-s") 'helm-org-run-org-schedule)
    (define-key map (kbd "C-c C-d") 'helm-org-run-org-deadline)
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

(defun helm-source-org-headings-for-files (filenames &optional parents)
  (helm-make-source "Org Headings" 'helm-org-headings-class
    :candidates (lambda ()
                  (if parents
                      (with-helm-current-buffer
                        (helm-org-get-parent-headings))
                    (helm-org-get-candidates filenames)))))

(defclass helm-org-headings-class (helm-source-sync)
  ((match :initform (lambda (candidate)
                      (string-match
                       helm-pattern
                       (helm-aif (get-text-property 0 'helm-real-display candidate)
                           it
                         candidate))))
   (candidate-transformer :initform 'helm-org-candidate-transformer)
   (help-message :initform 'helm-org-headings-help-message)
   (action :initform 'helm-org-headings-actions)
   (keymap :initform 'helm-org-headings-map)
   (group :initform 'helm-org)))


;; Actions
;;
;;

(defvar helm-org-switch-to-buffer-p nil
  "Whether to show the editing buffer.")

(defmacro helm-org-execute (markers &rest body)
  "Jump to MARKERS and execute BODY."
  (declare (indent 1))
  `(dolist (marker (if (listp ,markers)
                       ,markers
                     (list ,markers)))
     (if helm-org-switch-to-buffer-p
         (switch-to-buffer (marker-buffer marker))
       (set-buffer (marker-buffer marker)))
     (goto-char (marker-position marker))
     (org-show-context)
     ,@body))

(defun helm-org-goto-marker (marker)
  "Go to MARKER showing the entry's context, body and subheadings."
  (let ((helm-org-switch-to-buffer-p t))
    (helm-org-execute marker
      (org-show-entry)
      (org-show-children))))

(defun helm-org-change-state (marker)
  "Change the TODO state of marked candidates."
  (let* ((markers (helm-marked-candidates))
         (helm-org-switch-to-buffer-p t)
         (keywords (with-current-buffer (marker-buffer marker)
                     (mapcar (lambda (kwd)
                               (let ((face (org-get-todo-face kwd)))
                                 (list (org-add-props kwd nil 'face face))))
                             org-todo-keywords-1))))
    (helm :sources `(,(helm-build-sync-source "Change State"
                        :candidates keywords
                        :fuzzy-match t
                        :action (lambda (state)
                                  (helm-org-execute markers
                                    (org-todo state))))
                     ,(helm-build-sync-source "Remove State"
                        :candidates (list "clear")
                        :fuzzy-match t
                        :action (lambda (_candidate)
                                  (helm-org-execute markers
                                    (org-todo 'none))))))))

(defun helm-org-run-change-state ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-change-state)))
(put 'helm-org-run-change-state 'helm-only t)

(defun helm-org-set-tags (_marker)
  "Add or remove tags of marked candidates, replacing current tags."
  (let ((markers (helm-marked-candidates))
        (helm-org-switch-to-buffer-p t)
        tags
        local-tags)
    (helm-org-execute markers
      (setq tags (apply #'append (org-get-buffer-tags))
            local-tags (append local-tags (org-get-tags nil t))))
    (helm :sources `(,(helm-build-sync-source "Add Tags"
			:candidates tags
			:action (lambda (_candidate)
				  (helm-org-execute markers
				    (org-set-tags (helm-marked-candidates)))))
		     ,(helm-build-sync-source "Remove Tags"
			:candidates local-tags
			:action (lambda (_candidate)
                                  (let ((marked-tags (helm-marked-candidates)))
                                    (helm-org-execute markers
                                      ;; Remove marked tags but keep
                                      ;; the rest
                                      (org-set-tags
                                       (cl-remove-if (lambda (tag)
                                                       (member tag marked-tags))
                                                     (org-get-tags nil t)))))))))))

(defun helm-org-run-set-tags ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-set-tags)))
(put 'helm-org-run-set-tags 'helm-only t)

(defun helm-org-archive-subtree (_marker)
  "Archive marked candidates with the default command.
This command is set with the variable `org-archive-default-command'."
  (let ((markers (helm-marked-candidates)))
    (helm-org-execute markers
      (org-archive-subtree-default))))

(defun helm-org-run-archive-subtree ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org-archive-subtree)))
(put 'helm-org-run-archive-subtree 'helm-only t)

(defun helm-org--open-heading-in-indirect-buffer (marker)
  "Go to MARKER and create an indirect buffer of the current subtree."
  (save-excursion
    (helm-org-execute marker
      (org-tree-to-indirect-buffer)
      ;; Put the non-indirect buffer at the bottom of the prev-buffers
      ;; list so it won't be selected when the indirect buffer is killed
      (set-window-prev-buffers nil (append (cdr (window-prev-buffers))
                                           (car (window-prev-buffers)))))))

(defun helm-org-run-open-heading-in-indirect-buffer ()
  "Open selected subtree in an indirect buffer."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-org--open-heading-in-indirect-buffer)))
(put 'helm-org-run-open-heading-in-indirect-buffer 'helm-only t)

(defun helm-org-insert-link-to-heading-at-marker (marker)
  (with-current-buffer (marker-buffer marker)
    (let ((heading-name (save-excursion
			  (goto-char (marker-position marker))
                          (org-entry-get nil "ITEM")))
	  (file-name (buffer-file-name)))
      (org-insert-link file-name
		       (concat "file:" file-name "::*" heading-name)))))

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


;;; Commands
;;
;;

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
              (y-or-n-p (format "%s have auto save data, continue? "
                                (mapconcat #'identity autosaves ", "))))
      (helm :sources (helm-source-org-headings-for-files
                      (org-agenda-files))
            :candidate-number-limit 99999
            :truncate-lines helm-org-truncate-lines
            :buffer "*helm org headings*"))))

;;;###autoload
(defun helm-org-in-buffer-headings ()
  "Preconfigured Helm for buffer headings."
  (interactive)
  (helm :sources (helm-source-org-headings-for-files
                  (list (current-buffer)))
        :candidate-number-limit 99999
        :preselect (helm-org-in-buffer-preselect)
        :truncate-lines helm-org-truncate-lines
        :buffer "*helm org inbuffer*"))

;;;###autoload
(defun helm-org-parent-headings ()
  "Preconfigured Helm for parent headings of the current heading."
  (interactive)
  (helm :sources (helm-source-org-headings-for-files
                  (list (current-buffer)) t)
	:candidate-number-limit 99999
	:truncate-lines helm-org-truncate-lines
	:buffer "*helm org parent headings*"))

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
