;;; helm-semantic.el --- Helm interface for Semantic -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Daniel Hackney <dan@haxney.org>
;; Author: Daniel Hackney <dan@haxney.org>

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

;; Uses `candidates-in-buffer' for speed.

;;; Code:

(require 'cl-lib)
(require 'semantic)
(require 'helm-imenu)

(declare-function pulse-momentary-highlight-one-line "pulse.el" (point &optional face))

(defgroup helm-semantic nil
  "Semantic tags related libraries and applications for helm."
  :group 'helm)

(defcustom helm-semantic-lynx-style-map t
  "Use Arrow keys to jump to occurences."
  :group 'helm-semantic
  :type  'boolean)

;;; keymap
(defvar helm-semantic-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?") 'helm-semantic-help)
    (when helm-imenu-lynx-style-map
      (define-key map (kbd "<left>")  'helm-maybe-exit-minibuffer)
      (define-key map (kbd "<right>") 'helm-execute-persistent-action))
    (delq nil map)))

;; Internals vars
(defvar helm-semantic--tags-cache nil)

(defun helm-semantic--fetch-candidates (tags depth &optional class)
  "Write the contents of TAGS to the current buffer."
  (let ((class class) cur-type)
    (cl-dolist (tag tags)
      (when (listp tag)
        (cl-case (setq cur-type (semantic-tag-class tag))
          ((function variable type)
           (let ((spaces (make-string (* depth 2) ?\s))
                 (type-p (eq cur-type 'type)))
             (unless (and (> depth 0) (not type-p))
               (setq class nil))
             (insert
              (if (and class (not type-p))
                  (format "%s%sClass(%s) "
                          spaces (if (< depth 2) "" "├►") class)
                spaces)
              ;; Save the tag for later
              (propertize (semantic-format-tag-summarize tag nil t)
                          'semantic-tag tag)
              "\n")
             (and type-p (setq class (car tag)))
             ;; Recurse to children
             (helm-semantic--fetch-candidates
              (semantic-tag-components tag) (1+ depth) class)))

          ;; Don't do anything with packages or includes for now
          ((package include)
           (insert
            (propertize (semantic-format-tag-summarize tag nil t)
                        'semantic-tag tag)
            "\n")
           )
          ;; Catch-all
          (t))))))

(defun helm-semantic-default-action (_candidate &optional persistent)
  ;; By default, helm doesn't pass on the text properties of the selection.
  ;; Fix this.
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (with-current-buffer helm-buffer
    (when (looking-at " ")
      (goto-char (next-single-property-change
                  (point-at-bol) 'semantic-tag nil (point-at-eol)))) 
    (let ((tag (get-text-property (point) 'semantic-tag)))
      (semantic-go-to-tag tag)
      (unless persistent
        (pulse-momentary-highlight-one-line (point))))))

(defun helm-semantic-get-candidates ()
  "Get a list of candidates in the current buffer."
  (split-string (with-temp-buffer
                  (helm-semantic--fetch-candidates helm-semantic--tags-cache 0)
                  (buffer-string)) "\n"))

(defun helm-semantic--maybe-set-needs-update ()
  (with-helm-current-buffer
    (let ((tick (buffer-modified-tick)))
      (unless (eq helm-cached-imenu-tick tick)
        (setq helm-cached-imenu-tick tick)
        (semantic-parse-tree-set-needs-update)))))

(defvar helm-source-semantic nil)

(defclass helm-semantic-source (helm-source-sync)
  ((init :initform (lambda ()
                     (helm-semantic--maybe-set-needs-update)
                     (setq helm-semantic--tags-cache (semantic-fetch-tags))
                     (with-current-buffer (helm-candidate-buffer 'global)
                       (helm-semantic--fetch-candidates helm-semantic--tags-cache 0))))
   (candidates :initform 'helm-semantic-get-candidates)
   (persistent-help :initform "Show this entry")
   (keymap :initform 'helm-semantic-map)
   (mode-line :initform helm-semantic-mode-line)
   (persistent-action :initform (lambda (elm)
                                  (helm-semantic-default-action elm t)
                                  (helm-highlight-current-line)))
   (action :initform 'helm-semantic-default-action)))

(defcustom helm-semantic-fuzzy-match nil
  "Enable fuzzy matching in `helm-source-semantic'."
  :group 'helm-semantic
  :type  'boolean
  :set (lambda (var val)
         (set var val)
         (setq helm-source-semantic
               (helm-make-source "Semantic Tags" 'helm-semantic-source
                 :fuzzy-match helm-semantic-fuzzy-match))))

;;;###autoload
(defun helm-semantic (arg)
  "Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current"
  (interactive "P")
  (let ((tag (helm-aif (semantic-current-tag-parent)
                  (cons (format "\\_<%s\\_>" (car it))
                        (format "\\_<%s\\_>" (car (semantic-current-tag))))
                (format "\\_<%s\\_>" (car (semantic-current-tag))))))
    (unless helm-source-semantic
      (setq helm-source-semantic
            (helm-make-source "Semantic Tags" 'helm-semantic-source
              :fuzzy-match helm-semantic-fuzzy-match)))
    (helm :sources 'helm-source-semantic
          :candidate-number-limit 9999
          :preselect (if arg
                         (thing-at-point 'symbol)
                       tag)
          :buffer "*helm semantic*")))

;;;###autoload
(defun helm-semantic-or-imenu (arg)
  "Run `helm' with `semantic' or `imenu'.
If ARG is supplied, pre-select symbol at point instead of current
semantic tag in scope.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default."
  (interactive "P")
  (unless helm-source-semantic
    (setq helm-source-semantic
          (helm-make-source "Semantic Tags" 'helm-semantic-source
            :fuzzy-match helm-semantic-fuzzy-match)))
  (unless helm-source-imenu
    (setq helm-source-imenu
          (helm-make-source "Imenu" 'helm-imenu-source
            :fuzzy-match helm-imenu-fuzzy-match)))
  (let* ((source (if (semantic-active-p)
                     'helm-source-semantic
                     'helm-source-imenu))
         (imenu-p (eq source 'helm-source-imenu))
         (imenu-auto-rescan imenu-p)
         (helm-execute-action-at-once-if-one
          (and imenu-p
               helm-imenu-execute-action-at-once-if-one))
         (tag (helm-aif (semantic-current-tag-parent)
                  (cons (format "\\_<%s\\_>" (car it))
                        (format "\\_<%s\\_>" (car (semantic-current-tag))))
                (format "\\_<%s\\_>" (car (semantic-current-tag))))))
    (helm :sources source
          :candidate-number-limit 9999
          :preselect (if (or arg imenu-p)
                         (thing-at-point 'symbol)
                         tag)
          :buffer "*helm semantic/imenu*")))

(provide 'helm-semantic)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-semantic.el ends here
