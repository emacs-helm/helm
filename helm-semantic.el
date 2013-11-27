;;; helm-semantic.el --- Helm interface for Semantic -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2013 Daniel Hackney <dan@haxney.org>
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

(defun helm-semantic-init-candidates (tags depth &optional class)
  "Write the contents of TAGS to the current buffer."
  (let ((class class) cur-type)
    (cl-dolist (tag tags)
      (when (listp tag)
        (cl-case (setq cur-type (semantic-tag-class tag))
          ((function variable type)
           (let ((spaces (make-string (* depth 2) ?\s))
                 (type-p (eq cur-type 'type)))
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
             (helm-semantic-init-candidates
              (semantic-tag-components tag) (1+ depth) class)))

          ;; Don't do anything with packages or includes for now
          ((package include))
          ;; Catch-all
          (t))))))

(defun helm-semantic-default-action (_candidate)
  ;; By default, helm doesn't pass on the text properties of the selection.
  ;; Fix this.
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (with-current-buffer helm-buffer
    (when (looking-at " ")
      (goto-char (next-single-property-change
                  (point-at-bol) 'semantic-tag nil (point-at-eol)))) 
    (let ((tag (get-text-property (point) 'semantic-tag)))
      (semantic-go-to-tag tag))))

(defvar helm-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (let ((tags (semantic-fetch-tags)))
                (with-current-buffer (helm-candidate-buffer 'global)
                  (helm-semantic-init-candidates tags 0)))))
    (candidates-in-buffer)
    (allow-dups)
    (get-line . buffer-substring)
    (persistent-action . (lambda (elm)
                           (helm-semantic-default-action elm)
                           (helm-highlight-current-line)))
    (persistent-help . "Show this entry")
    (action . helm-semantic-default-action)
    "Source to search tags using Semantic from CEDET."))

;;;###autoload
(defun helm-semantic ()
  "Preconfigured `helm' for `semantic'."
  (interactive)
  (helm :sources 'helm-source-semantic
        :buffer "*helm semantic*"))

;;;###autoload
(defun helm-semantic-or-imenu ()
  "Run `helm' with `semantic' or `imenu'.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default."
  (interactive)
  (let ((source (if (semantic-active-p)
                    'helm-source-semantic
                    'helm-source-imenu)))
    (helm :sources source
          :buffer "*helm semantic/imenu*"
          :preselect (thing-at-point 'symbol))))

(provide 'helm-semantic)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-semantic.el ends here
