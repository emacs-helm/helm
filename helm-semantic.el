;;; helm-semantic.el --- Helm interface for Semantic

;; Copyright (C) 2012 Daniel Hackney <dan@haxney.org>
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

(eval-when-compile
  (require 'cl))

(require 'semantic)
(require 'helm-imenu)

(defun helm-semantic-init-candidates (tags depth)
  "Write the contents of TAGS to the current buffer."
  (dolist (tag tags)
    (when (listp tag)
      (case (semantic-tag-class tag)

        ((function variable type)
         (insert
          (make-string (* depth 2) ?\s)
          ;; Save the tag for later
          (propertize (semantic-format-tag-summarize tag nil t) 'semantic-tag tag)
          "\n")
         ;; Recurse to children
         (helm-semantic-init-candidates
          (semantic-tag-components tag) (1+ depth)))

        ;; Don't do anything with packages or includes for now
        ((package include))
        ;; Catch-all
        (t)))))

(defun helm-semantic-default-action (_candidate)
  ;; By default, helm doesn't pass on the text properties of the selection.
  ;; Fix this.
  (with-current-buffer helm-buffer
    (skip-chars-forward " " (point-at-eol))
    (let ((tag (get-text-property (point) 'semantic-tag)))
      (push-mark)
      (semantic-go-to-tag tag))))

(defvar helm-c-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (let ((tags (semantic-fetch-tags)))
                (with-current-buffer (helm-candidate-buffer 'global)
                  (helm-semantic-init-candidates tags 0)))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (persistent-action . (lambda (elm)
                           (helm-semantic-default-action elm)
                           (helm-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . helm-semantic-default-action)
    "Source to search tags using Semantic from CEDET."))

;;;###autoload
(defun helm-semantic ()
  "Preconfigured `helm' for `semantic'."
  (interactive)
  (helm :sources 'helm-c-source-semantic
        :buffer "*helm semantic*"))

;;;###autoload
(defun helm-semantic-or-imenu ()
  "Run `helm' with `semantic' or `imenu'.

If `semantic-mode' is active in the current buffer, then use
semantic for generating tags, otherwise fall back to `imenu'.
Fill in the symbol at point by default."
  (interactive)
  (let ((source (if (semantic-active-p)
                    'helm-c-source-semantic
                  'helm-c-source-imenu)))
    (push-mark)
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
