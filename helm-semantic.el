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

(defcustom helm-semantic-lynx-style-map t
  "Use Arrow keys to jump to occurences."
  :group 'helm-imenu
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

(defun helm-semantic-init-candidates (tags depth &optional class)
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
             (helm-semantic-init-candidates
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
  (let ((tags (semantic-fetch-tags)))
    (split-string (with-temp-buffer
                    (helm-semantic-init-candidates tags 0)
                    (buffer-string)) "\n")))

(defvar helm-source-semantic
  (helm-make-source "Semantic Tags" 'helm-source-sync
                    :header-name "Semantic Tags"
                    :candidates (lambda ()
                                  (with-helm-current-buffer
                                    (helm-semantic-get-candidates)))
                    :persistent-help "Show this entry"
                    :keymap 'helm-semantic-map
                    :mode-line helm-semantic-mode-line
                    :persistent-action (lambda (elm)
                                         (helm-semantic-default-action elm t)
                                         (helm-highlight-current-line))
                    :action 'helm-semantic-default-action))

;;;###autoload
(defun helm-semantic (arg)
  "Preconfigured `helm' for `semantic'.
If ARG is supplied, pre-select symbol at point instead of current"
  (interactive "P")
  (let ((tag (helm-aif (semantic-current-tag-parent)
                  (cons (format "\\_<%s\\_>" (car it))
                        (format "\\_<%s\\_>" (car (semantic-current-tag))))
                (format "\\_<%s\\_>" (car (semantic-current-tag))))))
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
