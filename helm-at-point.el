;;; helm-at-point.el --- Helm front-end for completion-at-point

;; Copyright (C) 2013 katspaugh

;; Author: katspaugh
;; Keywords: convenience, abbrev
;; URL: https://github.com/katspaugh/helm-at-point
;; Version: 0.0.1
;; Package-Requires: ((emacs "24")) ((cl "0.3")) ((helm "0.3"))

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

;; This package is an alternative frontend for `completion-at-point'.
;; It replaces the standard completions buffer with helm prompt.
;; Press <M-tab> or <C-M-i> to complete a symbol at point.

;;; Installation:

;; (require 'helm-at-point) ; unless installed from a package
;; (helm-at-point-mode)

;;; Code:

(require 'cl)
(require 'helm)

(defgroup helm-at-point nil
  "Helm interface for completion-at-point."
  :group 'helm)

(defcustom helm-at-point-partial t
  "If nil, don't complete partial match on the first completion attempt."
  :group 'helm-at-point
  :type  'boolean)

(defun helm-at-point-complete (_ start end collection &optional predicate)
  "Bring up completion-at-point interface in Helm."
  (lexical-let* ((input (buffer-substring-no-properties start end))
                 (choices (all-completions input collection predicate)))
    (cond ((null choices)
           (message "No match"))
          ((null (cdr choices))
           (helm-at-point-insert end input (car choices)))
          (t
           (lexical-let ((common (try-completion input choices)))
             (if (and helm-at-point-partial
                      (stringp common) (not (string= common input)))
                 (helm-at-point-insert end input common)
               (let ((helm-at-point-candidates choices)
                     (helm-at-point-end end)
                     (helm-at-point-common common))
                 (helm :sources '(helm-source-helm-at-point)
                       :buffer "*helm-at-point*"))))))))

(defun helm-at-point-insert-dynamic (candidate)
  (helm-at-point-insert
   helm-at-point-end helm-at-point-common candidate))

(defun helm-at-point-insert (end common completion)
  "Replaces text in buffer from END back to COMMON length with COMPLETION."
  ;; Completion text can have a property of `(face completions-common-part)'
  ;; which we'll use to determine, whether the completion contains
  ;; the common part (if any).
  ;; Note that not all completions come with text properties.
  (let ((len (or (next-property-change 0 completion) (length common) 0)))
    (goto-char end)
    (delete-region (- end len) end)
    (insert completion)))

(defvar helm-source-helm-at-point
  '((name . "completion-at-point")
    (volatile)
    (delayed)
    (candidates . helm-at-point-candidates)
    (action . helm-at-point-insert-dynamic)))

(defun helm-at-point-mode-set (enable)
  (if enable
      (add-to-list 'completion-in-region-functions
                   'helm-at-point-complete)
    (setq completion-in-region-functions
          (delq 'helm-at-point-complete
                completion-in-region-functions))))

;;;###autoload
(define-minor-mode helm-at-point-mode
  "Global minor mode to use Helm for `completion-at-point'.

When called interactively, toggle `helm-at-point-mode'. With
prefix ARG, enable `helm-at-point-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `helm-at-point-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`helm-at-point-mode'.  Otherwise behave as if called
interactively.

With `helm-at-point-mode' use Helm for `completion-at-point'."
  :variable ((memq 'helm-at-point-complete
                   completion-in-region-functions)
             .
             helm-at-point-mode-set))

(provide 'helm-at-point)

;;; helm-at-point.el ends here
