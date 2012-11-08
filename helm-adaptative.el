;;; helm-adaptative.el --- Adaptive Sorting of Candidates.

;; Original Author: Tamas Patrovics

;; Copyright (C) 2007 Tamas Patrovics
;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(require 'cl)
(require 'helm)


(defgroup helm-adapt nil
  "Adaptative sorting of candidates for Helm."
  :group 'helm)

(defcustom helm-c-adaptive-history-file
  "~/.emacs.d/helm-c-adaptive-history"
  "Path of file where history information is stored."
  :type 'string
  :group 'helm-adapt)

(defcustom helm-c-adaptive-history-length 50
  "Maximum number of candidates stored for a source."
  :type 'number
  :group 'helm-adapt)


;; Internal
(defvar helm-c-adaptive-done nil
  "nil if history information is not yet stored for the current
selection.")

(defvar helm-c-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

(defun helm-adaptative-done-reset ()
  (setq helm-c-adaptive-done nil))

(define-minor-mode helm-adaptative-mode
    "Toggle adaptative sorting in all sources."
  :group 'helm-adapt
  :require 'helm-adaptative
  :global t
  (if helm-adaptative-mode
      (progn
        ;; Should run at beginning of `helm-initial-setup'.
        (add-hook 'helm-before-initialize-hook 'helm-adaptative-done-reset)
        ;; Should run at beginning of `helm-exit-minibuffer'.
        (add-hook 'helm-before-action-hook 'helm-c-adaptive-store-selection)
        ;; Should run at beginning of `helm-select-action'.
        (add-hook 'helm-select-action-hook 'helm-c-adaptive-store-selection))
      (remove-hook 'helm-before-initialize-hook 'helm-adaptative-done-reset)
      (remove-hook 'helm-before-action-hook 'helm-c-adaptive-store-selection)
      (remove-hook 'helm-select-action-hook 'helm-c-adaptive-store-selection)))

(defun helm-c-source-use-adaptative-p (&optional source-name)
  "Return current source only if it use adaptative history, nil otherwise."
  (when helm-adaptative-mode
    (let* ((source (or source-name (helm-get-current-source)))
           (adapt-source (or (assoc-default 'filtered-candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   helm-type-attributes))
                             (assoc-default 'candidate-transformer
                                            (assoc (assoc-default 'type source)
                                                   helm-type-attributes))
                             (assoc-default 'filtered-candidate-transformer source)
                             (assoc-default 'candidate-transformer source))))
      (if (listp adapt-source)
          (when (member 'helm-c-adaptive-sort adapt-source) source)
          (when (eq adapt-source 'helm-c-adaptive-sort) source)))))

(defun helm-c-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless helm-c-adaptive-done
    (setq helm-c-adaptive-done t)
    (let ((source (helm-c-source-use-adaptative-p)))
      (when source
        (let* ((source-name (or (assoc-default 'type source)
                                (assoc-default 'name source)))
               (source-info (or (assoc source-name helm-c-adaptive-history)
                                (progn
                                  (push (list source-name) helm-c-adaptive-history)
                                  (car helm-c-adaptive-history))))
               (selection (helm-get-selection))
               (selection-info (progn
                                 (setcdr source-info
                                         (cons
                                          (let ((found (assoc selection (cdr source-info))))
                                            (if (not found)
                                                ;; new entry
                                                (list selection)
                                                ;; move entry to the beginning of the
                                                ;; list, so that it doesn't get
                                                ;; trimmed when the history is
                                                ;; truncated
                                                (setcdr source-info
                                                        (delete found (cdr source-info)))
                                                found))
                                          (cdr source-info)))
                                 (cadr source-info)))
               (pattern-info (progn
                               (setcdr selection-info
                                       (cons
                                        (let ((found (assoc helm-pattern (cdr selection-info))))
                                          (if (not found)
                                              ;; new entry
                                              (cons helm-pattern 0)

                                              ;; move entry to the beginning of the
                                              ;; list, so if two patterns used the
                                              ;; same number of times then the one
                                              ;; used last appears first in the list
                                              (setcdr selection-info
                                                      (delete found (cdr selection-info)))
                                              found))
                                        (cdr selection-info)))
                               (cadr selection-info))))

          ;; increase usage count
          (setcdr pattern-info (1+ (cdr pattern-info)))

          ;; truncate history if needed
          (if (> (length (cdr selection-info)) helm-c-adaptive-history-length)
              (setcdr selection-info
                      (subseq (cdr selection-info) 0 helm-c-adaptive-history-length))))))))

(defun helm-c-adaptative-maybe-load-history ()
  (when (and helm-adaptative-mode
             (file-readable-p helm-c-adaptive-history-file))
    (load-file helm-c-adaptive-history-file)))

(add-hook 'emacs-startup-hook 'helm-c-adaptative-maybe-load-history)
(add-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)

(defun helm-c-adaptive-save-history (&optional arg)
  "Save history information to file given by `helm-c-adaptive-history-file'."
  (interactive "p")
  (when helm-adaptative-mode
    (with-temp-buffer
      (insert
       ";; -*- mode: emacs-lisp -*-\n"
       ";; History entries used for helm adaptive display.\n")
      (prin1 `(setq helm-c-adaptive-history ',helm-c-adaptive-history)
             (current-buffer))
      (insert ?\n)
      (write-region (point-min) (point-max) helm-c-adaptive-history-file nil
                    (unless arg 'quiet)))))

(defun helm-c-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer you can use for the
attribute `filtered-candidate-transformer' of a source in
`helm-sources' or a type in `helm-type-attributes'."
  (let* ((source-name (or (assoc-default 'type source)
                          (assoc-default 'name source)))
         (source-info (assoc source-name helm-c-adaptive-history)))
    (if source-info
        (let ((usage
               ;; ... assemble a list containing the (CANIDATE . USAGE-COUNT)
               ;; pairs
               (mapcar (lambda (candidate-info)
                         (let ((count 0))
                           (dolist (pattern-info (cdr candidate-info))
                             (if (not (equal (car pattern-info)
                                             helm-pattern))
                                 (incf count (cdr pattern-info))

                                 ;; if current pattern is equal to the previously
                                 ;; used one then this candidate has priority
                                 ;; (that's why its count is boosted by 10000) and
                                 ;; it only has to compete with other candidates
                                 ;; which were also selected with the same pattern
                                 (setq count (+ 10000 (cdr pattern-info)))
                                 (return)))
                           (cons (car candidate-info) count)))
                       (cdr source-info)))
              sorted)
          (if (and usage (consp usage))
              ;; sort the list in descending order, so candidates with highest
              ;; priorty come first
              (progn
                (setq usage (sort usage (lambda (first second)
                                          (> (cdr first) (cdr second)))))

                ;; put those candidates first which have the highest usage count
                (dolist (info usage)
                  (when (member* (car info) candidates
                                 :test 'helm-c-adaptive-compare)
                    (push (car info) sorted)
                    (setq candidates (remove* (car info) candidates
                                              :test 'helm-c-adaptive-compare))))

                ;; and append the rest
                (append (reverse sorted) candidates nil))
              (message "Your `%s' is maybe corrupted or too old, \
you should reinitialize it with `helm-c-reset-adaptative-history'"
                       helm-c-adaptive-history-file)
              (sit-for 1)
              candidates))
        ;; if there is no information stored for this source then do nothing
        candidates)))

;;;###autoload
(defun helm-c-reset-adaptative-history ()
  "Delete all `helm-c-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-c-adaptive-history-file'."
  (interactive)
  (when (y-or-n-p "Really delete all your `helm-c-adaptive-history'? ")
    (setq helm-c-adaptive-history nil)
    (delete-file helm-c-adaptive-history-file)))

(defun helm-c-adaptive-compare (x y)
  "Compare candidates X and Y taking into account that the
candidate can be in (DISPLAY . REAL) format."
  (equal (if (listp x)
             (cdr x)
             x)
         (if (listp y)
             (cdr y)
             y)))


(provide 'helm-adaptative)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-adaptative.el ends here
