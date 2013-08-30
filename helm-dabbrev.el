;;; helm-dabbrev.el --- Helm implementation of dabbrev.

;; Copyright (C) 2012 ~ 2013 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(require 'helm)
(require 'helm-elisp) ; For show-completion.

(defgroup helm-dabbrev nil
  "Dabbrev related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-dabbrev-always-search-all t
  "Always search in all buffers when non--nil."
  :group 'helm-dabbrev
  :type 'boolean)

(defcustom helm-dabbrev-max-length-result 20
  "Max length of candidates before searching in all buffers.
If number of candidates found in current-buffer is <= to this,
search in all buffers.
Have no effect when `helm-dabbrev-always-search-all' is non--nil."
  :group 'helm-dabbrev
  :type 'integer)

(defcustom helm-dabbrev-ignored-buffers-regexps
  '("\\*helm" "\\*Messages" "\\*Minibuf" "\\*Echo Area" "\\*Buffer List")
  "List of regexps matching names of buffers that helm-dabbrev should not check."
  :group 'helm-dabbrev
  :type '(repeat regexp))

(defcustom helm-dabbrev-major-mode-assoc
  '((emacs-lisp-mode . lisp-interaction-mode))
  "Major mode association alist.
This allow helm-dabbrev searching in buffers with the associated `major-mode'.
e.g \(emacs-lisp-mode . lisp-interaction-mode\)
will allow searching in the lisp-interaction-mode buffer when `current-buffer'
is an `emacs-lisp-mode' buffer and vice versa i.e
no need to provide \(lisp-interaction-mode . emacs-lisp-mode\) association."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'helm-dabbrev)

(defcustom helm-dabbrev-lineno-around 30
  "Search first in this number of lines before an after point."
  :group 'helm-dabbrev
  :type 'integer)

(defcustom helm-dabbrev-cycle-thresold nil
  "Number of time helm-dabbrev cycle before displaying helm completion.
When nil or 0 disable cycling."
  :group 'helm-dabbrev
  :type '(choice (const :tag "Cycling disabled" nil) integer))

(defcustom helm-dabbrev-case-fold-search 'smart
  "Set `case-fold-search' in `helm-dabbrev'.
Same as `helm-case-fold-search' but for `helm-dabbrev'.
Note that this is not affecting searching in helm buffer,
but the initial search for all candidates in buffer(s)."
  :group 'helm-dabbrev
  :type '(choice (const :tag "Ignore case" t)
                 (const :tag "Respect case" nil)
                 (other :tag "Smart" 'smart)))

(defvar helm-dabbrev-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-/") 'helm-next-line)
    (define-key map (kbd "M-:") 'helm-previous-line)
    map))

;; Internal
(defvar helm-dabbrev--exclude-current-buffer-flag nil)

(defun helm-dabbrev--buffer-list ()
  (loop with lst = (buffer-list)
        for buf in (if helm-dabbrev--exclude-current-buffer-flag
                       (cdr lst) lst)
        unless (loop for r in helm-dabbrev-ignored-buffers-regexps
                     thereis (string-match r (buffer-name buf)))
        collect buf))

(defun helm-dabbrev--same-major-mode-p (buf)
  (or (or (assoc major-mode helm-dabbrev-major-mode-assoc)
          (rassoc major-mode helm-dabbrev-major-mode-assoc))
      (eq major-mode (with-helm-current-buffer major-mode))))

(defun helm-dabbrev--collect (str limit ignore-case all)
  (let ((case-fold-search ignore-case)
        (search-and-store
         #'(lambda (pattern direction)
             (declare (special result pos-before pos-after))
             (while (case direction
                      (1   (search-forward pattern nil t))
                      (-1  (search-backward pattern nil t))
                      (2   (let ((pos
                                  (save-excursion
                                    (forward-line
                                     helm-dabbrev-lineno-around)
                                    (point))))
                             (setq pos-after pos)
                             (search-forward pattern pos t)))
                      (-2  (let ((pos
                                  (save-excursion
                                    (forward-line
                                     (- helm-dabbrev-lineno-around))
                                    (point))))
                             (setq pos-before pos)
                             (search-backward pattern pos t))))
               (let ((match (substring-no-properties
                             (thing-at-point 'symbol)))) 
                 (unless (or (string= str match) (member match result))
                   (push match result)))))))
    (loop with result with pos-before with pos-after
          for buf in (if all (helm-dabbrev--buffer-list)
                         (list (current-buffer)))
          do (with-current-buffer buf
               (when (helm-dabbrev--same-major-mode-p buf)
                 (save-excursion
                   ;; Start searching before thing before point.
                   (goto-char (- (point) (length str)))
                   ;; Search the last 30 lines before point.
                   (funcall search-and-store str -2)) ; store pos [1]
                 (save-excursion
                   ;; Search the next 30 lines after point.
                   (funcall search-and-store str 2)) ; store pos [2]
                 (save-excursion
                   ;; Search all before point.
                   (goto-char pos-before) ; start from [1]
                   (funcall search-and-store str -1))
                 (save-excursion
                   ;; Search all after point.
                   (goto-char pos-after) ; start from [2]
                   (funcall search-and-store str 1))))
          when (> (length result) limit) return (nreverse result)
          finally return (nreverse result))))

(defun helm-dabbrev--get-candidates (abbrev)
  (assert abbrev nil "[No Match]")
  (with-current-buffer (current-buffer)
    (let* ((dabbrev-get #'(lambda (str all-bufs)
                             (helm-dabbrev--collect
                              str helm-candidate-number-limit
                              (case helm-dabbrev-case-fold-search
                                (smart (helm-set-case-fold-search-1 abbrev))
                                (t helm-dabbrev-case-fold-search))
                              all-bufs)))
           (lst (funcall dabbrev-get abbrev helm-dabbrev-always-search-all)))
      (if (and (not helm-dabbrev-always-search-all)
               (<= (length lst) helm-dabbrev-max-length-result))
          ;; Search all but don't recompute current-buffer.
          (let ((helm-dabbrev--exclude-current-buffer-flag t))
            (append lst (funcall dabbrev-get abbrev 'all-bufs)))
          lst))))

;; Internal
(defvar helm-dabbrev--cache nil)
(defvar helm-dabbrev--data nil)
(defstruct helm-dabbrev-info dabbrev limits iterator)

(defvar helm-source-dabbrev
  `((name . "Dabbrev Expand")
    (init . (lambda ()
              (helm-init-candidates-in-buffer
               'global
               helm-dabbrev--cache))) 
    (candidates-in-buffer)
    (keymap . ,helm-dabbrev-map)
    (action . (lambda (candidate)
                (with-helm-current-buffer
                  (let* ((limits (helm-bounds-of-thing-before-point))
                         (beg (car limits))
                         (end (point)))
                    (run-with-timer
                     0.01 nil
                     'helm-insert-completion-at-point
                     beg end candidate)))))))

;;;###autoload
(defun helm-dabbrev ()
  (interactive)
  (let ((dabbrev (helm-thing-before-point))
        (limits (helm-bounds-of-thing-before-point))
        (enable-recursive-minibuffers t)
        (cycling-disabled-p (or (null helm-dabbrev-cycle-thresold)
                                (zerop helm-dabbrev-cycle-thresold)))
        (helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate
         #'(lambda ()
             (message "[Helm-dabbrev: No expansion found]"))))
    (when (and
           ;; have been called at least once.
           (helm-dabbrev-info-p helm-dabbrev--data)
           ;; But user have moved with some other command
           ;; in the meaning time.
           (not (eq last-command 'helm-dabbrev)))
      (setq helm-dabbrev--data nil))
    (when cycling-disabled-p
      (setq helm-dabbrev--cache (helm-dabbrev--get-candidates dabbrev)))
    (unless (or cycling-disabled-p
                (helm-dabbrev-info-p helm-dabbrev--data))
      (setq helm-dabbrev--cache (helm-dabbrev--get-candidates dabbrev))
      (setq helm-dabbrev--data (make-helm-dabbrev-info
                                :dabbrev dabbrev
                                :limits limits
                                :iterator
                                (helm-iter-list
                                 (loop with selection
                                       for i in helm-dabbrev--cache
                                       when (string-match
                                             (concat "^" dabbrev) i)
                                       collect i into selection
                                       when (or (= (length selection)
                                                   helm-dabbrev-cycle-thresold)
                                                (= (length selection)
                                                   (length helm-dabbrev--cache)))
                                       return selection)))))
    (let ((iter (and (helm-dabbrev-info-p helm-dabbrev--data)
                     (helm-dabbrev-info-iterator helm-dabbrev--data))))
      (helm-aif (and iter (helm-iter-next iter))
          (progn
            (helm-insert-completion-at-point (car limits) (cdr limits) it)
            ;; Move already tried candidates to end of list.
            (setq helm-dabbrev--cache (append (remove it helm-dabbrev--cache)
                                              (list it))))
        (unless cycling-disabled-p
          (delete-region (car limits) (point))
          (setq dabbrev (helm-dabbrev-info-dabbrev helm-dabbrev--data)
                limits  (helm-dabbrev-info-limits helm-dabbrev--data))
          (setq helm-dabbrev--data nil)
          (insert dabbrev))
        (with-helm-show-completion (car limits) (cdr limits)
          (helm :sources 'helm-source-dabbrev
                :buffer "*helm dabbrev*"
                :input (concat "^" dabbrev " ")
                :resume 'noresume
                :allow-nest t))))))

(provide 'helm-dabbrev)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-dabbrev.el ends here
