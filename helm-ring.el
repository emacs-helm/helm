;;; helm-ring.el --- kill-ring, mark-ring, and register browsers for helm.

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

(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-utils)

(declare-function undo-tree-restore-state-from-register "ext:undo-tree.el" (register))


(defgroup helm-ring nil
  "Ring related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-kill-ring-threshold 10
  "Minimum length of a candidate to be listed by `helm-c-source-kill-ring'."
  :type 'integer
  :group 'helm-ring)

(defcustom helm-c-kill-ring-max-lines-number nil
  "Max number of lines displayed per candidate in kill-ring browser.
If nil or zero, don't truncate candidate, show all."
  :type 'integer
  :group 'helm-ring)

(defcustom helm-c-register-max-offset 160
  "Max size of string register entries before truncating."
  :group 'helm-ring
  :type  'integer)


;;; Kill ring
;;
;;
(defvar helm-kill-ring-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-y") 'helm-next-line)
    (define-key map (kbd "M-u") 'helm-previous-line)
    map)
  "Keymap for `helm-show-kill-ring'.")

(defvar helm-c-source-kill-ring
  `((name . "Kill Ring")
    (init . (lambda () (helm-attrset 'last-command last-command)))
    (candidates . helm-c-kill-ring-candidates)
    (filtered-candidate-transformer helm-c-kill-ring-transformer)
    (action . (("Yank" . helm-c-kill-ring-action)
               ("Delete" . (lambda (candidate)
                             (loop for cand in (helm-marked-candidates)
                                   do (setq kill-ring
                                            (delete cand kill-ring)))))))
    (keymap . ,helm-kill-ring-map)
    (last-command)
    (migemo)
    (multiline))
  "Source for browse and insert contents of kill-ring.")

(defun helm-c-kill-ring-candidates ()
  (loop for kill in (helm-fast-remove-dups kill-ring :test 'equal)
        unless (or (< (length kill) helm-kill-ring-threshold)
                   (string-match "^\\(\\s-\\|\t\\)+$" kill))
        collect kill))

(defun helm-c-kill-ring-transformer (candidates source)
  "Display only the `helm-c-kill-ring-max-lines-number' lines of candidate."
  (loop for i in candidates
        for nlines = (with-temp-buffer (insert i) (count-lines (point-min) (point-max)))
        if (and helm-c-kill-ring-max-lines-number
                (> nlines helm-c-kill-ring-max-lines-number))
        collect (cons
                 (with-temp-buffer
                   (insert i)
                   (goto-char (point-min))
                   (concat
                    (buffer-substring
                     (point-min)
                     (save-excursion
                       (forward-line helm-c-kill-ring-max-lines-number)
                       (point)))
                    "[...]")) i)
        else collect i))

(defun helm-c-kill-ring-action (str)
  "Insert STR in `kill-ring' and set STR to the head.
If this action is executed just after `yank',
replace with STR as yanked string."
  (setq kill-ring (delete str kill-ring))
  (if (not (eq (helm-attr 'last-command) 'yank))
      (with-helm-current-buffer (insert-for-yank str))
      ;; from `yank-pop'
      (let ((inhibit-read-only t)
            (before (< (point) (mark t))))
        (if before
            (funcall (or yank-undo-function 'delete-region) (point) (mark t))
            (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
        (setq yank-undo-function nil)
        (set-marker (mark-marker) (point) helm-current-buffer)
        (with-helm-current-buffer (insert-for-yank str))
        ;; Set the window start back where it was in the yank command,
        ;; if possible.
        (set-window-start (selected-window) yank-window-start t)
        (if before
            ;; This is like exchange-point-and-mark, but doesn't activate the mark.
            ;; It is cleaner to avoid activation, even though the command
            ;; loop would deactivate the mark because we inserted text.
            (goto-char (prog1 (mark t)
                         (set-marker (mark-marker) (point) helm-current-buffer))))))
  (kill-new str))



;;;; <Mark ring>
;; DO NOT include these sources in `helm-sources' use
;; the commands `helm-mark-ring', `helm-global-mark-ring' or
;; `helm-all-mark-rings' instead.

(defun helm-mark-ring-get-marks (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (let ((line  (car (split-string (thing-at-point 'line) "[\n\r]"))))
      (when (string= "" line)
        (setq line  "<EMPTY LINE>"))
      (format "%7d: %s" (line-number-at-pos) line))))

(defun helm-mark-ring-get-candidates ()
  (with-helm-current-buffer
    (loop with marks = (if (mark) (cons (mark-marker) mark-ring) mark-ring)
          with recip = nil
          for i in marks
          for m = (helm-mark-ring-get-marks i)
          unless (member m recip)
          collect m into recip
          finally return recip)))

(defvar helm-mark-ring-cache nil)
(defvar helm-c-source-mark-ring
  '((name . "mark-ring")
    (init . (lambda ()
              (setq helm-mark-ring-cache
                    (ignore-errors (helm-mark-ring-get-candidates)))))
    (candidates . (lambda ()
                    (helm-aif helm-mark-ring-cache
                        it)))
    (action . (("Goto line"
                . (lambda (candidate)
                    (helm-goto-line (string-to-number candidate))
                    (push-mark nil 'nomsg))))) 
    (persistent-action . (lambda (candidate)
                           (helm-goto-line (string-to-number candidate))
                           (helm-match-line-color-current-line)))
    (persistent-help . "Show this line")))


;;; Global-mark-ring
(defvar helm-c-source-global-mark-ring
  '((name . "global-mark-ring")
    (candidates . helm-global-mark-ring-get-candidates)
    (action . (("Goto line"
                . (lambda (candidate)
                    (let ((items (split-string candidate ":")))
                      (helm-c-switch-to-buffer (second items))
                      (helm-goto-line (string-to-number (car items))))))))
    (persistent-action . (lambda (candidate)
                           (let ((items (split-string candidate ":")))
                             (helm-c-switch-to-buffer (second items))
                             (helm-goto-line (string-to-number (car items)))
                             (helm-match-line-color-current-line))))
    (persistent-help . "Show this line")))

(defun helm-global-mark-ring-format-buffer (marker)
  (with-current-buffer (marker-buffer marker)
    (goto-char marker)
    (beginning-of-line)
    (let (line)
      (if (string= "" line)
          (setq line  "<EMPTY LINE>")
          (setq line (car (split-string (thing-at-point 'line)
                                        "[\n\r]"))))
      (format "%7d:%s:    %s"
              (line-number-at-pos) (marker-buffer marker) line))))

(defun helm-global-mark-ring-get-candidates ()
  (loop with marks = global-mark-ring
        with recip = nil
        for i in marks
        for gm = (unless (or (string-match
                              "^ " (format "%s" (marker-buffer i)))
                             (null (marker-buffer i)))
                   (helm-global-mark-ring-format-buffer i))
        when (and gm (not (member gm recip)))
        collect gm into recip
        finally return recip))


;;;; <Register>
;;; Insert from register
(defvar helm-c-source-register
  '((name . "Registers")
    (candidates . helm-c-register-candidates)
    (action-transformer . helm-c-register-action-transformer)
    (multiline)
    (action))
  "See (info \"(emacs)Registers\")")

(defun helm-c-register-candidates ()
  "Collecting register contents and appropriate commands."
  (loop for (char . val) in register-alist
        for key    = (single-key-description char)
        for string-actions =
        (cond
          ((numberp val)
           (list (int-to-string val)
                 'insert-register
                 'increment-register))
          ((markerp val)
           (let ((buf (marker-buffer val)))
             (if (null buf)
                 (list "a marker in no buffer")
                 (list (concat
                        "a buffer position:"
                        (buffer-name buf)
                        ", position "
                        (int-to-string (marker-position val)))
                       'jump-to-register
                       'insert-register))))
          ((and (consp val) (window-configuration-p (car val)))
           (list "window configuration."
                 'jump-to-register))
          ((and (consp val) (frame-configuration-p (car val)))
           (list "frame configuration."
                 'jump-to-register))
          ((and (consp val) (eq (car val) 'file))
           (list (concat "file:"
                         (prin1-to-string (cdr val))
                         ".")
                 'jump-to-register))
          ((and (consp val) (eq (car val) 'file-query))
           (list (concat "file:a file-query reference: file "
                         (car (cdr val))
                         ", position "
                         (int-to-string (car (cdr (cdr val))))
                         ".")
                 'jump-to-register))
          ((consp val)
           (let ((lines (format "%4d" (length val))))
             (list (format "%s: %s\n" lines
                           (truncate-string-to-width
                            (mapconcat 'identity (list (car val))
                                       "^J") (- (window-width) 15)))
                   'insert-register)))
          ((stringp val)
           (list
            ;; without properties
            (concat (substring-no-properties
                     val 0 (min (length val) helm-c-register-max-offset))
                    (if (> (length val) helm-c-register-max-offset)
                        "[...]" ""))
            'insert-register
            'append-to-register
            'prepend-to-register))
          ((vectorp val)
           (list
            "Undo-tree entry."
            'undo-tree-restore-state-from-register))
          (t
           "GARBAGE!"))
        collect (cons (format "Register %3s:\n %s" key (car string-actions))
                      (cons char (cdr string-actions)))))

(defun helm-c-register-action-transformer (actions register-and-functions)
  "Decide actions by the contents of register."
  (loop with func-actions =
        '((insert-register
           "Insert Register" .
           (lambda (c) (insert-register (car c))))
          (jump-to-register
           "Jump to Register" .
           (lambda (c) (jump-to-register (car c))))
          (append-to-register
           "Append Region to Register" .
           (lambda (c) (append-to-register
                        (car c) (region-beginning) (region-end))))
          (prepend-to-register
           "Prepend Region to Register" .
           (lambda (c) (prepend-to-register
                        (car c) (region-beginning) (region-end))))
          (increment-register
           "Increment Prefix Arg to Register" .
           (lambda (c) (increment-register
                        helm-current-prefix-arg (car c))))
          (undo-tree-restore-state-from-register
           "Restore Undo-tree register"
           (lambda (c) (and (fboundp 'undo-tree-restore-state-from-register)
                            (undo-tree-restore-state-from-register (car c))))))
        for func in (cdr register-and-functions)
        for cell = (assq func func-actions)
        when cell
        collect (cdr cell)))

;;;###autoload
(defun helm-mark-ring ()
  "Preconfigured `helm' for `helm-c-source-mark-ring'."
  (interactive)
  (helm :sources 'helm-c-source-mark-ring))

;;;###autoload
(defun helm-global-mark-ring ()
  "Preconfigured `helm' for `helm-c-source-global-mark-ring'."
  (interactive)
  (helm :sources 'helm-c-source-global-mark-ring))

;;;###autoload
(defun helm-all-mark-rings ()
  "Preconfigured `helm' for `helm-c-source-global-mark-ring' and \
`helm-c-source-mark-ring'."
  (interactive)
  (helm :sources '(helm-c-source-mark-ring
                   helm-c-source-global-mark-ring)))

;;;###autoload
(defun helm-register ()
  "Preconfigured `helm' for Emacs registers."
  (interactive)
  (helm-other-buffer 'helm-c-source-register "*helm register*"))

;;;###autoload
(defun helm-show-kill-ring ()
  "Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line."
  (interactive)
  (helm :sources helm-c-source-kill-ring
        :buffer "*helm kill-ring*"
        :allow-nest t))

(provide 'helm-ring)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-ring.el ends here
