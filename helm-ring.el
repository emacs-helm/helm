;;; helm-ring.el --- kill-ring, mark-ring, and register browsers for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-help)
(require 'helm-elisp)

(declare-function undo-tree-restore-state-from-register "ext:undo-tree.el" (register))


(defgroup helm-ring nil
  "Ring related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-kill-ring-threshold 3
  "Minimum length of a candidate to be listed by `helm-source-kill-ring'."
  :type 'integer
  :group 'helm-ring)

(defcustom helm-kill-ring-max-lines-number 5
  "Max number of lines displayed per candidate in kill-ring browser.
If nil or zero (disabled), don't truncate candidate, show all."
  :type '(choice (const :tag "Disabled" nil)
          (integer :tag "Max number of lines"))
  :group 'helm-ring)

(defcustom helm-register-max-offset 160
  "Max size of string register entries before truncating."
  :group 'helm-ring
  :type  'integer)

(defcustom helm-kill-ring-actions
  '(("Yank" . helm-kill-ring-action)
    ("Delete" . (lambda (_candidate)
                  (cl-loop for cand in (helm-marked-candidates)
                           do (setq kill-ring
                                    (delete cand kill-ring))))))
  "List of actions for kill ring source."
  :group 'helm-ring
  :type '(alist :key-type string :value-type function))


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

(defvar helm-source-kill-ring
  (helm-build-sync-source "Kill Ring"
    :init (lambda () (helm-attrset 'last-command last-command))
    :candidates #'helm-kill-ring-candidates
    :filtered-candidate-transformer #'helm-kill-ring-transformer
    :action 'helm-kill-ring-actions
    :persistent-action (lambda (_candidate) (ignore))
    :persistent-help "DoNothing"
    :keymap helm-kill-ring-map
    :migemo t
    :multiline t)
  "Source for browse and insert contents of kill-ring.")

(defun helm-kill-ring-candidates ()
  (cl-loop for kill in (helm-fast-remove-dups kill-ring :test 'equal)
        unless (or (< (length kill) helm-kill-ring-threshold)
                   (string-match "\\`[\n[:blank:]]+\\'" kill))
        collect kill))

(defun helm-kill-ring-transformer (candidates _source)
  "Display only the `helm-kill-ring-max-lines-number' lines of candidate."
  (cl-loop for i in candidates
           when (get-text-property 0 'read-only i)
           do (set-text-properties 0 (length i) '(read-only nil) i)
           for nlines = (with-temp-buffer (insert i) (count-lines (point-min) (point-max)))
           if (and helm-kill-ring-max-lines-number
                   (> nlines helm-kill-ring-max-lines-number))
           collect (cons
                    (with-temp-buffer
                      (insert i)
                      (goto-char (point-min))
                      (concat
                       (buffer-substring
                        (point-min)
                        (save-excursion
                          (forward-line helm-kill-ring-max-lines-number)
                          (point)))
                       "[...]")) i)
           else collect i))

(defun helm-kill-ring-action (str)
  "Insert STR in `kill-ring' and set STR to the head.
If this action is executed just after `yank',
replace with STR as yanked string."
  (with-helm-current-buffer
    (setq kill-ring (delete str kill-ring))
    (if (not (eq (helm-attr 'last-command helm-source-kill-ring) 'yank))
        (insert-for-yank str)
      ;; from `yank-pop'
      (let ((inhibit-read-only t)
            (before (< (point) (mark t))))
        (if before
            (funcall (or yank-undo-function 'delete-region) (point) (mark t))
          (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
        (setq yank-undo-function nil)
        (set-marker (mark-marker) (point) helm-current-buffer)
        (insert-for-yank str)
        ;; Set the window start back where it was in the yank command,
        ;; if possible.
        (set-window-start (selected-window) yank-window-start t)
        (when before
          ;; This is like exchange-point-and-mark, but doesn't activate the mark.
          ;; It is cleaner to avoid activation, even though the command
          ;; loop would deactivate the mark because we inserted text.
          (goto-char (prog1 (mark t)
                       (set-marker (mark-marker) (point) helm-current-buffer))))))
    (kill-new str)))


;;;; <Mark ring>
;; DO NOT use these sources with other sources use
;; the commands `helm-mark-ring', `helm-global-mark-ring' or
;; `helm-all-mark-rings' instead.

(defun helm-mark-ring-line-string-at-pos (pos)
  "Return line string at position POS."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((line (car (split-string (thing-at-point 'line) "[\n\r]"))))
      (if (string= "" line)
          "<EMPTY LINE>"
        line))))

(defun helm-mark-ring-get-candidates ()
  (with-helm-current-buffer
    (cl-loop with marks = (if (mark t) (cons (mark-marker) mark-ring) mark-ring)
             for i in marks
             with max-line-number = (line-number-at-pos (point-max))
             with width = (length (number-to-string max-line-number))
             for m = (format (concat "%" (number-to-string width) "d: %s")
                             (line-number-at-pos i)
                             (helm-mark-ring-line-string-at-pos i))
             unless (and recip (member m recip))
             collect m into recip
             finally return recip)))

(defvar helm-source-mark-ring
  (helm-build-sync-source "mark-ring"
    :candidates #'helm-mark-ring-get-candidates
    :action '(("Goto line"
               . (lambda (candidate)
                   (helm-goto-line (string-to-number candidate))))) 
    :persistent-action (lambda (candidate)
                         (helm-goto-line (string-to-number candidate))
                         (helm-highlight-current-line))
    :persistent-help "Show this line"))

;;; Global-mark-ring
(defvar helm-source-global-mark-ring
  (helm-build-sync-source "global-mark-ring"
    :candidates #'helm-global-mark-ring-get-candidates
    :action '(("Goto line"
              . (lambda (candidate)
                  (let ((items (split-string candidate ":")))
                    (switch-to-buffer (cl-second items))
                    (helm-goto-line (string-to-number (car items)))))))
    :persistent-action (lambda (candidate)
                         (let ((items (split-string candidate ":")))
                           (switch-to-buffer (cl-second items))
                           (helm-goto-line (string-to-number (car items)))
                           (helm-highlight-current-line)))
    :persistent-help "Show this line"))

(defun helm-global-mark-ring-format-buffer (marker)
  (with-current-buffer (marker-buffer marker)
    (goto-char marker)
    (forward-line 0)
    (let ((line (pcase (thing-at-point 'line)
                  ((and line (pred stringp)
                        (guard (not (string-match-p "\\`\n?\\'" line))))
                   (car (split-string line "[\n\r]")))
                  (_ "<EMPTY LINE>"))))
      (format "%7d:%s:    %s"
              (line-number-at-pos) (marker-buffer marker) line))))

(defun helm-global-mark-ring-get-candidates ()
  (let ((marks global-mark-ring))
    (when marks
      (cl-loop for i in marks
               for mb = (marker-buffer i)
               for gm = (unless (or (string-match "^ " (format "%s" mb))
                                    (null mb))
                          (helm-global-mark-ring-format-buffer i))
               when (and gm (not (member gm recip)))
               collect gm into recip
               finally return recip))))

(defun helm--push-mark (&optional location nomsg activate)
  "[Internal] Don't use directly, use instead `helm-push-mark-mode'."
  (unless (null (mark t))
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (when (> (length mark-ring) mark-ring-max)
      (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
      (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil)))
  (set-marker (mark-marker) (or location (point)) (current-buffer))
  ;; Now push the mark on the global mark ring.
  (setq global-mark-ring (cons (copy-marker (mark-marker))
                               ;; Avoid having multiple entries
                               ;; for same buffer in `global-mark-ring'.
                               (cl-loop with mb = (current-buffer)
                                        for m in global-mark-ring
                                        for nmb = (marker-buffer m)
                                        unless (eq mb nmb)
                                        collect m)))
  (when (> (length global-mark-ring) global-mark-ring-max)
    (move-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
    (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil))
  (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
      (message "Mark set"))
  (when (or activate (not transient-mark-mode))
    (set-mark (mark t)))
  nil)

(defadvice push-mark (around helm-push-mark-mode)
  (helm--push-mark location nomsg activate))

;;;###autoload
(define-minor-mode helm-push-mark-mode
    "Provide an improved version of `push-mark'.
Modify the behavior of `push-mark' to update
the `global-mark-ring' after each new visit."
  :group 'helm-ring
  :global t
  (if helm-push-mark-mode
      (if (fboundp 'advice-add)
          (advice-add 'push-mark :override #'helm--push-mark)
          (ad-activate 'push-mark))
      (if (fboundp 'advice-remove)
          (advice-remove 'push-mark #'helm--push-mark)
          (ad-deactivate 'push-mark))))

;;;; <Register>
;;; Insert from register
(defvar helm-source-register
  (helm-build-sync-source "Registers"
    :candidates #'helm-register-candidates
    :action-transformer #'helm-register-action-transformer
    :persistent-help ""
    :multiline t
    :action '(("Delete Register(s)" .
               (lambda (_candidate)
                 (cl-loop for candidate in (helm-marked-candidates)
                          for register = (car candidate)
                          do (setq register-alist
                                (delq (assoc register register-alist)
                                      register-alist)))))))
  "See (info \"(emacs)Registers\")")

(defun helm-register-candidates ()
  "Collecting register contents and appropriate commands."
  (cl-loop for (char . val) in register-alist
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
          ((and (vectorp val)
                (fboundp 'undo-tree-register-data-p)
                (undo-tree-register-data-p (elt val 1)))
           (list
            "Undo-tree entry."
            'undo-tree-restore-state-from-register))
          ((or (and (vectorp val) (eq 'registerv (aref val 0)))
               (and (consp val) (frame-configuration-p (car val))))
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
                     val 0 (min (length val) helm-register-max-offset))
                    (if (> (length val) helm-register-max-offset)
                        "[...]" ""))
            'insert-register
            'append-to-register
            'prepend-to-register)))
        unless (null string-actions) ; Fix Issue #1107.
        collect (cons (format "Register %3s:\n %s" key (car string-actions))
                      (cons char (cdr string-actions)))))

(defun helm-register-action-transformer (actions register-and-functions)
  "Decide actions by the contents of register."
  (cl-loop with transformer-actions = nil
           with func-actions =
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
           "Restore Undo-tree register" .
           (lambda (c) (and (fboundp 'undo-tree-restore-state-from-register)
                            (undo-tree-restore-state-from-register (car c))))))
        for func in (cdr register-and-functions)
        for cell = (assq func func-actions)
        when cell
        do (push (cdr cell) transformer-actions)
        finally return (append (nreverse transformer-actions) actions)))

;;;###autoload
(defun helm-mark-ring ()
  "Preconfigured `helm' for `helm-source-mark-ring'."
  (interactive)
  (helm :sources 'helm-source-mark-ring
        :resume 'noresume
        :buffer "*helm mark*"))

;;;###autoload
(defun helm-global-mark-ring ()
  "Preconfigured `helm' for `helm-source-global-mark-ring'."
  (interactive)
  (helm :sources 'helm-source-global-mark-ring
        :resume 'noresume
        :buffer "*helm global mark*"))

;;;###autoload
(defun helm-all-mark-rings ()
  "Preconfigured `helm' for `helm-source-global-mark-ring' and \
`helm-source-mark-ring'."
  (interactive)
  (helm :sources '(helm-source-mark-ring
                   helm-source-global-mark-ring)
        :resume 'noresume
        :buffer "*helm mark ring*"))

;;;###autoload
(defun helm-register ()
  "Preconfigured `helm' for Emacs registers."
  (interactive)
  (helm :sources 'helm-source-register
        :resume 'noresume
        :buffer "*helm register*"))

;;;###autoload
(defun helm-show-kill-ring ()
  "Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.

First call open the kill-ring browser, next calls move to next line."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (helm :sources helm-source-kill-ring
          :buffer "*helm kill ring*"
          :resume 'noresume
          :allow-nest t)))

;;;###autoload
(defun helm-execute-kmacro ()
  "Preconfigured helm for keyboard macros.
Define your macros with `f3' and `f4'.
See (info \"(emacs) Keyboard Macros\") for detailed infos.
This command is useful when used with persistent action."
  (interactive)
  (helm :sources
        (helm-build-sync-source "Kmacro"
          :candidates (lambda ()
                        (helm-fast-remove-dups
                         (cons (kmacro-ring-head)
                               kmacro-ring)
                         :test 'equal))
          :multiline t
          :candidate-transformer
          (lambda (candidates)
            (cl-loop for c in candidates collect
                     (propertize (help-key-description (car c) nil)
                                 'helm-realvalue c)))
          :persistent-help "Execute kmacro"
          :help-message 'helm-kmacro-help-message
          :action
          (helm-make-actions
           "Execute kmacro (`C-u <n>' to execute <n> times)"
           (lambda (candidate)
             (interactive)
             ;; Move candidate on top of list for next use.
             (setq kmacro-ring (delete candidate kmacro-ring))
             (kmacro-push-ring)
             (kmacro-split-ring-element candidate)
             (kmacro-exec-ring-item
              candidate helm-current-prefix-arg))))
        :buffer "*helm kmacro*"))

(provide 'helm-ring)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-ring.el ends here
