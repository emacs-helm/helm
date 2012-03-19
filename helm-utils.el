;;; helm-utils.el --- Utilities Functions for helm.

;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; X-URL: <https://github.com/emacs-helm/helm>

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software: you can redistribute it and/or modify
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

(defun helm-ff-find-printers ()
  "Return a list of available printers on Unix systems."
  (when (executable-find "lpstat")
    (let ((printer-list (with-temp-buffer
                          (call-process "lpstat" nil t nil "-a")
                          (split-string (buffer-string) "\n"))))
      (loop for p in printer-list
            for printer = (car (split-string p))
            when printer
            collect printer))))

;; Shut up byte compiler in emacs24*.
(defun helm-c-switch-to-buffer (buffer-or-name)
  "Same as `switch-to-buffer' whithout warnings at compile time."
  (with-no-warnings
    (switch-to-buffer buffer-or-name)))

(defun* helm-c-position (item seq &key (test 'eq) all)
  "A simple and faster replacement of CL `position'.
Return position of first occurence of ITEM found in SEQ.
Argument SEQ can be a string, in this case ITEM have to be a char.
Argument ALL, if non--nil specify to return a list of positions of
all ITEM found in SEQ."
  (let ((key (if (stringp seq) 'across 'in)))
    (eval
     `(loop for c ,key seq
            for index from 0
            when (funcall test c item)
            if all collect index into ls
            else return index
            finally return ls))))

(defun helm-c-get-pid-from-process-name (process-name)
  "Get pid from running process PROCESS-NAME."
  (loop with process-list = (list-system-processes)
        for pid in process-list
        for process = (assoc-default 'comm (process-attributes pid))
        when (and process (string-match process-name process))
        return pid))

(defun* helm-current-buffer-narrowed-p (&optional
                                        (buffer helm-current-buffer))
  "Check if BUFFER is narrowed.
Default is `helm-current-buffer'."
  (with-current-buffer buffer
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun helm-region-active-p ()
  (and transient-mark-mode mark-active (/= (mark) (point))))

(defun helm-goto-char (loc)
  "Go to char, revealing if necessary."
  (goto-char loc)
  (when (or (eq major-mode 'org-mode)
            (and (boundp 'outline-minor-mode)
                 outline-minor-mode))
    (require 'org) ; On some old Emacs versions org may not be loaded.
    (org-reveal)))

(defun helm-goto-line (lineno &optional noanim)
  "Goto LINENO opening only outline headline if needed.
Animation is used unless NOANIM is non--nil."
  (goto-char (point-min))
  (helm-goto-char (point-at-bol lineno))
  (unless noanim
    (helm-match-line-color-current-line)
    (sit-for 0.3)
    (helm-match-line-cleanup)))

(defun helm-show-this-source-only ()
  "Show all candidates of this source."
  (interactive)
  (let (helm-candidate-number-limit)
    (helm-set-source-filter
     (list (assoc-default 'name (helm-get-current-source))))))

;;;###autoload
(defun helm-test-sources ()
  "List all helm sources for test.
The output is sexps which are evaluated by \\[eval-last-sexp]."
  (interactive)
  (with-output-to-temp-buffer "*Helm Test Sources*"
    (mapc (lambda (s) (princ (format ";; (helm '%s)\n" s)))
          (apropos-internal "^helm-c-source" #'boundp))
    (pop-to-buffer standard-output)))

(defun helm-displaying-source-names ()
  "Display sources name."
  (with-current-buffer helm-buffer
    (goto-char (point-min))
    (loop with pos
          while (setq pos (next-single-property-change (point) 'helm-header))
          do (goto-char pos)
          collect (buffer-substring-no-properties (point-at-bol)(point-at-eol))
          do (forward-line 1))))

(defun helm-c-match-on-file-name (candidate)
  "Return non-nil if `helm-pattern' match basename of filename CANDIDATE."
  (string-match helm-pattern (file-name-nondirectory candidate)))

(defun helm-c-match-on-directory-name (candidate)
  "Return non-nil if `helm-pattern' match directory part of CANDIDATE."
  (helm-aif (file-name-directory candidate)
      (string-match helm-pattern it)))

(defun helm-c-match-on-basename (candidate)
  "Return non-nil if `helm-pattern' match basename of filename CANDIDATE."
  (string-match helm-pattern (helm-c-basename candidate)))

(defun helm-c-string-match (candidate)
  "Return non-nil if `helm-pattern' match CANDIDATE.
The match is done with `string-match'."
  (string-match helm-pattern candidate))

(defun helm-c-skip-entries (list regexp)
  "Remove entries which matches REGEXP from LIST."
  (remove-if (lambda (x) (and (stringp x) (string-match regexp x)))
             list))

(defun helm-c-shadow-entries (list regexp)
  "Display elements of LIST matching REGEXP with the `file-name-shadow' face."
  (mapcar (lambda (file)
            ;; Add shadow face property to boring files.
            (let ((face (if (facep 'file-name-shadow)
                            'file-name-shadow
                            ;; fall back to default on XEmacs
                            'default)))
              (if (string-match regexp file)
                  (setq file (propertize file 'face face))))
            file)
          list))

(defun helm-c-stringify (str-or-sym)
  "Get string of STR-OR-SYM."
  (if (stringp str-or-sym)
      str-or-sym
      (symbol-name str-or-sym)))

(defun helm-c-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (if (symbolp str-or-sym)
      str-or-sym
      (intern str-or-sym)))

(defun helm-c-describe-function (func)
  "FUNC is symbol or string."
  (describe-function (helm-c-symbolify func)))

(defun helm-c-describe-variable (var)
  "VAR is symbol or string."
  (describe-variable (helm-c-symbolify var)))

(defun helm-c-find-function (func)
  "FUNC is symbol or string."
  (find-function (helm-c-symbolify func)))

(defun helm-c-find-variable (var)
  "VAR is symbol or string."
  (find-variable (helm-c-symbolify var)))

(defun helm-c-kill-new (candidate &optional replace)
  "CANDIDATE is symbol or string.
See `kill-new' for argument REPLACE."
  (kill-new (helm-c-stringify candidate) replace))

(defun* helm-fast-remove-dups (seq &key (test 'eq))
  "Remove duplicates elements in list SEQ.
This is same as `remove-duplicates' but with memoisation.
It is much faster, especially in large lists.
A test function can be provided with TEST argument key.
Default is `eq'."
  (loop with cont = (make-hash-table :test test)
        for elm in seq
        unless (gethash elm cont)
        do (puthash elm elm cont)
        finally return
        (loop for i being the hash-values in cont collect i)))

(defadvice eval-defun (after helm-source-hack activate)
  "Allow immediate execution of helm source when evaling it.
See `helm-c-enable-eval-defun-hack'."
  (when helm-c-enable-eval-defun-hack
    (let ((varsym (save-excursion
                    (beginning-of-defun)
                    (forward-char 1)
                    (when (memq (read (current-buffer)) '(defvar setq))
                      (read (current-buffer))))))
      (when (string-match "^helm-c-source-" (symbol-name varsym))
        (helm varsym)))))
;; (progn (ad-disable-advice 'eval-defun 'after 'helm-source-hack) (ad-update 'eval-defun))

(defun helm-quit-and-find-file ()
  "Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory."
  (interactive)
  (helm-run-after-quit
   (lambda (f)
     (if (file-exists-p f)
         (helm-find-files-1 (file-name-directory f)
                            (if helm-ff-transformer-show-only-basename
                                (helm-c-basename f) f))
         (helm-find-files-1 f)))
   (helm-aif (get-buffer (helm-get-selection))
       (or (buffer-file-name it)
           (car (rassoc it dired-buffers))
           (and (with-current-buffer it
                  (eq major-mode 'org-agenda-mode))
                org-directory
                (expand-file-name org-directory))
           default-directory)
     (let ((sel (helm-get-selection)))
       (cond ((or (file-remote-p sel)
                  (file-exists-p sel))
              (expand-file-name sel))
             ((string-match ffap-url-regexp sel)
              sel)
             (t default-directory))))))

(defmacro* helm-c-walk-directory (directory &key path (directories t) match)
  "Walk through DIRECTORY tree.
PATH can be one of basename, relative, or full.
DIRECTORIES when non--nil (default) return also directories names, otherwise
skip directories names.
MATCH match only filenames matching regexp MATCH."
  `(let (result
         (fn (case ,path
               (basename 'file-name-nondirectory)
               (relative 'file-relative-name)
               (full     'identity)
               (t        'file-name-nondirectory))))
     (labels ((ls-R (dir)
                (loop with ls = (directory-files
                                 dir t directory-files-no-dot-files-regexp)
                      for f in ls
                      if (file-directory-p f)
                      do (progn (when ,directories
                                  (push (funcall fn f) result))
                                ;; Don't recurse in directory symlink.
                                (unless (file-symlink-p f)
                                  (ls-R f)))
                      else do
                      (unless (and ,match (not (string-match
                                                ,match
                                                (file-name-nondirectory f))))
                        (push (funcall fn f) result)))))
       (ls-R ,directory)
       (nreverse result))))

(defun helm-c-basename (fname &optional ext)
  "Print FNAME  with any  leading directory  components removed.
If specified, also remove filename extension EXT."
  (if (and ext (or (string= (file-name-extension fname) ext)
                   (string= (file-name-extension fname t) ext))
           (not (file-directory-p fname)))
      (file-name-sans-extension (file-name-nondirectory fname))
      (file-name-nondirectory (directory-file-name fname))))

(defun helm-file-human-size (size)
  "Return a string showing SIZE of a file in human readable form.
SIZE can be an integer or a float depending it's value.
`file-attributes' will take care of that to avoid overflow error.
KBSIZE if a floating point number, default value is 1024.0."
  (let ((M (cons "M" (/ size (expt helm-ff-default-kbsize 2))))
        (G (cons "G" (/ size (expt helm-ff-default-kbsize 3))))
        (K (cons "K" (/ size helm-ff-default-kbsize)))
        (B (cons "B" size)))
    (loop with result = B
          for (a . b) in
          (loop for (x . y) in (list M G K B)
                unless (< y 1) collect (cons x y))
          when (< b (cdr result)) do (setq result (cons a b))
          finally return (if (string= (car result) "B")
                             (format "%s" size)
                             (format "%.1f%s" (cdr result) (car result))))))

(defun* helm-file-attributes
    (file &key type links uid gid access-time modif-time
          status size mode gid-change inode device-num dired human-size)
  "Easy interface for `file-attributes'."
  (let ((all (destructuring-bind
                   (type links uid gid access-time modif-time
                         status size mode gid-change inode device-num)
                 (file-attributes file 'string)
               (list :type        type
                     :links       links
                     :uid         uid
                     :gid         gid
                     :access-time access-time
                     :modif-time  modif-time
                     :status      status
                     :size        size
                     :mode        mode
                     :gid-change  gid-change
                     :inode       inode
                     :device-num  device-num))))
    (cond (type
           (let ((result (getf all :type)))
             (cond ((stringp result)
                    "symlink")
                   (result "directory")
                   (t "file"))))
          (links (getf all :links))
          (uid   (getf all :uid))
          (gid   (getf all :gid))
          (access-time
           (format-time-string "%Y-%m-%d %R" (getf all :access-time)))
          (modif-time
           (format-time-string "%Y-%m-%d %R" (getf all :modif-time)))
          (status
           (format-time-string "%Y-%m-%d %R" (getf all :status)))
          (size (if human-size (helm-file-human-size (getf all :size))
                    (getf all :size)))
          (mode (getf all :mode))
          (gid-change (getf all :gid-change))
          (inode (getf all :inode))
          (device-num (getf all :device-num))
          (dired
           (concat
            (getf all :mode) " "
            (number-to-string (getf all :links)) " "
            (getf all :uid) ":"
            (getf all :gid) " "
            (if human-size (helm-file-human-size (getf all :size))
                (int-to-string (getf all :size))) " "
                (format-time-string "%Y-%m-%d %R" (getf all :modif-time))))
          (t all))))

(defun helm-c-current-directory ()
  "Return current-directory name at point.
Useful in dired buffers when there is inserted subdirs."
  (if (eq major-mode 'dired-mode)
      (dired-current-directory)
      default-directory))

(provide 'helm-utils)

;;; helm-utils.el ends here
