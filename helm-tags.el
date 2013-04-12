;;; helm-tags.el --- Helm for Etags and Ctags.

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

(eval-when-compile (require 'cl))
(require 'helm)


(defgroup helm-tags nil
  "Tags related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-etags-tag-file-name "TAGS"
  "Etags tag file name."
  :type  'string
  :group 'helm-tags)

(defcustom helm-etags-tag-file-search-limit 10
  "The limit level of directory to search tag file.
Don't search tag file deeply if outside this value."
  :type  'number
  :group 'helm-tags)


(defvar helm-etags-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-c ?")    'helm-etags-help)
    map)
  "Keymap used in Etags.")


;;; Ctags
;;
;;
(defvar helm-ctags-modes
  '( c-mode c++-mode awk-mode csharp-mode java-mode javascript-mode lua-mode
    makefile-mode pascal-mode perl-mode cperl-mode php-mode python-mode
    scheme-mode sh-mode slang-mode sql-mode tcl-mode ))

(defun helm-ctags-init ()
  (when (and buffer-file-name
             (memq major-mode helm-ctags-modes)
             (helm-current-buffer-is-modified))
    (with-current-buffer (helm-candidate-buffer 'local)
      (call-process-shell-command
       (if (string-match "\\.el\\.gz$" helm-buffer-file-name)
           (format "ctags -e -u -f- --language-force=lisp --fields=n =(zcat %s) "
                   helm-buffer-file-name)
           (format "ctags -e -u -f- --fields=n %s " helm-buffer-file-name))
       nil (current-buffer))
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point-min) (point))
      (loop while (and (not (eobp)) (search-forward "\001" (point-at-eol) t))
            for lineno-start = (point)
            for lineno = (buffer-substring
                          lineno-start
                          (1- (search-forward "," (point-at-eol) t)))
            do
            (forward-line 0)
            (insert (format "%5s:" lineno))
            (search-forward "\177" (point-at-eol) t)
            (delete-region (1- (point)) (point-at-eol))
            (forward-line 1)))))

(defvar helm-source-ctags
  '((name . "Exuberant ctags")
    (init . helm-ctags-init)
    (candidates-in-buffer)
    (adjust)
    (type . line))
  "Needs Exuberant Ctags.

http://ctags.sourceforge.net/")


;;; Etags
;;
;;
(defvar helm-etags-tag-file-dir nil
  "Etags file directory.")
(defvar helm-etags-mtime-alist nil
  "Store the last modification time of etags files here.")
(defvar helm-etags-cache (make-hash-table :test 'equal)
  "Cache content of etags files used here for faster access.")

(defun helm-etags-get-tag-file (&optional directory)
  "Return the path of etags file if found."
  ;; Get tag file from `default-directory' or upper directory.
  (let ((current-dir (helm-etags-find-tag-file-directory
                      (or directory default-directory))))
    ;; Return nil if not find tag file.
    (when current-dir
      ;; Set tag file directory.
      (setq helm-etags-tag-file-dir current-dir)
      (expand-file-name helm-etags-tag-file-name current-dir))))

(defun helm-etags-find-tag-file-directory (current-dir)
  "Try to find the directory containing tag file.
If not found in CURRENT-DIR search in upper directory."
  (let ((file-exists? #'(lambda (dir)
                          (let ((tag-path (expand-file-name
                                           helm-etags-tag-file-name dir)))
                            (and (stringp tag-path)
                                 (file-regular-p tag-path)
                                 (file-readable-p tag-path))))))
    (loop with count = 0
          until (funcall file-exists? current-dir)
          ;; Return nil if outside the value of
          ;; `helm-etags-tag-file-search-limit'.
          if (= count helm-etags-tag-file-search-limit)
          do (return nil)
          ;; Or search upper directories.
          else
          do (incf count)
          (setq current-dir (expand-file-name (concat current-dir "../")))
          finally return current-dir)))

(defun helm-etags-get-header-name (x)
  "Create header name for this helm etags session."
  (concat "Etags in "
          (with-helm-current-buffer
            (helm-etags-get-tag-file))))

(defun helm-etags-create-buffer (file)
  "Create the `helm-buffer' based on contents of etags tag FILE."
  (let* ((tag-fname file)
          max
          (split (with-current-buffer (find-file-noselect tag-fname)
                   (prog1
                       (split-string (buffer-string) "\n" 'omit-nulls)
                     (setq max (line-number-at-pos (point-max)))
                     (kill-buffer))))
          (progress-reporter (make-progress-reporter "Loading tag file..." 0 max)))
     (loop
           with fname
           with cand
           for i in split for count from 0
           for elm = (unless (string-match "^\x0c" i)
                       (helm-aif (string-match "\177" i)
                           (substring i 0 it)
                         i))
           do (cond ((and elm (string-match "^\\([^,]+\\),[0-9]+$" elm))
                     (setq fname (match-string 1 elm)))
                    (elm (setq cand (concat fname ": " elm)))
                    (t (setq cand nil)))
           when cand do (progn
                          (insert (concat cand "\n"))
                          (progress-reporter-update progress-reporter count)))))

(defun helm-etags-init ()
  "Feed `helm-buffer' using `helm-etags-cache' or tag file.
If no entry in cache, create one."
  (let ((tagfile (helm-etags-get-tag-file)))
    (when tagfile
      (with-current-buffer (helm-candidate-buffer 'global)
        (helm-aif (gethash tagfile helm-etags-cache)
            ;; An entry is present in cache, insert it.
            (insert it)
          ;; No entry, create a new buffer using content of tag file (slower).
          (helm-etags-create-buffer tagfile)
          ;; Store content of buffer in cache.
          (puthash tagfile (buffer-string) helm-etags-cache)
          ;; Store or set the last modification of tag file.
          (helm-aif (assoc tagfile helm-etags-mtime-alist)
              ;; If an entry exists modify it.
              (setcdr it (helm-etags-mtime tagfile))
            ;; No entry create a new one.
            (add-to-list 'helm-etags-mtime-alist
                         (cons tagfile (helm-etags-mtime tagfile)))))))))

(defvar helm-source-etags-select
  `((name . "Etags")
    (header-name . helm-etags-get-header-name)
    (init . helm-etags-init)
    (candidates-in-buffer)
    (match-part . (lambda (candidate)
                    ;; Match only the tag part of CANDIDATE
                    ;; and not the filename.
                    (cadr (split-string candidate ":"))))
    (mode-line . helm-etags-mode-line-string)
    (keymap . ,helm-etags-map)
    (action . helm-etags-default-action)
    (persistent-action . (lambda (candidate)
                           (helm-etags-default-action candidate)
                           (helm-match-line-color-current-line))))
  "Helm source for Etags.")

(defvar find-tag-marker-ring)

(defun helm-etags-default-action (candidate)
  "Helm default action to jump to an etags entry."
  (require 'etags)
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (let* ((split (split-string candidate ": "))
         (fname (expand-file-name
                 (car split) helm-etags-tag-file-dir))
         (elm   (cadr split)))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-file fname)
    (goto-char (point-min))
    (search-forward elm nil t)
    (goto-char (match-beginning 0))))

(defun helm-etags-mtime (file)
  "Last modification time of etags tag FILE."
  (cadr (nth 5 (file-attributes file))))

(defun helm-etags-file-modified-p (file)
  "Check if tag FILE have been modified in this session.
If FILE is nil return nil."
  (let ((last-modif (and file
                         (assoc-default file helm-etags-mtime-alist))))
    (and last-modif
         (/= last-modif (helm-etags-mtime file)))))


;;;###autoload
(defun helm-etags-select (arg)
  "Preconfigured helm for etags.
Called with one prefix arg use symbol at point as initial input.
Called with two prefix arg reinitialize cache.
If tag file have been modified reinitialize cache."
  (interactive "P")
  (let ((tag  (helm-etags-get-tag-file))
        (helm-execute-action-at-once-if-one t))
    (when (or (equal arg '(4))
              (and helm-etags-mtime-alist
                   (helm-etags-file-modified-p tag)))
      (remhash tag helm-etags-cache))
    (if (and tag (file-exists-p tag))
        (helm :sources 'helm-source-etags-select
              :keymap helm-etags-map
              :default (concat "\\_<" (thing-at-point 'symbol) "\\_>")
              :buffer "*helm etags*")
        (message "Error: No tag file found, please create one with etags shell command."))))

(provide 'helm-tags)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-tags.el ends here
