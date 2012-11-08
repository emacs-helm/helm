;;; helm-w3m.el --- W3m bookmark - helm interface.

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
(require 'helm-adaptative)
;; Some users have the emacs-w3m library in load-path
;; without having the w3m executable :-;
;; So check if w3m program is present before trying to load
;; emacs-w3m.
(eval-when-compile
  (when (executable-find "w3m")
    (require 'w3m-bookmark nil t)))


(defgroup helm-w3m nil
  "W3m related Applications and libraries for Helm."
  :group 'helm)

(defface helm-w3m-bookmarks '((t (:foreground "cyan1" :underline t)))
  "Face for w3m bookmarks" :group 'helm-w3m)


(defvar w3m-bookmark-file "~/.w3m/bookmark.html")
(defvar helm-w3m-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")
(defvar helm-w3m-bookmark-url-regexp "\\(https\\|http\\|ftp\\|file\\)://[^>]*")
(defvar helm-c-w3m-bookmarks-alist nil)
(defvar helm-c-source-w3m-bookmarks
  '((name . "W3m Bookmarks")
    (init . (lambda ()
              (setq helm-c-w3m-bookmarks-alist
                    (helm-html-bookmarks-to-alist
                     w3m-bookmark-file
                     helm-w3m-bookmark-url-regexp
                     helm-w3m-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car helm-c-w3m-bookmarks-alist)))
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-w3m-bookmarks)
    (action . (("Browse Url"
                . (lambda (candidate)
                    (helm-c-w3m-browse-bookmark candidate)))
               ("Copy Url"
                . (lambda (elm)
                    (kill-new (helm-c-w3m-bookmarks-get-value elm))))
               ("Browse Url Externally"
                . (lambda (candidate)
                    (helm-c-w3m-browse-bookmark candidate t)))
               ("Delete Bookmark"
                . (lambda (candidate)
                    (helm-c-w3m-delete-bookmark candidate)))
               ("Rename Bookmark"
                . (lambda (candidate)
                    (helm-c-w3m-rename-bookmark candidate)))))
    (persistent-action . (lambda (candidate)
                           (if current-prefix-arg
                               (helm-c-w3m-browse-bookmark candidate t)
                               (helm-c-w3m-browse-bookmark candidate nil t))))
    (persistent-help . "Open URL with emacs-w3m in new tab / \
C-u \\[helm-execute-persistent-action]: Open URL with Firefox"))
  "Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/")


(defun helm-c-w3m-bookmarks-get-value (elm)
  (replace-regexp-in-string
   "\"" "" (cdr (assoc elm helm-c-w3m-bookmarks-alist))))

(defun helm-c-w3m-browse-bookmark (elm &optional use-external new-tab)
  (let* ((fn  (if use-external 'helm-c-browse-url 'w3m-browse-url))
         (arg (and (eq fn 'w3m-browse-url) new-tab)))
    (funcall fn (helm-c-w3m-bookmarks-get-value elm) arg)))

(defun helm-c-highlight-w3m-bookmarks (bookmarks source)
  (loop for i in bookmarks
        collect (propertize
                 i 'face 'helm-w3m-bookmarks
                 'help-echo (helm-c-w3m-bookmarks-get-value i))))


(defun helm-c-w3m-delete-bookmark (elm)
  "Delete w3m bookmark from `w3m-bookmark-file'."
  (with-current-buffer
      (find-file-literally w3m-bookmark-file)
    (goto-char (point-min))
    (when (re-search-forward elm nil t)
      (beginning-of-line)
      (delete-region (point)
                     (line-end-position))
      (delete-blank-lines))
    (save-buffer)
    (kill-buffer)))

(defun helm-c-w3m-rename-bookmark (elm)
  "Rename w3m bookmark in `w3m-bookmark-file'."
  (let* ((old-title (replace-regexp-in-string ">" "" elm))
         (new-title (read-string "NewTitle: " old-title)))
    (with-current-buffer
        (find-file-literally w3m-bookmark-file)
      (goto-char (point-min))
      (when (re-search-forward (concat elm "<") nil t)
        (goto-char (1- (point)))
        (delete-char (- (length old-title)))
        (insert new-title))
      (save-buffer)
      (kill-buffer))))

;;;###autoload
(defun helm-w3m-bookmarks ()
  "Preconfigured `helm' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/"
  (interactive)
  (helm-other-buffer 'helm-c-source-w3m-bookmarks
                     "*helm w3m bookmarks*"))


(provide 'helm-w3m)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-w3m.el ends here
