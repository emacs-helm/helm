;;; helm-bmkext.el --- Sources to filter bookmark-extensions bookmarks.

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
(require 'helm-bookmark)
(require 'helm-adaptative)

(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))

;; Dependency: https://github.com/thierryvolpiatto/emacs-bmk-ext
;; If you want to enable google-maps in addressbook you will need
;; Julien Danjou google-maps-el package available here:
;; http://julien.danjou.info/google-maps-el.html

(defun helm-c-bmkext-filter-setup-alist (fn &rest args)
  "Return a filtered `bookmark-alist' sorted alphabetically."
  (loop
        with alist = (if args
                         (apply #'(lambda (x) (funcall fn x)) args)
                         (funcall fn))
        for i in alist
        for b = (car i)
        collect b into sa
        finally return (sort sa 'string-lessp)))

;;;###autoload
(defun helm-c-bmkext-run-edit ()
  "Run `bmkext-edit-bookmark' from keyboard."
  (interactive)
  (helm-c-quit-and-execute-action 'bmkext-edit-bookmark))

;;; Addressbook.
;;
;;
(defvar helm-c-source-bmkext-addressbook
  '((name . "Bookmark Addressbook")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               "*helm bmkext addressbook*"
               (helm-c-bmkext-addressbook-setup-alist))))
    (candidates-in-buffer)
    (persistent-action
     . (lambda (candidate)
         (let ((bmk (helm-bookmark-get-bookmark-from-name
                     candidate)))
           (bookmark--jump-via bmk 'pop-to-buffer))))
    (persistent-help . "Show contact - Prefix with C-u to append")
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (action . (("Show Contact(s)"
                . (lambda (candidate)
                    (let* ((contacts (helm-marked-candidates))
                           (current-prefix-arg (or helm-current-prefix-arg
                                                   (> (length contacts) 1))))
                      (bookmark-jump
                       (helm-bookmark-get-bookmark-from-name (car contacts)))
                      (helm-aif (cdr contacts)
                          (loop for bmk in it do
                                (bookmark-jump
                                 (helm-bookmark-get-bookmark-from-name bmk)))))))
               ("Send Mail"
                . (lambda (candidate)
                    (let* ((contacts (helm-marked-candidates))
                           (bmk      (helm-bookmark-get-bookmark-from-name
                                      (car contacts)))
                           (append   (message-buffers)))
                      (if append
                          (addressbook-set-mail-buffer1 bmk 'append)
                          (addressbook-set-mail-buffer1 bmk))
                      (setq contacts (cdr contacts))
                      (when contacts
                        (loop for bmk in contacts do
                              (addressbook-set-mail-buffer1 bmk 'append))))))
               ("Edit Bookmark"
                . (lambda (candidate)
                    (let ((bmk (helm-bookmark-get-bookmark-from-name
                                candidate)))
                      (addressbook-bookmark-edit
                       (assoc bmk bookmark-alist)))))
               ("Insert Email at point"
                . (lambda (candidate)
                    (let* ((bmk   (helm-bookmark-get-bookmark-from-name
                                   candidate))
                           (mlist (split-string
                                   (assoc-default
                                    'email (assoc bmk bookmark-alist))
                                   ", ")))
                      (insert
                       (if (> (length mlist) 1)
                           (helm-comp-read
                            "Insert Mail Address: " mlist :must-match t)
                           (car mlist))))))
               ("Show annotation"
                . (lambda (candidate)
                    (let ((bmk (helm-bookmark-get-bookmark-from-name
                                candidate)))
                      (bookmark-show-annotation bmk))))
               ("Edit annotation"
                . (lambda (candidate)
                    (let ((bmk (helm-bookmark-get-bookmark-from-name
                                candidate)))
                      (bookmark-edit-annotation bmk))))
               ("Show Google map"
                . (lambda (candidate)
                    (let* ((bmk (helm-bookmark-get-bookmark-from-name
                                 candidate))
                           (full-bmk (assoc bmk bookmark-alist)))
                      (addressbook-google-map full-bmk))))))))


(defun helm-c-bmkext-addressbook-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-c-bmkext-filter-setup-alist 'bmkext-addressbook-alist-only))

;; W3m bookmarks from bookmark-extensions.
(defvar helm-c-source-bookmark-w3m
  '((name . "Bookmark W3m")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               "*helm bmkext w3m*" (helm-c-bookmark-w3m-setup-alist))))
    (candidates-in-buffer)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-w3m-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-c-bmkext-filter-setup-alist 'bmkext-w3m-alist-only))

;; Images
(defvar helm-c-source-bookmark-images
  '((name . "Bookmark Images")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               "*helm bmkext images*" (helm-c-bookmark-images-setup-alist))))
    (candidates-in-buffer)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-images-setup-alist ()
  "Specialized filter function for images bookmarks."
  (helm-c-bmkext-filter-setup-alist 'bmkext-image-file-alist-only))

;; Woman Man
(defvar helm-c-source-bookmark-man
  '((name . "Bookmark Woman&Man")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               "*helm bmkext man*" (helm-c-bookmark-man-setup-alist))))
    (candidates-in-buffer)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-man-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (append (helm-c-bmkext-filter-setup-alist 'bmkext-man-alist-only)
          (helm-c-bmkext-filter-setup-alist 'bmkext-woman-alist-only)))

;; Gnus
(defvar helm-c-source-bookmark-gnus
  '((name . "Bookmark Gnus")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               "*helm bmkext gnus*" (helm-c-bookmark-gnus-setup-alist))))
    (candidates-in-buffer)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-gnus-setup-alist ()
  "Specialized filter function for bookmarks gnus."
  (helm-c-bmkext-filter-setup-alist 'bmkext-gnus-alist-only))

;; Info
(defvar helm-c-source-bookmark-info
  '((name . "Bookmark Info")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               "*helm bmkext info*" (helm-c-bookmark-info-setup-alist))))
    (candidates-in-buffer)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-info-setup-alist ()
  "Specialized filter function for bookmarks info."
  (helm-c-bmkext-filter-setup-alist 'bmkext-info-alist-only))

;; Local Files&directories
(defvar helm-c-source-bookmark-files&dirs
  '((name . "Bookmark Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               "*helm bmkext files*" (helm-c-bookmark-local-files-setup-alist))))
    (candidates-in-buffer)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark)
    (type . bookmark)))

(defun helm-c-bookmark-local-files-setup-alist ()
  "Specialized filter function for bookmarks locals files."
  (helm-c-bmkext-filter-setup-alist 'bmkext-local-file-alist-only))

;; Su Files&directories
(defvar helm-c-source-bookmark-su-files&dirs
  '((name . "Bookmark Root-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               "*helm bmkext su*" (helm-c-bookmark-su-files-setup-alist))))
    (candidates-in-buffer)
    (filtered-candidate-transformer
     helm-c-adaptive-sort
     helm-c-highlight-bookmark-su)
    (type . bookmark)))

(defun helm-c-bookmark-su-files-setup-alist ()
  "Specialized filter function for bookmarks su/sudo files."
  (declare (special bmkext-su-or-sudo-regexp))
  (loop
        with l = (helm-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
        for i in l
        for isfile = (bookmark-get-filename i)
        for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                           (save-match-data
                             (string-match tramp-file-name-regexp isfile)))
        for issu = (and istramp
                        (string-match bmkext-su-or-sudo-regexp isfile))
        if issu
        collect i))

;; Ssh Files&directories
(defvar helm-c-source-bookmark-ssh-files&dirs
  '((name . "Bookmark Ssh-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               "*helm bmkext ssh*" (helm-c-bookmark-ssh-files-setup-alist))))
    (candidates-in-buffer)
    (filtered-candidate-transformer . helm-c-adaptive-sort)
    (type . bookmark)))

(defun helm-c-bookmark-ssh-files-setup-alist ()
  "Specialized filter function for bookmarks ssh files."
  (loop
        with l = (helm-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
        for i in l
        for isfile = (bookmark-get-filename i)
        for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                           (save-match-data
                             (string-match tramp-file-name-regexp isfile)))
        for isssh = (and istramp
                         (string-match "/ssh:" isfile))
        if isssh
        collect i))

;;;###autoload
(defun helm-bookmark-ext ()
  "Preconfigured `helm' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `helm-c-source-google-suggest'."
  (interactive)
  (helm
   :sources
   '(helm-c-source-bookmark-files&dirs
     helm-c-source-bookmark-w3m
     helm-c-source-google-suggest
     helm-c-source-bmkext-addressbook
     helm-c-source-bookmark-gnus
     helm-c-source-bookmark-info
     helm-c-source-bookmark-man
     helm-c-source-bookmark-images
     helm-c-source-bookmark-su-files&dirs
     helm-c-source-bookmark-ssh-files&dirs
     helm-c-source-bookmark-set)
   :prompt "SearchBookmark: "
   :buffer "*helm bmkext*"
   :default (buffer-name helm-current-buffer)))

(provide 'helm-bmkext)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-bmkext.el ends here
