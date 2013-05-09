;;; helm-bmkext.el --- Sources to filter bookmark-extensions bookmarks.

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

;; Dependency: https://github.com/thierryvolpiatto/emacs-bmk-ext
;; If you want to enable google-maps in addressbook you will need
;; Julien Danjou google-maps-el package available here:
;; http://julien.danjou.info/google-maps-el.html

;;; Code:
(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-bookmark)
(require 'helm-adaptative)

(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))


;;; Filter functions
;;
(defun helm-bmkext-filter-setup-alist (fn &rest args)
  "Return a filtered `bookmark-alist' sorted alphabetically."
  (loop
        with alist = (if args
                         (apply #'(lambda (x) (funcall fn x)) args)
                         (funcall fn))
        for i in alist
        for b = (car i)
        collect (propertize b 'location (bookmark-location b)) into sa
        finally return (sort sa 'string-lessp)))

;;;###autoload
(defun helm-bmkext-run-edit ()
  "Run `bmkext-edit-bookmark' from keyboard."
  (interactive)
  (helm-quit-and-execute-action 'bmkext-edit-bookmark))


;;; Addressbook.
;;
;;
(defvar helm-source-bmkext-addressbook
  '((name . "Bookmark Addressbook")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               'global
               (helm-bmkext-addressbook-setup-alist))))
    (candidates-in-buffer)
    (search helm-bookmark-search-fn)
    (match-part . helm-pp-bookmark-match-fn)
    (persistent-action
     . (lambda (candidate)
         (let ((bmk (helm-bookmark-get-bookmark-from-name
                     candidate)))
           (bookmark--jump-via bmk 'switch-to-buffer))))
    (persistent-help . "Show contact - Prefix with C-u to append")
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-bookmark)
    (action . (("Show Contact(s)"
                . (lambda (candidate)
                    (let* ((contacts (helm-marked-candidates))
                           (current-prefix-arg helm-current-prefix-arg))
                      (bookmark-jump
                       (helm-bookmark-get-bookmark-from-name (car contacts)))
                      (helm-aif (cdr contacts)
                          (let ((current-prefix-arg '(4)))
                            (loop for bmk in it do
                                  (bookmark-jump
                                   (helm-bookmark-get-bookmark-from-name bmk))))))))
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
               ("Delete bookmark(s)" . helm-delete-marked-bookmarks)
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


(defun helm-bmkext-addressbook-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-bmkext-filter-setup-alist 'bmkext-addressbook-alist-only))


;;; W3m bookmarks from bookmark-extensions.
;;
(defvar helm-source-bookmark-w3m
  '((name . "Bookmark W3m")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               'global (helm-bookmark-w3m-setup-alist))))
    (candidates-in-buffer)
    (search helm-bookmark-search-fn)
    (match-part . helm-pp-bookmark-match-fn)
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-bookmark)
    (type . bookmark)))

(defun helm-bookmark-w3m-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-bmkext-filter-setup-alist 'bmkext-w3m-alist-only))


;;; Images
;;
(defvar helm-source-bookmark-images
  '((name . "Bookmark Images")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               'global (helm-bookmark-images-setup-alist))))
    (candidates-in-buffer)
    (search helm-bookmark-search-fn)
    (match-part . helm-pp-bookmark-match-fn)
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-bookmark)
    (type . bookmark)))

(defun helm-bookmark-images-setup-alist ()
  "Specialized filter function for images bookmarks."
  (helm-bmkext-filter-setup-alist 'bmkext-image-file-alist-only))


;;; Woman Man
;;
(defvar helm-source-bookmark-man
  '((name . "Bookmark Woman&Man")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               'global (helm-bookmark-man-setup-alist))))
    (candidates-in-buffer)
    (search helm-bookmark-search-fn)
    (match-part . helm-pp-bookmark-match-fn)
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-bookmark)
    (type . bookmark)))

(defun helm-bookmark-man-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (append (helm-bmkext-filter-setup-alist 'bmkext-man-alist-only)
          (helm-bmkext-filter-setup-alist 'bmkext-woman-alist-only)))


;;; Gnus
;;
(defvar helm-source-bookmark-gnus
  '((name . "Bookmark Gnus")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               'global (helm-bookmark-gnus-setup-alist))))
    (candidates-in-buffer)
    (search helm-bookmark-search-fn)
    (match-part . helm-pp-bookmark-match-fn)
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-bookmark)
    (type . bookmark)))

(defun helm-bookmark-gnus-setup-alist ()
  "Specialized filter function for bookmarks gnus."
  (helm-bmkext-filter-setup-alist 'bmkext-gnus-alist-only))


;;; Info
;;
(defvar helm-source-bookmark-info
  '((name . "Bookmark Info")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               'global (helm-bookmark-info-setup-alist))))
    (candidates-in-buffer)
    (search helm-bookmark-search-fn)
    (match-part . helm-pp-bookmark-match-fn)
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-bookmark)
    (type . bookmark)))

(defun helm-bookmark-info-setup-alist ()
  "Specialized filter function for bookmarks info."
  (helm-bmkext-filter-setup-alist 'bmkext-info-alist-only))


;;; Files and directories
;;
(defvar helm-source-bookmark-files&dirs
  '((name . "Bookmark Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)
              (helm-init-candidates-in-buffer
               'global (helm-bookmark-local-files-setup-alist))))
    (candidates-in-buffer)
    (search helm-bookmark-search-fn)
    (match-part . helm-pp-bookmark-match-fn)
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-bookmark)
    (type . bookmark)))

(defun helm-bookmark-local-files-setup-alist ()
  "Specialized filter function for bookmarks locals files."
  (helm-bmkext-filter-setup-alist 'bmkext-file-alist-only))


;;;###autoload
(defun helm-bookmark-ext ()
  "Preconfigured `helm' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `helm-source-google-suggest'."
  (interactive)
  (helm
   :sources
   '(helm-source-bookmark-files&dirs
     helm-source-bookmark-w3m
     helm-source-google-suggest
     helm-source-bmkext-addressbook
     helm-source-bookmark-gnus
     helm-source-bookmark-info
     helm-source-bookmark-man
     helm-source-bookmark-images
     helm-source-bookmark-set)
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
