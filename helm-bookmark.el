;;; helm-bookmark.el --- Helm for Emacs regular Bookmarks.

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
(eval-when-compile (require 'bookmark))
(require 'helm)
(require 'helm-utils)
(require 'helm-info)
(require 'helm-adaptative)


(defgroup helm-bookmark nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defcustom helm-bookmark-show-location nil
  "Show location of bookmark on display."
  :group 'helm-bookmark
  :type 'boolean)


(defface helm-bookmark-info
    '((t (:foreground "green")))
  "Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'helm-bookmark)

(defface helm-bookmark-w3m
    '((t (:foreground "yellow")))
  "Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'helm-bookmark)

(defface helm-bookmark-gnus
    '((t (:foreground "magenta")))
  "Face used for Gnus bookmarks."
  :group 'helm-bookmark)

(defface helm-bookmark-man
    '((t (:foreground "Orange4")))
  "Face used for Woman/man bookmarks."
  :group 'helm-bookmark)

(defface helm-bookmark-file
    '((t (:foreground "Deepskyblue2")))
  "Face used for file bookmarks."
  :group 'helm-bookmark)

(defface helm-bookmark-directory
    '((t (:inherit helm-ff-directory)))
  "Face used for file bookmarks."
  :group 'helm-bookmark)

(defface helm-bookmark-addressbook
    '((t (:foreground "tomato")))
  "Face used for addressbook bookmarks."
  :group 'helm-bookmark)


(defvar helm-bookmark-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-bookmark-run-jump-other-window)
    (define-key map (kbd "C-d")   'helm-bookmark-run-delete)
    (define-key map (kbd "M-t")   'helm-bookmark-toggle-filename)
    (when (locate-library "bookmark-extensions")
      (define-key map (kbd "M-e") 'helm-bmkext-run-edit))
    (define-key map (kbd "C-c ?") 'helm-bookmark-help)
    (delq nil map))
  "Generic Keymap for emacs bookmark sources.")

(defvar helm-bookmarks-cache nil)
(defvar helm-source-bookmarks
  `((name . "Bookmarks")
    (init . (lambda ()
              (require 'bookmark)
              (setq helm-bookmarks-cache
                    (bookmark-all-names))))
    (no-delay-on-input) ; Issue #173 needed for helm-for-files.
    (candidates . helm-bookmarks-cache)
    (filtered-candidate-transformer . helm-bookmark-transformer)
    (match . helm-bookmark-match-fn)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defun helm-bookmark-transformer (candidates source)
  (loop for i in candidates
        for loc = (bookmark-location i)
        for len =  (string-width i)
        for trunc = (if (> len bookmark-bmenu-file-column)
                        (helm-substring i bookmark-bmenu-file-column)
                        i)
        for sep = (make-string (- (+ bookmark-bmenu-file-column 2)
                                  (length trunc)) ? )
        if helm-bookmark-show-location
        collect (cons (concat trunc sep loc) i)
        else collect i))

(defun helm-bookmark-match-fn (candidate)
  "Match function for bookmark sources using `candidates'."
  (if helm-bookmark-show-location
      ;; match only location, match-plugin will match also name.
      (string-match helm-pattern (bookmark-location candidate))
      (string-match helm-pattern candidate)))

;;;###autoload
(defun helm-bookmark-toggle-filename ()
  "Toggle bookmark location visibility."
  (interactive)
  (let ((real (helm-get-selection helm-buffer)))
    (setq helm-bookmark-show-location (not helm-bookmark-show-location))
    (helm-update (if helm-bookmark-show-location
                     (bookmark-location real) real))))

(defun helm-bookmark-jump (candidate)
  "Jump to bookmark from keyboard."
  (let ((current-prefix-arg helm-current-prefix-arg))
    (bookmark-jump candidate)))


;;; bookmark-set
;;
(defvar helm-source-bookmark-set
  '((name . "Set Bookmark")
    (dummy)
    (no-delay-on-input)
    (action . bookmark-set))
  "See (info \"(emacs)Bookmarks\").")


;;; Colorize bookmarks by category
;;
(defvar helm-source-pp-bookmarks
  '((name . "PP-Bookmarks")
    (init . (lambda ()
              (require 'bookmark)
              (helm-init-candidates-in-buffer
               'global (loop for b in (bookmark-all-names) collect
                             (propertize b 'location (bookmark-location b))))))
    (candidates-in-buffer)
    (search helm-bookmark-search-fn)
    (match-part . helm-pp-bookmark-match-fn)
    (filtered-candidate-transformer
     helm-adaptive-sort
     helm-highlight-bookmark)
    (no-delay-on-input)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")

(defun helm-bookmark-search-fn (pattern)
  "Search function for bookmark sources using `candidates-in-buffer'.
Should be used with `helm-pp-bookmark-match-fn' as `match-part' function."
  (if helm-bookmark-show-location
      (helm-aif (next-single-property-change (point) 'location)
          (goto-char it))
      (re-search-forward pattern nil t)))

(defun helm-pp-bookmark-match-fn (candidate)
  "Search function for bookmark sources using `candidates-in-buffer'.
Should be used with `helm-bookmark-search-fn' as `search' function."
  (helm-aif (and helm-bookmark-show-location
                 (bookmark-location candidate))
      ;; Match against bookmark-name and location.
      (concat candidate " " it)
    ;; Match against bookmark-name.
    candidate))

(defun helm-highlight-bookmark (bookmarks source)
  "Used as `filtered-candidate-transformer' to colorize bookmarks.
Work both with standard Emacs bookmarks and bookmark-extensions.el."
  (let ((non-essential t))
    (loop for i in bookmarks
          for isfile        = (bookmark-get-filename i)
          for bufp          = (and (fboundp 'bmkext-get-buffer-name)
                                   (bmkext-get-buffer-name i))
          for handlerp      = (and (fboundp 'bookmark-get-handler)
                                   (bookmark-get-handler i))
          for isw3m         = (and (fboundp 'bmkext-w3m-bookmark-p)
                                   (bmkext-w3m-bookmark-p i))
          for isgnus        = (and (fboundp 'bmkext-gnus-bookmark-p)
                                   (bmkext-gnus-bookmark-p i))
          for isman         = (and (fboundp 'bmkext-man-bookmark-p) ; Man
                                   (bmkext-man-bookmark-p i))
          for iswoman       = (and (fboundp 'bmkext-woman-bookmark-p) ; Woman
                                   (bmkext-woman-bookmark-p i))
          for handlerp      = (bookmark-get-handler i)
          for isannotation  = (bookmark-get-annotation i)
          for isabook       = (string= (bookmark-prop-get i 'type)
                                       "addressbook")
          for isinfo        = (eq handlerp 'Info-bookmark-jump)
          for loc = (bookmark-location i)
          for len =  (string-width i)
          for trunc = (if (and helm-bookmark-show-location
                               (> len bookmark-bmenu-file-column))
                          (helm-substring
                           i bookmark-bmenu-file-column)
                          i)
          ;; Add a * if bookmark have annotation
          if (and isannotation (not (string-equal isannotation "")))
          do (setq trunc (concat "*" (if helm-bookmark-show-location trunc i)))
          for sep = (and helm-bookmark-show-location
                         (make-string (- (+ bookmark-bmenu-file-column 2)
                                         (string-width trunc)) ? ))
          for bmk = (cond ( ;; info buffers
                           isinfo
                           (propertize trunc 'face 'helm-bookmark-info
                                       'help-echo isfile))
                          ( ;; w3m buffers
                           isw3m
                           (propertize trunc 'face 'helm-bookmark-w3m
                                       'help-echo isfile))
                          ( ;; gnus buffers
                           isgnus
                           (propertize trunc 'face 'helm-bookmark-gnus
                                       'help-echo isfile))
                          ( ;; Man Woman
                           (or iswoman isman)
                           (propertize trunc 'face 'helm-bookmark-man
                                       'help-echo isfile))
                          ( ;; Addressbook
                           isabook
                           (propertize trunc 'face 'helm-bookmark-addressbook))
                          ( ;; directories
                           (and isfile
                                ;; This is needed because `non-essential'
                                ;; is not working on Emacs-24.2 and the behavior
                                ;; of tramp seems to have changed since previous
                                ;; versions (Need to reenter password even if a
                                ;; first connection have been established,
                                ;; probably when host is named differently
                                ;; i.e machine/localhost)
                                (not (file-remote-p isfile))
                                (file-directory-p isfile))
                           (propertize trunc 'face 'helm-bookmark-directory
                                       'help-echo isfile))
                          ( ;; regular files
                           t
                           (propertize trunc 'face 'helm-bookmark-file
                                       'help-echo isfile)))
          collect (if helm-bookmark-show-location
                      (cons (concat bmk sep loc) i)
                      (cons bmk i)))))


;;; Bookmarks attributes
;;
(define-helm-type-attribute 'bookmark
    `((coerce . helm-bookmark-get-bookmark-from-name)
      (action
       ("Jump to bookmark" . helm-bookmark-jump)
       ("Jump to BM other window" . bookmark-jump-other-window)
       ("Bookmark edit annotation" . bookmark-edit-annotation)
       ("Bookmark show annotation" . bookmark-show-annotation)
       ("Delete bookmark(s)" . helm-delete-marked-bookmarks)
       ,@(and (locate-library "bookmark-extensions")
              `(("Edit Bookmark" . bmkext-edit-bookmark)))
       ("Rename bookmark" . bookmark-rename)
       ("Relocate bookmark" . bookmark-relocate))
      (keymap . ,helm-bookmark-map)
      (mode-line . helm-bookmark-mode-line-string))
  "Bookmark name.")


;;;###autoload
(defun helm-bookmark-run-jump-other-window ()
  "Jump to bookmark from keyboard."
  (interactive)
  (helm-quit-and-execute-action 'bookmark-jump-other-window))

;;;###autoload
(defun helm-bookmark-run-delete ()
  "Delete bookmark from keyboard."
  (interactive)
  (when (y-or-n-p "Delete bookmark?")
    (helm-quit-and-execute-action 'helm-delete-marked-bookmarks)))

(defun helm-bookmark-get-bookmark-from-name (bmk)
  "Return bookmark name even if it is a bookmark with annotation.
e.g prepended with *.
Return nil if bmk is not a valid bookmark."
  (let ((bookmark (replace-regexp-in-string "\*" "" bmk)))
    (if (assoc bookmark bookmark-alist)
        bookmark
        (when (assoc bmk bookmark-alist)
          bmk))))

(defun helm-delete-marked-bookmarks (ignore)
  "Delete this bookmark or all marked bookmarks."
  (dolist (i (helm-marked-candidates))
    (bookmark-delete (helm-bookmark-get-bookmark-from-name i)
                     'batch)))


;;;###autoload
(defun helm-bookmarks ()
  "Preconfigured `helm' for bookmarks."
  (interactive)
  (helm :sources '(helm-source-bookmarks
                   helm-source-bookmark-set)
        :buffer "*helm bookmarks*"
        :default (buffer-name helm-current-buffer)))

;;;###autoload
(defun helm-pp-bookmarks ()
  "Preconfigured `helm' for bookmarks (pretty-printed)."
  (interactive)
  (helm :sources '(helm-source-pp-bookmarks
                   helm-source-bookmark-set)
        :buffer "*helm pp bookmarks*"
        :default (buffer-name helm-current-buffer)))


(provide 'helm-bookmark)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-bookmark.el ends here
