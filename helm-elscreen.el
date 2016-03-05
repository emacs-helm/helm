;;; helm-elscreen.el -- Elscreen support -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2016 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(declare-function elscreen-find-screen-by-buffer "ext:elscreen.el" (buffer &optional create))
(declare-function elscreen-find-file "ext:elscreen.el" (filename))
(declare-function elscreen-goto "ext:elscreen.el" (screen))
(declare-function elscreen-get-conf-list "ext:elscreen.el" (type))

(defun helm-find-buffer-on-elscreen (candidate)
  "Open buffer in new screen, if marked buffers open all in elscreens."
  (helm-require-or-error 'elscreen 'helm-find-buffer-on-elscreen)
  (helm-aif (helm-marked-candidates)
      (cl-dolist (i it)
        (let ((target-screen (elscreen-find-screen-by-buffer
                              (get-buffer i) 'create)))
          (elscreen-goto target-screen)))
    (let ((target-screen (elscreen-find-screen-by-buffer
                          (get-buffer candidate) 'create)))
      (elscreen-goto target-screen))))

(defun helm-elscreen-find-file (file)
  (helm-require-or-error 'elscreen 'helm-elscreen-find-file)
  (elscreen-find-file file))

(defclass helm-source-elscreen (helm-source-sync)
  ((candidates
    :initform
    (lambda ()
      (when (cdr (elscreen-get-screen-to-name-alist))
        (cl-sort (cl-loop for (screen . name) in (elscreen-get-screen-to-name-alist)
                       collect (cons (format "[%d] %s" screen name) screen))
                 #'< :key #'cdr))))
   (action :initform
           '(("Change Screen" .
              (lambda (candidate)
                (elscreen-goto candidate)))
             ("Kill Screen(s)" .
              (lambda (_)
                (cl-dolist (i (helm-marked-candidates))
                  (elscreen-goto i)
                  (elscreen-kill))))
             ("Only Screen" .
              (lambda (candidate)
                (elscreen-goto candidate)
                (elscreen-kill-others)))))
   (migemo :initform t)))

(defclass helm-source-elscreen-history (helm-source-elscreen)
  ((candidates
    :initform
    (lambda ()
      (let ((sname (elscreen-get-screen-to-name-alist)))
        (when (cdr sname)
          (cl-loop for screen in (cdr (elscreen-get-conf-list 'screen-history))
                collect (cons (format "[%d] %s" screen (cdr (assq screen sname)))
                              screen))))))))

(defvar helm-source-elscreen-list
  (helm-make-source "ElScreen" 'helm-source-elscreen))

(defvar helm-source-elscreen-history-list
  (helm-make-source "ElScreen History" 'helm-source-elscreen-history))

;;;###autoload
(defun helm-elscreen ()
  "Preconfigured helm to list elscreen."
  (interactive)
  (helm-other-buffer 'helm-source-elscreen-list "*Helm ElScreen*"))

;;;###autoload
(defun helm-elscreen-history ()
  "Preconfigured helm to list elscreen in history order."
  (interactive)
  (helm-other-buffer 'helm-source-elscreen-history-list "*Helm ElScreen*"))

(provide 'helm-elscreen)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-elscreen.el ends here
