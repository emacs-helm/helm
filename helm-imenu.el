;;; helm-imenu.el --- Helm interface for Imenu -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'helm)
(require 'imenu)


(defgroup helm-imenu nil
  "Imenu related libraries and applications for helm."
  :group 'helm)

(defcustom helm-imenu-delimiter " / "
  "Delimit type of candidates and his value in `helm-buffer'."
  :group 'helm-imenu
  :type 'string)

(defcustom helm-imenu-execute-action-at-once-if-one t
  "Goto the candidate when only one is remaining."
  :group 'helm-imenu
  :type 'boolean)

;;; Internals
(defvar helm-cached-imenu-alist nil)
(make-variable-buffer-local 'helm-cached-imenu-alist)

(defvar helm-cached-imenu-candidates nil)
(make-variable-buffer-local 'helm-cached-imenu-candidates)

(defvar helm-cached-imenu-tick nil)
(make-variable-buffer-local 'helm-cached-imenu-tick)

(defvar helm-source-imenu
  '((name . "Imenu")
    (candidates . helm-imenu-candidates)
    (allow-dups)
    (candidate-transformer . helm-imenu-transformer)
    (persistent-action . (lambda (elm)
                           (imenu elm)
                           (unless (fboundp 'semantic-imenu-tag-overlay)
                             (helm-highlight-current-line))))
    (persistent-help . "Show this entry")
    (action . imenu))
  "See (info \"(emacs)Imenu\")")

(defun helm-imenu-candidates ()
  (with-helm-current-buffer
    (let ((tick (buffer-modified-tick)))
      (if (eq helm-cached-imenu-tick tick)
          helm-cached-imenu-candidates
          (setq imenu--index-alist nil)
          (setq helm-cached-imenu-tick tick
                helm-cached-imenu-candidates
                (let ((index (imenu--make-index-alist))) 
                  (helm-imenu--candidates-1
                   (delete (assoc "*Rescan*" index) index))))))))

(defun helm-imenu--candidates-1 (alist)
  (cl-loop for elm in alist
           append (if (consp (cdr elm))
                      (helm-imenu--candidates-1
                       (cl-loop for (e . v) in (cdr elm) collect
                                (cons (propertize
                                       e 'helm-imenu-type (car elm))
                                      v)))
                      (and (cdr elm) ; bug in imenu, should not be needed.
                           (list elm)))))

(defun helm-imenu-transformer (candidates)
  (cl-loop for (k . v) in candidates collect
           (cons (concat
                  (or (get-text-property 0 'helm-imenu-type k)
                      "Function") helm-imenu-delimiter k)
                 (cons k v))))

;;;###autoload
(defun helm-imenu ()
  "Preconfigured `helm' for `imenu'."
  (interactive)
  (let ((imenu-auto-rescan t)
        (helm-execute-action-at-once-if-one
         helm-imenu-execute-action-at-once-if-one)
        (imenu-default-goto-function
         (if (fboundp 'semantic-imenu-goto-function)
             'semantic-imenu-goto-function
             'imenu-default-goto-function)))
    (helm :sources 'helm-source-imenu
          :buffer "*helm imenu*")))

(provide 'helm-imenu)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-imenu.el ends here
