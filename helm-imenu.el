;;; helm-imenu.el --- Helm interface for Imenu

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

(require 'cl)
(require 'helm)
(require 'imenu)

(defgroup helm-imenu nil
  "Imenu related libraries and applications for helm."
  :group 'helm)

(defcustom helm-imenu-delimiter " / "
  "Delimit type of candidates and his value in `helm-buffer'."
  :group 'helm-imenu
  :type 'string)

;;; Internals
(defvar helm-imenu-index-filter nil)
(make-variable-buffer-local 'helm-imenu-index-filter)

(defvar helm-cached-imenu-alist nil)
(make-variable-buffer-local 'helm-cached-imenu-alist)

(defvar helm-cached-imenu-candidates nil)
(make-variable-buffer-local 'helm-cached-imenu-candidates)

(defvar helm-cached-imenu-tick nil)
(make-variable-buffer-local 'helm-cached-imenu-tick)

(defun helm-imenu-create-candidates (entry)
  "Create candidates with ENTRY."
  (if (listp (cdr entry))
      (mapcan
       (lambda (sub)
         (if (consp (cdr sub))
             (mapcar
              (lambda (subentry)
                (concat (car entry) helm-imenu-delimiter subentry))
              (helm-imenu-create-candidates sub))
             (list (concat (car entry) helm-imenu-delimiter (car sub)))))
       (cdr entry))
      (list entry)))

(defvar helm-source-imenu
  '((name . "Imenu")
    (candidates . helm-imenu-candidates)
    (persistent-action . (lambda (elm)
                           (helm-imenu-default-action elm)
                           (unless (fboundp 'semantic-imenu-tag-overlay)
                             (helm-match-line-color-current-line))))
    (persistent-help . "Show this entry")
    (action . helm-imenu-default-action))
  "See (info \"(emacs)Imenu\")")

(defun helm-imenu-candidates ()
  (with-helm-current-buffer
    (let ((tick (buffer-modified-tick)))
      (if (eq helm-cached-imenu-tick tick)
          helm-cached-imenu-candidates
          (setq imenu--index-alist nil)
          (setq helm-cached-imenu-tick tick
                helm-cached-imenu-candidates
                (ignore-errors
                  (mapcan
                   'helm-imenu-create-candidates
                   (setq helm-cached-imenu-alist
                         (let ((index (imenu--make-index-alist)))
                           (if helm-imenu-index-filter
                               (funcall helm-imenu-index-filter index)
                               index))))))
          (setq helm-cached-imenu-candidates
                (mapcar #'(lambda (x)
                            (if (stringp x) x (car x)))
                        helm-cached-imenu-candidates))))))

(defun helm-imenu-default-action (elm)
  "The default action for `helm-source-imenu'."
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (let ((path (split-string elm helm-imenu-delimiter))
        (alist helm-cached-imenu-alist))
    (dolist (elm path)
      (setq alist (assoc elm alist)))
    (imenu alist)))

;;;###autoload
(defun helm-imenu ()
  "Preconfigured `helm' for `imenu'."
  (interactive)
  (let ((imenu-auto-rescan t)
        (helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate t)
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
