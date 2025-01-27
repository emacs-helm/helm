;;; helm-x-icons.el --- Provide compatibility between icons providers -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto

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

(require 'all-the-icons nil t)
(require 'nerd-icons nil t)

(declare-function nerd-icons-octicon "ext:nerd-icons")
(declare-function nerd-icons-faicon  "ext:nerd-icons")
(declare-function nerd-icons-mdicon  "ext:nerd-icons")
(declare-function nerd-icons-codicon "ext:nerd-icons")

(defgroup helm-x-icons nil
  "Compatibility functions between icons provider packages."
  :group 'helm)

(defcustom helm-x-icons-provider 'all-the-icons
  "The icons provider package.
The currently supported providers are `all-the-icons' and `nerd-icons'."
  :type '(choice
          (const :tag "Use `all-the-icons' package" all-the-icons)
          (const :tag "Use `nerd-icons' package" nerd-icons)))

(defun helm-x-icons-match-to-alist (file type)
  "Match FILE against an entry in ALIST using `string-match-p'.
Supported TYPE are ext, regexp, url and dir."
  (cl-loop with alist = (helm-x-icons-resolve-alist type)
           for (elm . rest) in alist
           for target = (if (eq type 'url) file (helm-basename file))
           when (string-match-p (helm-stringify elm) target)
           return rest))

(defun helm-x-icons-resolve-alist (type)
  "Return the icon alist corresponding to TYPE.
The returned alist is computed according to `helm-x-icons-provider'."
  (let* ((provider-name (symbol-name helm-x-icons-provider))
         (alist-name (helm-acase type
                      (ext "extension-icon-alist")
                      (regexp "regexp-icon-alist")
                      (dir "dir-icon-alist")
                      (url "url-alist"))))
    (symbol-value
     (intern-soft (concat provider-name "-" alist-name)))))

(defun helm-x-icons-icon-for-file (&rest args)
  "Compatibility function for `*-icon-for-file'."
  (let ((fn (helm-acase helm-x-icons-provider
              (all-the-icons 'all-the-icons-icon-for-file)
              (nerd-icons 'nerd-icons-icon-for-file))))
    (when fn (apply fn args))))

(defun helm-x-icons-octicon (icon-name &rest args)
  "Compatibility function for octicon.
May use other provider than octicon if ICON-NAME is not found in octicon."
  (let ((fn (helm-acase helm-x-icons-provider
              (all-the-icons #'all-the-icons-octicon)
              (nerd-icons #'nerd-icons-octicon))))
    (when (eq helm-x-icons-provider 'nerd-icons)
      (cond ((string= icon-name "file-symlink-directory")
             (setq fn #'nerd-icons-codicon
                   icon-name "nf-cod-file_symlink_directory"))
            ((string= icon-name "star")
             (setq fn #'nerd-icons-mdicon
                   icon-name "nf-md-star"))
            ((string= icon-name "mail-read")
             (setq fn #'nerd-icons-codicon
                   icon-name "nf-cod-mail_read"))
            ((string= icon-name "info")
             (setq fn #'nerd-icons-faicon
                   icon-name "nf-fa-info"))
            ((string= icon-name "link-external")
             (setq fn #'nerd-icons-faicon
                   icon-name "nf-fa-external_link"))
            ((string= icon-name "mail")
             (setq fn #'nerd-icons-mdicon
                   icon-name "nf-md-email"))))
      (when fn (apply fn icon-name args))))

(defun helm-x-icons-material (icon-name &rest args)
  "Compatibility function for material.
May use other provider than material if ICON-NAME is not found in material."
  (let ((fn (helm-acase helm-x-icons-provider
              (all-the-icons #'all-the-icons-material)
              (nerd-icons #'nerd-icons-mdicon))))
    (when (eq helm-x-icons-provider 'nerd-icons)
      (cond ((string= icon-name "note_add")
             (setq fn #'nerd-icons-codicon
                   icon-name "nf-cod-new_file"))
            ((string= icon-name "create_new_folder")
             (setq fn #'nerd-icons-codicon
                   icon-name "nf-cod-new_folder"))))
    (when fn (apply fn icon-name args))))

(defun helm-x-icons-faicon (icon-name &rest args)
  "Compatibility function for faicon.
May use other provider than faicon if ICON-NAME is not found in faicon."
  (let ((fn (helm-acase helm-x-icons-provider
              (all-the-icons #'all-the-icons-faicon)
              (nerd-icons #'nerd-icons-faicon))))
    (when (eq helm-x-icons-provider 'nerd-icons)
      (cond ((string= icon-name "firefox")
             (setq fn #'nerd-icons-faicon
                   icon-name "nf-fa-firefox"))
            ((string= icon-name "globe")
             (setq fn #'nerd-icons-faicon
                   icon-name "nf-fa-globe"))))
    (and fn (apply fn icon-name args))))

(defun helm-x-icons-wicon (icon-name &rest args)
  "Compatibility function for wicon.
May use other provider than wicon if ICON-NAME is not found in wicon."
  (let ((fn (helm-acase helm-x-icons-provider
              (all-the-icons #'all-the-icons-wicon)
              (nerd-icons #'nerd-icons-wicon))))
    (when fn (apply fn icon-name args))))

(defun helm-x-icons-fileicon (icon-name &rest args)
  "Compatibility function for fileicon.
May use other provider than fileicon if ICON-NAME is not found in fileicon."
  (let ((fn (helm-acase helm-x-icons-provider
              (all-the-icons #'all-the-icons-fileicon)
              (nerd-icons #'nerd-icons-sucicon))))
    (if (and (eq helm-x-icons-provider 'nerd-icons)
             (string= icon-name "man-page"))
        (apply #'nerd-icons-octicon "nf-oct-command_palette" args)
      (and fn (apply fn icon-name args)))))



(provide 'helm-x-icons)

;;; helm-x-icons.el ends here
