;;; helm-packages.el --- helm interface to manage packages  -*- lexical-binding: t; -*- 

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

(require 'cl-lib)
(require 'helm)
(require 'package)

;;; Actions
;;
;;
(defun helm-packages-upgrade (_candidate)
  "Helm action for upgrading marked packages."
  (let ((mkd (helm-marked-candidates)))
    (mapc #'package-upgrade mkd)))

(defun helm-packages-describe (candidate)
  "Helm action for describing package CANDIDATE."
  (describe-package candidate))

(defun helm-packages-visit-homepage (candidate)
  "Helm action for visiting package CANDIDATE home page."
  (let* ((id (package-get-descriptor candidate))
         (name (package-desc-name id))
         (extras (package-desc-extras id))
         (url (and (listp extras) (cdr-safe (assoc :url extras)))))
    (if (stringp url)
        (browse-url url)
      (message "Package %s has no homepage"
               (propertize (symbol-name name)
                           'face 'font-lock-keyword-face)))))

(defun helm-packages-package-reinstall (_candidate)
  "Helm action for reinstalling marked packages."
  (let ((mkd (helm-marked-candidates)))
    (mapc #'package-reinstall mkd)))

(defun helm-packages-delete-1 (packages &optional force)
  "Run `package-delete' on PACKAGES.
If FORCE is non nil force deleting packages."
  (mapc (lambda (x)
          (package-delete (package-get-descriptor x) force))
        packages))

(defun helm-packages-uninstall (_candidate)
  "Helm action for uninstalling marked packages.
Unlike `helm-packages-delete' this will refuse to delete packages when they are
needed by others packages as dependencies."
  (let ((mkd (helm-marked-candidates)))
    (helm-packages-delete-1 mkd)))

(defun helm-packages-delete (_candidate)
  "Helm action for deleting marked packages.
Unlike `helm-packages-uninstall' this delete packages even when they are needed
as dependencies."
  (let ((mkd (helm-marked-candidates)))
    (helm-packages-delete-1 mkd 'force)))

(defun helm-packages-recompile (_candidate)
  "Helm action for recompiling marked packages."
  (let ((mkd (helm-marked-candidates)))
    (mapc #'package-recompile mkd)))

(defun helm-packages-install (_candidate)
  "Helm action for installing marked packages."
  (let ((mkd (helm-marked-candidates)))
    (mapc #'package-install mkd)))

;;; Transformer
;;
;;
(defun helm-packages-transformer (candidates _source)
  "Transformer function for `helm-packages'."
  (cl-loop for c in candidates
           for sym = (intern-soft c)
           for archive = (assq sym package-archive-contents)
           for id = (package-get-descriptor sym)
           for provider = (and archive (package-desc-archive (cadr archive)))
           for status = (and id (package-desc-status id))
           for version = (and id (mapconcat #'number-to-string (package-desc-version id) "."))
           for description = (and id (package-desc-summary id))
           for disp = (format "%s%s%s%s%s%s%s%s%s"
                              (propertize c 'face 'font-lock-keyword-face 'match-part c)
                              (make-string (1+ (- (helm-in-buffer-get-longest-candidate)
                                                  (length c)))
                                           ? )
                              (or status "")
                              (make-string (1+ (- 10 (length status))) ? )
                              (or provider "")
                              (make-string (1+ (- 10 (length provider))) ? )
                              (or version "")
                              (make-string (1+ (- 20 (length version))) ? )
                              (if description
                                  (propertize description 'face 'font-lock-warning-face)
                                ""))
           collect (cons disp c)))

;;;###autoload
(defun helm-packages (&optional arg)
  "Helm interface to list packages."
  (interactive "P")
  (package-initialize)
  (when arg
    (package-refresh-contents))
  (let ((upgrades (package--upgradeable-packages))
        (removables (package--removable-packages)))
    (helm :sources (list
                    (helm-build-sync-source "Availables for upgrade"
                      :candidates upgrades
                      :filtered-candidate-transformer
                      (lambda (candidates _source)
                        (cl-loop for c in candidates
                                 collect (cons (propertize
                                                (symbol-name c)
                                                'face 'font-lock-keyword-face)
                                               c)))
                      :coerce #'helm-symbolify
                      :action '(("Upgrade package(s)" . helm-packages-upgrade)))
                    (helm-build-sync-source "Packages to delete"
                      :candidates removables
                      :coerce #'helm-symbolify
                      :filtered-candidate-transformer
                      (lambda (candidates _source)
                        (cl-loop for c in candidates
                                 collect (cons (propertize
                                                (symbol-name c)
                                                'face 'font-lock-keyword-face)
                                               c)))
                      :action '(("Delete package(s)" . helm-packages-delete)))
                    (helm-build-in-buffer-source "Installed packages"
                      :data (mapcar #'car package-alist)
                      :coerce #'helm-symbolify
                      :filtered-candidate-transformer
                      '(helm-packages-transformer
                        (lambda (candidates _source)
                          (sort candidates #'helm-generic-sort-fn)))
                      :action '(("Describe package" . helm-packages-describe)
                                ("Visit homepage" . helm-packages-visit-homepage)
                                ("Reinstall package(s)" . helm-packages-package-reinstall)
                                ("Recompile package(s)" . helm-packages-recompile)
                                ("Uninstall package(s)" . helm-packages-uninstall)))
                    (helm-build-in-buffer-source "Other packages"
                      :data (cl-loop for p in package-archive-contents
                                     for sym = (car p)
                                     for id = (package-get-descriptor sym)
                                     for status = (package-desc-status id)
                                     unless (or (and id (member
                                                         status
                                                         '("installed" "dependency")))
                                                (and id (assoc sym package--builtins)))
                                     nconc (list (car p)))
                      :coerce #'helm-symbolify
                      :filtered-candidate-transformer
                      '(helm-packages-transformer
                        (lambda (candidates _source)
                          (sort candidates #'helm-generic-sort-fn)))
                      :action '(("Describe package" . helm-packages-describe)
                                ("Visit homepage" . helm-packages-visit-homepage)
                                ("Install packages(s)" . helm-packages-install))))
          :buffer "*helm test*")))

(provide 'helm-packages)

;;; helm-packages ends here
