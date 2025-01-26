;;; helm-icons.el --- Icon support for Helm. -*- lexical-binding: t -*-

;; Copyright (C) 2025 - Björn Bidar

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

;;; Commentary:

;; Provider independent icon support for Helm.
;; Requires that any of the font providers are installed.
;; If disabled no icons are used.

;; In specific instances providers specific fonts are used,
;; e.g. in case one provider doesn't contain a exact or better
;; match for the specific icon requested.
;; See `helm-icons-action-icon' for details.

;;; Code:

(defgroup helm-icons nil
  "Icon support for Helm."
  :group 'helm)

(defcustom helm-icons-provider 'nerd-icons
  "Icon source for Helm."
  :type '(choice (const all-the-icons)
                 (const nerd-icons)
                 (const treemacs) ;;FIXME
                 (const :tag "Disabled" nil)))

(defcustom helm-icons-mode-icons-alist
  '((dired-mode . "dir-closed")
    (emacs-lisp-mode . "el")
    (spacemacs-buffer-mode . "el"))
  "Alist describing which mode should use which icon.

If MODE isn't contained in list fallback to icon provider."
  :type '(alist :key-type symbol :value-type sexp))

(defconst helm-icons--provider-transform-alist
  '((nerd-icons . ("-" "_" "nf-%s-%s")))
  "Alist for transform icons if applicable.
Each element is cons cell of the font PROVIDER,
FROM-STRING, TO-STRING and a pattern to insert the final icon name.")
(defconst helm-icons--provider-font-prefix-alist
  '((nerd-icons . ((octicon . "oct")
                   (faicon . "fa")
                   (wicon . "weather")
                   (mdicon . "md")
                   (flicon . "linux")
                   (pomicon . "pomi")
                   (codicon . "cod"))))
  "Alist of providers, fonts and their font-prefixes.")

(defconst helm-icons--provider-family-alist
  '((all-the-icons . ((mdicon . markdown)))))

(defconst helm-icons--action-map-alist
  '((nerd-icons . ((new-file . (codicon "new-file"))
                   (new-folder . (codicon "new-folder"))))
    (all-the-icons . ((new-file . (material "note_add"))
                      (new-folder . (material "create_new_folder")))))
  "Provider and alist of support actions.
Each action is a alist where the car is the action-name
while the cdr is a list of font-family and icon name.")

(defun helm-icons-file-icon (file &rest arg-overrides)
  "Return icon for FILE from `helm-icons-provider'.
ARG-OVERRIDES is forwarded as is."
  (apply (helm-icons--provider-function
            helm-icons-provider
            'icon-for-file) file arg-overrides))

(defun helm-icons-directory-icon (directory &rest arg-overrides)
  "Return icon for DIRECTORY from `helm-icons-provider'.
ARG-OVERRIDES is forwarded as is."
  (apply (helm-icons--provider-function
            helm-icons-provider
            'icon-for-dir) directory arg-overrides))

(defun helm-icons-mode-icon (mode &rest arg-overrides)
  "Return for MODE from `helm-icons-provider'.
ARG-OVERRIDES is forwarded as is."
  (if-let* ((mode-icon (assoc mode helm-icons-mode-icons-alist)))
      (helm-icons-file-icon (cdr mode-icon))
    (apply (helm-icons--provider-function
              helm-icons-provider
              'icon-for-mode) mode arg-overrides)))

(defun helm-icons-buffer-icon (&optional buffer)
  "Return icon for BUFFER or current buffer if nil.
ARG-OVERRIDES is forwarded as is."
  (with-current-buffer (or buffer
                           (current-buffer))
    (apply (helm-icons--provider-function
              helm-icons-provider
              'icon-for-buffer))))

(defun helm-icons-octicon-icon (icon &rest arg-overrides)
  "Return ICON in `helm-icons-provider' from octicons.
ARG-OVERRIDES is forwarded as is."
  (helm-icons--font-family-function helm-icons-provider
                                    'octicon
                                    icon arg-overrides))

(defun helm-icons-faicon-icon (icon &rest arg-overrides)
  "Return ICON in `helm-icons-provider' from faicons.
ARG-OVERRIDES is forwarded as is."
  (helm-icons--font-family-function helm-icons-provider
                                    'faicon
                                    icon arg-overrides))

(defun helm-icons-wicon-icon (icon &rest arg-overrides)
  "Return ICON in `helm-icons-provider' from wicon.
ARG-OVERRIDES is forwarded as is."
  (helm-icons--font-family-function helm-icons-provider
                                    'wicon
                                    icon arg-overrides))


(defun helm-icons-mdicon-icon (icon &rest arg-overrides)
  "Return ICON in `helm-icons-provider' from mdicon.
ARG-OVERRIDES is forwarded as is."
  (helm-icons--font-family-function helm-icons-provider
                                    'mdicon
                                    icon arg-overrides))

(defun helm-icons-action-icon (action &rest arg-overrides)
  "Return icon for ACTION from `helm-icons-provider'.
ARG-OVERRIDES is forwarded as is."
  (when-let* ((actions (alist-get helm-icons-provider
                                  helm-icons--action-map-alist))
              (action-set (alist-get action
                                     actions)))
    (helm-icons--font-family-function
     helm-icons-provider
     (car action-set)
     (cadr action-set) arg-overrides)))

(defun helm-icons--font-family-function (provider
                                         family icon
                                         &rest arg-overrides)
  "Return ICON for PROVIDER from FAMILY.
ARG-OVERRIDES is forwarded as is."
  (let ((icon-name icon))
    (when-let* ((transform-regex
                 (alist-get provider helm-icons--provider-transform-alist))
                (font (alist-get family
                                 (alist-get
                                  provider
                                  helm-icons--provider-font-prefix-alist))))
      (setq icon-name (format (caddr transform-regex) font
                              (replace-regexp-in-string
                               (car transform-regex)
                               (cadr transform-regex)
                               icon))))
    (when-let* ((mapped-fonts (alist-get provider
                                         helm-icons--provider-family-alist))
                (mapped-font (alist-get family
                                        mapped-fonts)))
      (setq family mapped-font))
    (apply (helm-icons--provider-function provider family)
             icon-name arg-overrides)))

(defun helm-icons--provider-function (provider function)
  "Return a function symbol based on PROVIDER and FUNCTION."
  ;; FIXME: Return result that makes the rest of the code
  ;; to silently ignore that icons are disabled.
  (when provider
   (let ((func (intern (format "%s-%s" provider function))))
    (unless (fboundp func)
        (if (not (require provider nil t))
            (error "Trying to require a provider that doesn't exist"))
      (unless (fboundp func)
        (error "No such function: %s" func)))
    func)))

(provide 'helm-icons)

;;; helm-icons.el ends here
