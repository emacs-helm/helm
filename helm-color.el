;;; helm-color.el --- colors and faces

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

;;; Customize Face
;;
;;
(defun helm-custom-faces-init ()
  "Initialize buffer for `helm-c-source-customize-face'."
  (unless (helm-candidate-buffer)
    (save-selected-window
      (list-faces-display))
    (let ((cands (with-current-buffer (get-buffer "*Faces*")
                   (goto-char (point-min))
                   (forward-line 3)
                   (buffer-substring (point) (point-max)))))
      (helm-init-candidates-in-buffer "*helm faces*" cands))))

(defvar helm-c-source-customize-face
  '((name . "Customize Face")
    (init . helm-custom-faces-init)
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (lambda (line)
                (customize-face (intern (car (split-string line)))))))
  "See (info \"(emacs)Faces\")")

;;; Colors browser
;;
;;
(defvar helm-c-source-colors
  '((name . "Colors")
    (init . (lambda ()
              (unless (helm-candidate-buffer)
                (save-selected-window
                  (list-colors-display))
                (helm-candidate-buffer (get-buffer "*Colors*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action
     ("Copy Name" . (lambda (candidate)
                      (kill-new (helm-c-colors-get-name candidate))))
     ("Copy RGB" . (lambda (candidate)
                     (kill-new (helm-c-colors-get-rgb candidate))))
     ("Insert Name" . (lambda (candidate)
                        (with-helm-current-buffer
                          (insert (helm-c-colors-get-name candidate)))))
     ("Insert RGB" . (lambda (candidate)
                       (with-helm-current-buffer
                         (insert (helm-c-colors-get-rgb candidate))))))))

(defun helm-c-colors-get-name (candidate)
  "Get color name."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-min))
     (search-forward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-max))
     (buffer-string))))

(defun helm-c-colors-get-rgb (candidate)
  "Get color RGB."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-max))
     (search-backward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-min))
     (buffer-string))))

;;;###autoload
(defun helm-colors ()
  "Preconfigured `helm' for color."
  (interactive)
  (helm-other-buffer
   '(helm-c-source-colors helm-c-source-customize-face)
   "*helm colors*"))

(provide 'helm-color)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-color.el ends here
