;;; helm-color.el --- colors and faces -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2014 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Customize Face
;;
;;
(defun helm-custom-faces-init ()
  "Initialize buffer for `helm-source-customize-face'."
  (unless (helm-candidate-buffer)
    (save-selected-window
      (list-faces-display)
      (message nil))
    (helm-init-candidates-in-buffer
        'global
      (with-current-buffer (get-buffer "*Faces*")
        (buffer-substring
         (next-single-char-property-change (point-min) 'face)
         (point-max))))
    (kill-buffer "*Faces*")))

(defvar helm-source-customize-face
  '((name . "Customize Face")
    (init . helm-custom-faces-init)
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (("Customize"
                . (lambda (line)
                    (customize-face (intern (car (split-string line))))))
               ("Copy name"
                . (lambda (line)
                    (kill-new (car (split-string line " " t))))))))
  "See (info \"(emacs)Faces\")")

;;; Colors browser
;;
;;
(defun helm-colors-init ()
  (unless (helm-candidate-buffer)
    (save-selected-window
      (list-colors-display)
      (message nil))
    (helm-init-candidates-in-buffer
        'global
      (with-current-buffer (get-buffer "*Colors*")
        (buffer-string)))
    (kill-buffer "*Colors*")))

(defun helm-color-insert-name (candidate)
  (interactive)
  (with-helm-current-buffer
    (insert (helm-colors-get-name candidate))))

(defun helm-color-kill-name (candidate)
  (interactive)
  (kill-new (helm-colors-get-name candidate)))

(defun helm-color-insert-rgb (candidate)
  (interactive)
  (with-helm-current-buffer
    (insert (helm-colors-get-rgb candidate))))

(defun helm-color-kill-rgb (candidate)
  (interactive)
  (kill-new (helm-colors-get-rgb candidate)))

(defun helm-color-run-insert-name ()
  "Insert name of color from `helm-source-colors'"
  (interactive)
  (with-helm-alive-p (helm-quit-and-execute-action 'helm-color-insert-name)))

(defun helm-color-run-kill-name ()
  "Kill name of color from `helm-source-colors'"
  (interactive)
  (with-helm-alive-p (helm-quit-and-execute-action 'helm-color-kill-name)))

(defun helm-color-run-insert-rgb ()
  "Insert RGB of color from `helm-source-colors'"
  (interactive)
  (with-helm-alive-p (helm-quit-and-execute-action 'helm-color-insert-rgb)))

(defun helm-color-run-kill-rgb ()
  "Kill RGB of color from `helm-source-colors'"
  (interactive)
  (with-helm-alive-p (helm-quit-and-execute-action 'helm-color-kill-rgb)))

(defvar helm-color-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c n") 'helm-color-run-insert-name)
    (define-key map (kbd "C-c N") 'helm-color-run-kill-name)
    (define-key map (kbd "C-c r") 'helm-color-run-insert-rgb)
    (define-key map (kbd "C-c R") 'helm-color-run-kill-rgb)
    (define-key map (kbd "C-c ?") 'helm-color-help)
    map))

(defvar helm-source-colors
  `((name . "Colors")
    (init . helm-colors-init)
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (keymap . ,helm-color-map)
    (persistent-help . "Kill entry in RGB format.")
    (persistent-action . helm-color-kill-rgb)
    (mode-line . helm-color-mode-line-string)
    (action
     ("Copy Name (C-c N)" . helm-color-kill-name)
     ("Copy RGB (C-c R)" . helm-color-kill-rgb)
     ("Insert Name (C-c n)" . helm-color-insert-name)
     ("Insert RGB (C-c r)" . helm-color-insert-rgb))))

(defun helm-colors-get-name (candidate)
  "Get color name."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-min))
     (search-forward-regexp "\\s-\\{2,\\}")
     (delete-region (point) (point-max))
     (buffer-string))))

(defun helm-colors-get-rgb (candidate)
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
   '(helm-source-colors helm-source-customize-face)
   "*helm colors*"))

(provide 'helm-color)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-color.el ends here
