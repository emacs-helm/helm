;;; helm-font --- Font and ucs selection for Helm

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

(defvar helm-c-ucs-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-backspace>") 'helm-c-ucs-persistent-delete)
    (define-key map (kbd "<C-left>")      'helm-c-ucs-persistent-backward)
    (define-key map (kbd "<C-right>")     'helm-c-ucs-persistent-forward)
    (define-key map (kbd "<C-return>")    'helm-c-ucs-persistent-insert)
    (define-key map (kbd "C-c ?")         'helm-c-ucs-help)
    map)
  "Keymap for `helm-ucs'.")


;;; Xfont selection
;;
;;
(defun helm-c-persistent-xfont-action (elm)
  "Show current font temporarily"
  (let ((current-font (cdr (assoc 'font (frame-parameters))))
        (default-font elm))
    (unwind-protect
         (progn (set-frame-font default-font 'keep-size) (sit-for 2))
      (set-frame-font current-font))))

(defvar helm-c-xfonts-cache nil)
(defvar helm-c-source-xfonts
  '((name . "X Fonts")
    (init . (lambda ()
              (unless helm-c-xfonts-cache
                (setq helm-c-xfonts-cache
                      (x-list-fonts "*")))))
    (candidates . helm-c-xfonts-cache)
    (action . (("Copy to kill ring" . (lambda (elm)
                                        (kill-new elm)))
               ("Set Font" . (lambda (elm)
                               (kill-new elm)
                               (set-frame-font elm 'keep-size)
                               (message "New font have been copied to kill ring")))))
    (persistent-action . helm-c-persistent-xfont-action)
    (persistent-help . "Switch to this font temporarily")))

;;; 𝕌𝕔𝕤 𝕊𝕪𝕞𝕓𝕠𝕝 𝕔𝕠𝕞𝕡𝕝𝕖𝕥𝕚𝕠𝕟
;;
;;
(defvar helm-c-ucs-max-len 0)
(defun helm-c-calculate-ucs-max-len ()
  "Calculate the length of longest `ucs-names' candidate."
  (loop with count = 0
        for (n . v) in (ucs-names)
        for len = (length n)
        if (> len count)
        do (setq count len)
        finally return count))

(defun helm-c-ucs-init ()
  "Initialize an helm buffer with ucs symbols.
Only math* symbols are collected."
  (unless (> helm-c-ucs-max-len 0)
    (setq helm-c-ucs-max-len
          (helm-c-calculate-ucs-max-len)))
  (with-current-buffer (helm-candidate-buffer
                        (get-buffer-create "*helm ucs*"))
    ;; `ucs-names' fn will not run again, data is cached in
    ;; var `ucs-names'.
    (loop for (n . v) in (ucs-names)
          for len = (length n)
          for diff = (+ (- helm-c-ucs-max-len len) 2)
          unless (string= "" n)
          do (progn (insert (concat
                             n ":"
                             (make-string
                              diff ? )))
                    (ucs-insert v)
                    (insert "\n")))))

(defun helm-c-ucs-forward-char (candidate)
  (with-helm-current-buffer
    (forward-char 1)))

(defun helm-c-ucs-backward-char (candidate)
  (with-helm-current-buffer
    (forward-char -1)))

(defun helm-c-ucs-delete-backward (candidate)
  (with-helm-current-buffer
    (delete-char -1)))

(defun helm-c-ucs-insert-char (candidate)
  (with-helm-current-buffer
    (insert
     (replace-regexp-in-string
      " " ""
      (cadr (split-string candidate ":"))))))

(defun helm-c-ucs-persistent-insert ()
  (interactive)
  (helm-attrset 'action-insert 'helm-c-ucs-insert-char)
  (helm-execute-persistent-action 'action-insert))

(defun helm-c-ucs-persistent-forward ()
  (interactive)
  (helm-attrset 'action-forward 'helm-c-ucs-forward-char)
  (helm-execute-persistent-action 'action-forward))

(defun helm-c-ucs-persistent-backward ()
  (interactive)
  (helm-attrset 'action-back 'helm-c-ucs-backward-char)
  (helm-execute-persistent-action 'action-back))

(defun helm-c-ucs-persistent-delete ()
  (interactive)
  (helm-attrset 'action-delete 'helm-c-ucs-delete-backward)
  (helm-execute-persistent-action 'action-delete))

(defvar helm-c-source-ucs
  '((name . "Ucs names")
    (init . helm-c-ucs-init)
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (mode-line . helm-c-ucs-mode-line-string)
    (action . (("Insert" . helm-c-ucs-insert-char)
               ("Forward char" . helm-c-ucs-forward-char)
               ("Backward char" . helm-c-ucs-backward-char)
               ("Delete char backward" . helm-c-ucs-delete-backward))))
  "Source for collecting `ucs-names' math symbols.")

;;;###autoload
(defun helm-select-xfont ()
  "Preconfigured `helm' to select Xfont."
  (interactive)
  (helm-other-buffer 'helm-c-source-xfonts "*helm select* xfont"))

;;;###autoload
(defun helm-ucs ()
  "Preconfigured helm for `ucs-names' math symbols."
  (interactive)
  (helm :sources 'helm-c-source-ucs
        :keymap  helm-c-ucs-map))

(provide 'helm-font)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-font.el ends here
