;;; helm-shell.el --- Shell prompt navigation for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2019 Pierre Neidhardt <mail@ambrevar.xyz>

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
;;
;; Enable like this in .emacs:
;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;               (define-key shell-mode-map (kbd "M-s f") 'helm-shell-prompts-all)))

;;; Code:
(require 'cl-lib)
(require 'helm)
(require 'helm-lib)
(require 'helm-help)
(require 'helm-elisp)

;;; Shell prompts
;;
(defface helm-shell-prompts-promptidx
  '((t (:foreground "cyan")))
  "Face used to highlight Shell prompt index."
  :group 'helm-shell-faces)

(defface helm-shell-prompts-buffer-name
  '((t (:foreground "green")))
  "Face used to highlight Shell buffer name."
  :group 'helm-shell-faces)

(defcustom helm-shell-prompts-promptidx-p t
  "Show prompt number."
  :group 'helm-shell
  :type 'boolean)

(defvar helm-shell-prompts-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o")   'helm-shell-prompts-other-window)
    (define-key map (kbd "C-c C-o") 'helm-shell-prompts-other-frame)
    map)
  "Keymap for `helm-shell-prompt-all'.")

(defvar shell-prompt-pattern)

(defun helm-shell-prompts-list (&optional buffer)
  "List the prompts in Shell BUFFER.

Return a list of (\"prompt\" (point) (buffer-name) prompt-index))
e.g. (\"ls\" 162 \"*shell*\" 3).
If BUFFER is nil, use current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (when (eq major-mode 'shell-mode)
      (save-excursion
        (goto-char (point-min))
        (let (result (count 1))
          (save-mark-and-excursion
            (helm-awhile (and (not (eobp)) (comint-next-prompt 1))
              (push (list (buffer-substring-no-properties
                           it (point-at-eol))
                          it (buffer-name) count)
                    result)
              (setq count (1+ count))))
          (nreverse result))))))

(defun helm-shell-prompts-list-all ()
  "List the prompts of all Shell buffers.
See `helm-shell-prompts-list'."
  (cl-loop for b in (buffer-list)
           append (helm-shell-prompts-list b)))

(defun helm-shell-prompts-transformer (candidates &optional all)
  ;; ("ls" 162 "*shell*" 3) => ("*shell*:3:ls" . ("ls" 162 "*shell*" 3))
  (cl-loop for (prt pos buf id) in candidates
           collect `(,(concat
                       (when all
                         (concat (propertize
                                  buf
                                  'face 'helm-shell-prompts-buffer-name)
                                 ":"))
                       (when helm-shell-prompts-promptidx-p
                         (concat (propertize
                                  (number-to-string id)
                                  'face 'helm-shell-prompts-promptidx)
                                 ":"))
                       prt)
                      . ,(list prt pos buf id))))

(defun helm-shell-prompts-all-transformer (candidates)
  (helm-shell-prompts-transformer candidates t))

(cl-defun helm-shell-prompts-goto (candidate &optional (action 'switch-to-buffer))
  ;; Candidate format: ("ls" 162 "*shell*" 3)
  (let ((buf (nth 2 candidate)))
    (unless (and (string= (buffer-name) buf)
                 (eq action 'switch-to-buffer))
      (funcall action buf))
    (goto-char (nth 1 candidate))
    (recenter)))

(defun helm-shell-prompts-goto-other-window (candidate)
  (helm-shell-prompts-goto candidate 'switch-to-buffer-other-window))

(defun helm-shell-prompts-goto-other-frame (candidate)
  (helm-shell-prompts-goto candidate 'switch-to-buffer-other-frame))

(defun helm-shell-prompts-other-window ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-shell-prompts-goto-other-window)))
(put 'helm-shell-prompts-other-window 'helm-only t)

(defun helm-shell-prompts-other-frame ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-shell-prompts-goto-other-frame)))
(put 'helm-shell-prompts-other-frame 'helm-only t)

;;;###autoload
(defun helm-shell-prompts ()
  "Pre-configured `helm' to browse the prompts of the current Shell."
  (interactive)
  (if (eq major-mode 'shell-mode)
      (helm :sources
            (helm-build-sync-source "Shell prompts"
              :candidates (helm-shell-prompts-list)
              :candidate-transformer 'helm-shell-prompts-transformer
              :action '(("Go to prompt" . helm-shell-prompts-goto)))
            :buffer "*helm Shell prompts*")
    (message "Current buffer is not an Shell buffer")))

;;;###autoload
(defun helm-shell-prompts-all ()
  "Pre-configured `helm' to browse the prompts of all Shell sessions."
  (interactive)
  (helm :sources
        (helm-build-sync-source "All Shell prompts"
          :candidates (helm-shell-prompts-list-all)
          :candidate-transformer 'helm-shell-prompts-all-transformer
          :action '(("Go to prompt" . helm-shell-prompts-goto)
                    ("Go to prompt in other window `C-c o`" .
                     helm-shell-prompts-goto-other-window)
                    ("Go to prompt in other frame `C-c C-o`" .
                     helm-shell-prompts-goto-other-frame))
          :keymap helm-shell-prompts-keymap)
        :buffer "*helm Shell all prompts*"))

(provide 'helm-shell)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-shell ends here
