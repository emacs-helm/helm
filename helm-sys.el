;;; helm-sys.el --- System related functions for helm.

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


;;; Top (process)
(defvar helm-c-top-command "COLUMNS=%s top -b -n 1"
  "Top command (batch mode). %s is replaced with `frame-width'.")
(defvar helm-c-source-top
  '((name . "Top (Press C-c C-u to refresh)")
    (init . helm-c-top-init)
    (candidates-in-buffer)
    (display-to-real . helm-c-top-display-to-real)
    (persistent-action . helm-c-top-sh-persistent-action)
    (persistent-help . "SIGTERM")
    (action
     ("kill (TERM)" . (lambda (pid)
                        (helm-c-top-sh (format "kill -TERM %s" pid))))
     ("kill (KILL)" . (lambda (pid)
                        (helm-c-top-sh (format "kill -KILL %s" pid))))
     ("Copy PID" . (lambda (pid) (kill-new pid))))))

(defun helm-c-top-sh (cmd)
  (message "Executed %s\n%s" cmd (shell-command-to-string cmd)))

(defun helm-c-top-sh-persistent-action (pid)
  (delete-other-windows)
  (helm-c-top-sh (format "kill -TERM %s" pid))
  (helm-force-update))

(defun helm-c-top-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (call-process-shell-command
     (format helm-c-top-command (frame-width))
     nil (current-buffer))))

(defun helm-c-top-display-to-real (line)
  (car (split-string line)))

;;; X RandR resolution change
;;
;;
;;; FIXME I do not care multi-display.

(defun helm-c-xrandr-info ()
  "Return a pair with current X screen number and current X display name."
  (with-temp-buffer
    (call-process "xrandr" nil (current-buffer) nil
                  "--current")
    (let (screen output)
      (goto-char (point-min))
      (save-excursion
        (when (re-search-forward "\\(^Screen \\)\\([0-9]\\):" nil t)
          (setq screen (match-string 2))))
      (when (re-search-forward "^\\(.*\\) connected" nil t)
        (setq output (match-string 1)))
      (list screen output))))

(defun helm-c-xrandr-screen ()
  "Return current X screen number."
  (car (helm-c-xrandr-info)))

(defun helm-c-xrandr-output ()
  "Return current X display name."
  (cadr (helm-c-xrandr-info)))

(defvar helm-c-source-xrandr-change-resolution
  '((name . "Change Resolution")
    (candidates
     . (lambda ()
         (with-temp-buffer
           (call-process "xrandr" nil (current-buffer) nil
                         "--screen" (helm-c-xrandr-screen) "-q")
           (goto-char 1)
           (loop with modes = nil
                 while (re-search-forward "   \\([0-9]+x[0-9]+\\)" nil t)
                 for mode = (match-string 1)
                 unless (member mode modes)
                 collect mode into modes
                 finally return modes))))
    (action
     ("Change Resolution"
      . (lambda (mode)
          (call-process "xrandr" nil nil nil
                        "--screen" (helm-c-xrandr-screen)
                        "--output" (helm-c-xrandr-output)
                        "--mode" mode))))))

;;; Emacs process
;;
;;
(defvar helm-c-source-emacs-process
  '((name . "Emacs Process")
    (candidates . (lambda () (mapcar #'process-name (process-list))))
    (persistent-action . (lambda (elm)
                           (delete-process (get-process elm))
                           (helm-delete-current-selection)))
    (persistent-help . "Kill Process")
    (action ("Kill Process" . (lambda (elm)
                                (delete-process (get-process elm)))))))

;;;###autoload
(defun helm-top ()
  "Preconfigured `helm' for top command."
  (interactive)
  (let ((helm-samewindow t)
        (helm-display-function 'helm-default-display-buffer)
        (helm-candidate-number-limit 9999))
    (save-window-excursion
      (delete-other-windows)
      (helm-other-buffer 'helm-c-source-top "*helm top*"))))

;;;###autoload
(defun helm-list-emacs-process ()
  "Preconfigured `helm' for emacs process."
  (interactive)
  (helm-other-buffer 'helm-c-source-emacs-process "*helm process*"))

;;;###autoload
(defun helm-xrandr-set ()
  (interactive)
  (helm :sources 'helm-c-source-xrandr-change-resolution
        :buffer "*helm xrandr*"))

(provide 'helm-sys)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-sys.el ends here
