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
(require 'helm-utils)


(defgroup helm-sys nil
  "System related helm library."
  :group 'helm)

(defcustom helm-c-top-command "env COLUMNS=%s top -b -n 1"
  "Top command used to display output of top.
A format string where %s will be replaced with `frame-width'."
  :group 'helm-sys
  :type 'string)


;;; Top (process)
;;
;;
(defvar helm-c-source-top
  '((name . "Top")
    (header-name . (lambda (name) (concat name " (Press C-c C-u to refresh)"))) 
    (init . helm-c-top-init)
    (candidates-in-buffer)
    (display-to-real . helm-c-top-display-to-real)
    (persistent-action . helm-c-top-sh-persistent-action)
    (persistent-help . "SIGTERM")
    (filtered-candidate-transformer . helm-top-transformer)
    (action-transformer . helm-top-action-transformer)))

(defun helm-top-transformer (candidates source)
  "Transformer for `helm-top'.
Return empty string for non--valid candidates."
  (loop for disp in candidates collect
        (if (string-match "^ *[0-9]+" disp) disp (cons disp ""))))

(defun helm-top-action-transformer (actions candidate)
  "Action transformer for `top'.
Show actions only on line starting by a PID."
  (let ((disp (helm-get-selection nil t)))
    (cond ((string-match "^ *[0-9]+" disp)
           (list '("kill (SIGTERM)" . (lambda (pid) (helm-c-top-sh "TERM" pid)))
                 '("kill (SIGKILL)" . (lambda (pid) (helm-c-top-sh "KILL" pid)))
                 '("Copy PID" . (lambda (pid) (kill-new pid)))))
          (t actions))))

(defun helm-c-top-sh (sig pid)
  "Run kill shell command with signal SIG on PID for `helm-top'."
  (let ((cmd (format "kill -%s %s" sig pid)))
    (message "Executed %s\n%s" cmd (shell-command-to-string cmd))))

(defun helm-c-top-sh-persistent-action (pid)
  (delete-other-windows)
  (helm-c-top-sh "TERM" pid)
  (helm-force-update))

(defun helm-c-top-init ()
  "Insert output of top command in candidate buffer."
  (with-current-buffer (helm-candidate-buffer 'global)
    (call-process-shell-command
     (format helm-c-top-command (frame-width))
     nil (current-buffer))))

(defun helm-c-top-display-to-real (line)
  "Return pid only from LINE."
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
    (init . (lambda () (list-processes--refresh)))
    (candidates . (lambda () (mapcar #'process-name (process-list))))
    (persistent-action . (lambda (elm)
                           (delete-process (get-process elm))
                           (helm-delete-current-selection)))
    (update . list-processes--refresh)
    (persistent-help . "Kill Process")
    (action ("Kill Process" . (lambda (elm)
                                (delete-process (get-process elm)))))))


;;;###autoload
(defun helm-top ()
  "Preconfigured `helm' for top command."
  (interactive)
  (save-window-excursion
    (unless helm-alive-p (delete-other-windows))
    (helm :sources 'helm-c-source-top
          :buffer "*helm top*" :full-frame t
          :candidate-number-limit 9999)))

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
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-sys.el ends here
