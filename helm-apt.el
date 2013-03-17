;;; helm-apt.el --- Helm interface for Debian/Ubuntu packages (apt-*)

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

(eval-when-compile (require 'cl))
(require 'helm)
(require 'helm-utils)
(require 'helm-external)

(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")
(declare-function term-send-eof "term")


(defgroup helm-apt nil
  "Apt related Applications and libraries for Helm."
  :group 'helm)

(defface helm-apt-installed
    '((t (:foreground "green")))
  "Face used for apt installed candidates."
  :group 'helm-apt)

(defface helm-apt-deinstalled
    '((t (:foreground "DimGray")))
  "Face used for apt deinstalled candidates."
  :group 'helm-apt)


(defvar helm-source-apt
  '((name . "APT")
    (init . helm-apt-init)
    (candidates-in-buffer)
    (candidate-transformer helm-apt-candidate-transformer)
    (display-to-real . helm-apt-display-to-real)
    (requires-pattern . 2)
    (update . helm-apt-refresh)
    (action
     ("Show package description" . helm-apt-cache-show)
     ("Install package" . helm-apt-install)
     ("Reinstall package" . helm-apt-reinstall)
     ("Remove package" . helm-apt-uninstall)
     ("Purge package" . helm-apt-purge))
    (persistent-action . helm-apt-persistent-action)
    (persistent-help . "Show package description")))

(defvar helm-apt-query "emacs")
(defvar helm-apt-search-command "apt-cache search '%s'")
(defvar helm-apt-show-command "apt-cache show '%s'")
(defvar helm-apt-installed-packages nil)
(defvar helm-apt-all-packages nil)
(defvar helm-apt-input-history nil)

(defun helm-apt-refresh ()
  "Refresh installed candidates list."
  (setq helm-apt-installed-packages nil)
  (setq helm-apt-all-packages nil))

(defun helm-apt-persistent-action (candidate)
  "Persistent action for APT source."
  (helm-apt-cache-show candidate))

(defun helm-apt-candidate-transformer (candidates)
  "Show installed CANDIDATES and the ones to deinstall in a different color."
  (loop for cand in candidates
        for name = (helm-apt-display-to-real cand)
        collect (cond ((string= (assoc-default
                                 name helm-apt-installed-packages)
                                "deinstall")
                       (propertize cand 'face 'helm-apt-deinstalled))
                      ((string= (assoc-default
                                 name helm-apt-installed-packages)
                                "install")
                       (propertize cand 'face 'helm-apt-installed))
                      (t cand))))

(defun helm-apt-init ()
  "Initialize list of debian packages."
  (let ((query ""))
    (unless (and helm-apt-installed-packages
                 helm-apt-all-packages)
      (message "Loading package list...")
      (setq helm-apt-installed-packages
            (with-temp-buffer
              (call-process-shell-command "dpkg --get-selections"
                                          nil (current-buffer))
              (loop for i in (split-string (buffer-string) "\n" t)
                    for p = (split-string i)
                    collect (cons (car p) (cadr p)))))
      (setq helm-apt-all-packages
            (with-current-buffer
                (helm-candidate-buffer
                 (get-buffer-create (format "*helm-apt*")))
              (erase-buffer)
              (call-process-shell-command
               (format helm-apt-search-command query)
               nil (current-buffer))))
      (message "Loading package list done")
      (sit-for 0.5))))

(defun helm-apt-display-to-real (line)
  "Return only name of a debian package.
LINE is displayed like:
package name - description."
  (car (split-string line " - ")))

(defun helm-shell-command-if-needed (command)
  "Run shell command COMMAND to describe package.
If a buffer named COMMAND already exists, just switch to it."
  (let ((buf (get-buffer command)))
    (helm-switch-to-buffer (get-buffer-create command))
    (unless buf (insert (shell-command-to-string command)))))

(defun helm-apt-cache-show (package)
  "Show information on apt package PACKAGE."
  (helm-shell-command-if-needed
   (format helm-apt-show-command package)))

(defun helm-apt-install (package)
  "Run 'apt-get install' shell command on PACKAGE."
  (helm-apt-generic-action :action 'install))

(defun helm-apt-reinstall (package)
  "Run 'apt-get install --reinstall' shell command on PACKAGE."
  (helm-apt-generic-action :action 'reinstall))

(defun helm-apt-uninstall (package)
  "Run 'apt-get remove' shell command on PACKAGE."
  (helm-apt-generic-action :action 'uninstall))

(defun helm-apt-purge (package)
  "Run 'apt-get purge' shell command on PACKAGE."
  (helm-apt-generic-action :action 'purge))

(defun* helm-apt-generic-action (&key action)
  "Run 'apt-get ACTION'.
Support install, remove and purge actions."
  (ansi-term (getenv "SHELL") "helm apt")
  (term-line-mode)
  (let ((command   (case action
                     ('install   "sudo apt-get install ")
                     ('reinstall "sudo apt-get install --reinstall ")
                     ('uninstall "sudo apt-get remove ")
                     ('purge     "sudo apt-get purge ")
                     (t          (error "Unknow action"))))
        (beg       (point))
        end
        (cand-list (mapconcat #'(lambda (x) (format "'%s'" x))
                              (helm-marked-candidates) " ")))
    (goto-char (point-max))
    (insert (concat command cand-list))
    (setq end (point))
    (if (y-or-n-p (format "%s package" (symbol-name action)))
        (progn
          (setq helm-external-commands-list nil)
          (setq helm-apt-installed-packages nil)
          (term-char-mode) (term-send-input))
        (delete-region beg end) (term-send-eof) (kill-buffer))))

;;;###autoload
(defun helm-apt (arg)
  "Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache."
  (interactive "P")
  (let ((query (read-string "Search Package: " nil 'helm-apt-input-history)))
    (when arg (helm-apt-refresh))
    (helm :sources 'helm-source-apt
          :prompt "Search Package: "
          :input query
          :history 'helm-apt-input-history)))


(provide 'helm-apt)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-apt.el ends here
