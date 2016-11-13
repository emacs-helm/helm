;;; helm-misc.el --- Various functions for helm -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2016 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-help)
(require 'helm-types)

(declare-function display-time-world-display "time.el")
(defvar display-time-world-list)
(declare-function LaTeX-math-mode "ext:latex.el")
(declare-function jabber-chat-with "ext:jabber.el")
(declare-function jabber-read-account "ext:jabber.el")


(defgroup helm-misc nil
  "Various Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-time-zone-home-location "Paris"
  "The time zone of your home"
  :group 'helm-misc
  :type 'string)

(defcustom helm-timezone-actions
  '(("Set timezone env (TZ)" . (lambda (candidate)
                                 (setenv "TZ" candidate))))
  "Actions for helm-timezone."
  :group 'helm-misc
  :type '(alist :key-type string :value-type function))

(defcustom helm-mini-default-sources '(helm-source-buffers-list
                                       helm-source-recentf
                                       helm-source-buffer-not-found)
  "Default sources list used in `helm-mini'."
  :group 'helm-misc
  :type '(repeat (choice symbol)))

(defface helm-time-zone-current
    '((t (:foreground "green")))
  "Face used to colorize current time in `helm-world-time'."
  :group 'helm-misc)

(defface helm-time-zone-home
    '((t (:foreground "red")))
  "Face used to colorize home time in `helm-world-time'."
  :group 'helm-misc)



;;; Latex completion
(defvar LaTeX-math-menu)
(defun helm-latex-math-candidates ()
  "Collect candidates for latex math completion."
  (cl-loop for i in (cddr LaTeX-math-menu)
        for elm = (cl-loop for s in i when (vectorp s)
                        collect (cons (aref s 0) (aref s 1)))
        append elm))

(defvar helm-source-latex-math
  (helm-build-sync-source "Latex Math Menu"
    :init (lambda ()
            (with-helm-current-buffer
              (LaTeX-math-mode 1)))
    :candidate-number-limit 9999
    :candidates 'helm-latex-math-candidates
    :action (lambda (candidate)
              (call-interactively candidate))))


;;; Jabber Contacts (jabber.el)
(defun helm-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (cl-loop for item in (jabber-concat-rosters)
          when (get item 'connected)
          collect
          (if (get item 'name)
              (cons (get item 'name) item)
            (cons (symbol-name item) item)))))

(defvar helm-source-jabber-contacts
  (helm-build-sync-source "Jabber Contacts"
    :init (lambda () (require 'jabber))
    :candidates (lambda () (mapcar 'car (helm-jabber-online-contacts)))
    :action (lambda (x)
              (jabber-chat-with
               (jabber-read-account)
               (symbol-name
                (cdr (assoc x (helm-jabber-online-contacts))))))))

;;; World time
;;
(defun helm-time-zone-transformer (candidates _source)
  (cl-loop for i in candidates
           for (z . p) in display-time-world-list
           collect
           (cons 
            (cond ((string-match (format-time-string "%H:%M" (current-time)) i)
                   (propertize i 'face 'helm-time-zone-current))
                  ((string-match helm-time-zone-home-location i)
                   (propertize i 'face 'helm-time-zone-home))
                  (t i))
            z)))

(defvar helm-source-time-world
  (helm-build-in-buffer-source "Time World List"
    :data (lambda ()
            (with-temp-buffer
              (display-time-world-display display-time-world-list)
              (buffer-string)))
    :action 'helm-timezone-actions
    :filtered-candidate-transformer 'helm-time-zone-transformer))

;;; LaCarte
;;
;;
(declare-function lacarte-get-overall-menu-item-alist "ext:lacarte.el" (&optional MAPS))

(defun helm-lacarte-candidate-transformer (cands)
  (mapcar (lambda (cand)
            (let* ((item (car cand))
                   (match (string-match "[^>] \\((.*)\\)$" item)))
              (when match
                (put-text-property (match-beginning 1) (match-end 1)
                                   'face 'helm-M-x-key item))
              cand))
          cands))

(defclass helm-lacarte (helm-source-sync helm-type-command)
    ((init :initform (lambda () (require 'lacarte)))
     (candidates :initform 'helm-lacarte-get-candidates)
     (candidate-transformer :initform 'helm-lacarte-candidate-transformer)
     (candidate-number-limit :initform 9999)))

(defun helm-lacarte-get-candidates (&optional maps)
  "Extract candidates for menubar using lacarte.el.
See http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el.
Optional argument MAPS is a list specifying which keymaps to use: it
can contain the symbols `local', `global', and `minor', mean the
current local map, current global map, and all current minor maps."
  (with-helm-current-buffer
    ;; When a keymap doesn't have a [menu-bar] entry
    ;; the filtered map returned and passed to
    ;; `lacarte-get-a-menu-item-alist-22+' is nil, which
    ;; fails because this code is not protected for such case.
    (condition-case nil
        (lacarte-get-overall-menu-item-alist maps)
      (error nil))))

;;;###autoload
(defun helm-browse-menubar ()
  "Preconfigured helm to the menubar using lacarte.el."
  (interactive)
  (require 'lacarte)
  (helm :sources (mapcar 
                  (lambda (spec) (helm-make-source (car spec) 'helm-lacarte
                                   :candidates
                                   (lambda ()
                                     (helm-lacarte-get-candidates (cdr spec)))))
                  '(("Major Mode"  . (local))
                    ("Minor Modes" . (minor))
                    ("Global Map"  . (global))))
        :buffer "*helm lacarte*"))

(defun helm-call-interactively (cmd-or-name)
  "Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`helm-current-prefix-arg' is used as the command's prefix argument."
  (setq extended-command-history
        (cons (helm-stringify cmd-or-name)
              (delete (helm-stringify cmd-or-name) extended-command-history)))
  (let ((current-prefix-arg helm-current-prefix-arg)
        (cmd (helm-symbolify cmd-or-name)))
    (if (stringp (symbol-function cmd))
        (execute-kbd-macro (symbol-function cmd))
      (setq this-command cmd)
      (call-interactively cmd))))

;;; Minibuffer History
;;
;;
(defvar helm-source-minibuffer-history
  (helm-build-sync-source "Minibuffer History"
    :header-name (lambda (name)
                   (format "%s (%s)" name minibuffer-history-variable))
    :candidates
     (lambda ()
       (let ((history (cl-loop for i in
                               (symbol-value minibuffer-history-variable)
                               unless (string= "" i) collect i)))
         (if (consp (car history))
             (mapcar 'prin1-to-string history)
             history)))
    :migemo t
    :multiline t
    :action (lambda (candidate)
              (with-helm-current-buffer
                (delete-minibuffer-contents)
                (insert candidate)))))

;;; Shell history
;;
;;
(defun helm-comint-input-ring-action (candidate)
  "Default action for comint history."
  (with-helm-current-buffer
    (delete-region (comint-line-beginning-position) (point-max))
    (insert candidate)))

(defvar helm-source-comint-input-ring
  (helm-build-sync-source "Comint history"
    :candidates (lambda ()
                  (with-helm-current-buffer
                    (ring-elements comint-input-ring)))
    :action 'helm-comint-input-ring-action)
  "Source that provide helm completion against `comint-input-ring'.")


;;; Helm ratpoison UI
;;
;;
(defvar helm-source-ratpoison-commands
  (helm-build-in-buffer-source "Ratpoison Commands"
    :init 'helm-ratpoison-commands-init
    :action (helm-make-actions
             "Execute the command" 'helm-ratpoison-commands-execute)
    :display-to-real 'helm-ratpoison-commands-display-to-real
    :candidate-number-limit 999999))

(defun helm-ratpoison-commands-init ()
  (unless (helm-candidate-buffer)
    (with-current-buffer (helm-candidate-buffer 'global)
      ;; with ratpoison prefix key
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "<ratpoison> \\1: \\2"))
      (goto-char (point-max))
      ;; direct binding
      (save-excursion
        (call-process "ratpoison" nil (current-buffer) nil "-c" "help top"))
      (while (re-search-forward "^\\([^ ]+\\) \\(.+\\)$" nil t)
        (replace-match "\\1: \\2")))))

(defun helm-ratpoison-commands-display-to-real (display)
  (and (string-match ": " display)
       (substring display (match-end 0))))

(defun helm-ratpoison-commands-execute (candidate)
  (call-process "ratpoison" nil nil nil "-ic" candidate))

;;; Helm stumpwm UI
;;
;;
(defvar helm-source-stumpwm-commands
  (helm-build-in-buffer-source "Stumpwm Commands"
    :init 'helm-stumpwm-commands-init
    :action (helm-make-actions
             "Execute the command" 'helm-stumpwm-commands-execute)
    :candidate-number-limit 999999))

(defun helm-stumpwm-commands-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (save-excursion
      (call-process "stumpish" nil (current-buffer) nil "commands"))
    (while (re-search-forward "[ ]*\\([^ ]+\\)[ ]*\n?" nil t)
      (replace-match "\n\\1\n"))
    (delete-blank-lines)
    (sort-lines nil (point-min) (point-max))
    (goto-char (point-max))))

(defun helm-stumpwm-commands-execute (candidate)
  (call-process "stumpish" nil nil nil  candidate))

;;;###autoload
(defun helm-world-time ()
  "Preconfigured `helm' to show world time.
Default action change TZ environment variable locally to emacs."
  (interactive)
  (helm-other-buffer 'helm-source-time-world "*helm world time*"))

;;;###autoload
(defun helm-insert-latex-math ()
  "Preconfigured helm for latex math symbols completion."
  (interactive)
  (helm-other-buffer 'helm-source-latex-math "*helm latex*"))

;;;###autoload
(defun helm-ratpoison-commands ()
  "Preconfigured `helm' to execute ratpoison commands."
  (interactive)
  (helm-other-buffer 'helm-source-ratpoison-commands
                     "*helm ratpoison commands*"))

;;;###autoload
(defun helm-stumpwm-commands()
  "Preconfigured helm for stumpwm commands."
  (interactive)
  (helm-other-buffer 'helm-source-stumpwm-commands
                     "*helm stumpwm commands*"))

;;;###autoload
(defun helm-minibuffer-history ()
  "Preconfigured `helm' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (helm :sources 'helm-source-minibuffer-history
          :buffer "*helm minibuffer-history*"
          :allow-nest t)))

;;;###autoload
(defun helm-comint-input-ring ()
  "Preconfigured `helm' that provide completion of `comint' history."
  (interactive)
  (when (derived-mode-p 'comint-mode)
    (helm :sources 'helm-source-comint-input-ring
          :input (buffer-substring-no-properties (comint-line-beginning-position)
                                                 (point-at-eol))
          :buffer "*helm comint history*")))


(provide 'helm-misc)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-misc.el ends here
