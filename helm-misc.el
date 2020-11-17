;;; helm-misc.el --- Various functions for helm -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2019 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(declare-function helm-comp-read "helm-mode")


(defgroup helm-misc nil
  "Various Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-time-zone-home-location "Paris"
  "The time zone of your home."
  :group 'helm-misc
  :type 'string)

(defcustom helm-timezone-actions
  '(("Set timezone env (TZ)" . (lambda (candidate)
                                 (setenv "TZ" candidate))))
  "Actions for helm-timezone."
  :group 'helm-misc
  :type '(alist :key-type string :value-type function))

(defface helm-time-zone-current
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "green"))
  "Face used to colorize current time in `helm-world-time'."
  :group 'helm-misc)

(defface helm-time-zone-home
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "red"))
  "Face used to colorize home time in `helm-world-time'."
  :group 'helm-misc)



;;; Latex completion
;;
;; Test
;; (setq LaTeX-math-menu '("Math"
;; ["foo" val0 t]
;; ("bar"
;; ["baz" val1 t])
;; ("aze"
;; ["zer" val2 t])
;; ("AMS"
;; ("rec"
;; ["fer" val3 t])
;; ("rty"
;; ["der" val4 t]))
;; ("ABC"
;; ("xcv"
;; ["sdf" val5 t])
;; ("dfg"
;; ["fgh" val6 t]))))
;; (helm-latex-math-candidates)
;; =>
;; (("foo" . val0)
;; ("baz" . val1)
;; ("zer" . val2)
;; ("fer" . val3)
;; ("der" . val4)
;; ("sdf" . val5)
;; ("fgh" . val6))

(defvar LaTeX-math-menu)
(defun helm-latex-math-candidates ()
  (cl-labels ((helm-latex--math-collect (L)
                (cond ((vectorp L)
                       (list (cons (aref L 0) (aref L 1))))
                      ((listp L)
                       (cl-loop for a in L nconc
                                (helm-latex--math-collect a))))))
    (helm-latex--math-collect LaTeX-math-menu)))

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
(defvar zoneinfo-style-world-list)
(defvar legacy-style-world-list)

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
    :init (lambda ()
            (require 'time)
            (unless (and display-time-world-list
                         (listp display-time-world-list))
              ;; adapted from `time--display-world-list' from
              ;; emacs-27 for compatibility as
              ;; `display-time-world-list' is set by default to t.
              (setq display-time-world-list
                    ;; Determine if zoneinfo style timezones are
                    ;; supported by testing that America/New York and
                    ;; Europe/London return different timezones.
                    (let ((nyt (format-time-string "%z" nil "America/New_York"))
                          (gmt (format-time-string "%z" nil "Europe/London")))
                      (if (string-equal nyt gmt)
                          legacy-style-world-list
                        zoneinfo-style-world-list)))))
    :data (lambda ()
            (with-temp-buffer
              (display-time-world-display display-time-world-list)
              (buffer-string)))
    :action 'helm-timezone-actions
    :filtered-candidate-transformer 'helm-time-zone-transformer))

;;; Commands
;;
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
(defvar helm-minibuffer-history-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map [remap helm-minibuffer-history] 'undefined)
    map))

(defcustom helm-minibuffer-history-must-match t
  "Allow inserting non matching elements when nil or 'confirm."
  :group 'helm-misc
  :type '(choice
          (const :tag "Must match" t)
          (const :tag "Confirm" 'confirm)
          (const :tag "Always allow" nil)))


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
  (cl-assert (minibuffer-window-active-p (selected-window)) nil
             "Error: Attempt to use minibuffer history outside a minibuffer")
  (let* ((enable-recursive-minibuffers t)
         (query-replace-p (or (eq last-command 'query-replace)
                              (eq last-command 'query-replace-regexp)))
         (elm (helm-comp-read "Next element matching (regexp): "
                              (cl-loop for i in
                                       (symbol-value minibuffer-history-variable)
                                       unless (equal "" i) collect i into history
                                       finally return
                                       (if (consp (car history))
                                           (mapcar 'prin1-to-string history)
                                           history))
                              :header-name
                              (lambda (name)
                                (format "%s (%s)" name minibuffer-history-variable))
                              :buffer "*helm minibuffer-history*"
                              :must-match helm-minibuffer-history-must-match
                              :multiline t
                              :keymap helm-minibuffer-history-map
                              :allow-nest t)))
    ;; Fix issue #1667 with emacs-25+ `query-replace-from-to-separator'.
    (when (and (boundp 'query-replace-from-to-separator) query-replace-p)
      (let ((pos (string-match "\0" elm)))
        (and pos
             (add-text-properties
              pos (1+ pos)
              `(display ,query-replace-from-to-separator separator t)
              elm))))
    (delete-minibuffer-contents)
    (insert elm)))

;;; GPG keys
;;
;;
(eval-when-compile (require 'epg))
(defvar epa-protocol)
(defvar epa-last-coding-system-specified)
(defvar epg-key-validity-alist)
(defvar mail-header-separator)
(declare-function epg-list-keys             "epg")
(declare-function epg-make-context          "epg")
(declare-function epg-key-sub-key-list      "epg")
(declare-function epg-sub-key-id            "epg")
(declare-function epg-key-user-id-list      "epg")
(declare-function epg-user-id-string        "epg")
(declare-function epg-user-id-validity      "epg")
(declare-function epa-sign-region           "epa")
(declare-function epa--read-signature-type  "epa")
(declare-function epa-display-error         "epa")
(declare-function epg-export-keys-to-string "epg")
(declare-function epg-context-armor         "epg")
(declare-function epg-context-set-armor     "epg")
(declare-function epg-delete-keys           "epg")

(defvar helm-epa--list-only-secrets nil
  "[INTERNAL] Used to pass MODE argument to `epg-list-keys'.")

(defcustom helm-epa-actions '(("Show key" . epa--show-key)
                              ("encrypt file with key" . helm-epa-encrypt-file)
                              ("Copy keys to kill ring" . helm-epa-kill-keys-armor)
                              ("Delete keys" . helm-epa-delete-keys))
  "Actions for `helm-epa-list-keys'."
  :type '(alist :key-type string :value-type symbol)
  :group 'helm-misc)

(defclass helm-epa (helm-source-sync)
  ((init :initform (lambda ()
                     (require 'epg)
                     (require 'epa)))
   (candidates :initform 'helm-epa-get-key-list)
   (keymap :initform helm-comp-read-map)
   (mode-line :initform helm-comp-read-mode-line))
  "Allow building helm sources for GPG keys.")

(defun helm-epa-get-key-list (&optional keys)
  "Build candidate list for `helm-epa-list-keys'."
  (cl-loop with all-keys = (or keys (epg-list-keys (epg-make-context epa-protocol)
                                                   nil helm-epa--list-only-secrets))
           for key in all-keys
           for sublist = (car (epg-key-sub-key-list key))
           for subkey-id = (epg-sub-key-id sublist)
           for uid-list = (epg-key-user-id-list key)
           for uid = (epg-user-id-string (car uid-list))
           for validity = (epg-user-id-validity (car uid-list))
           collect (cons (format " %s %s %s"
                                 (helm-aif (rassq validity epg-key-validity-alist)
                                     (string (car it))
                                   "?")
                                 (propertize
                                  subkey-id
                                  'face (cl-case validity
                                          (none 'epa-validity-medium)
                                          ((revoked expired)
                                           'epa-validity-disabled)
                                          (t 'epa-validity-high)))
                                 (propertize
                                  uid 'face 'font-lock-warning-face))
                         key)))

(defun helm-epa--select-keys (prompt keys)
  "A helm replacement for `epa--select-keys'."
  (let ((result (helm :sources (helm-make-source "Epa select keys" 'helm-epa
                                 :candidates (lambda ()
                                               (helm-epa-get-key-list keys)))
                      :prompt (and prompt (helm-epa--format-prompt prompt))
                      :buffer "*helm epa*")))
    (unless (equal result "")
      result)))

(defun helm-epa--format-prompt (prompt)
  (let ((split (split-string prompt "\n")))
    (if (cdr split)
        (format "%s\n(%s): "
                (replace-regexp-in-string "\\.[\t ]*\\'" "" (car split))
                (replace-regexp-in-string "\\.[\t ]*\\'" "" (cadr split)))
      (format "%s: " (replace-regexp-in-string "\\.[\t ]*\\'" "" (car split))))))

(defun helm-epa--read-signature-type ()
  "A helm replacement for `epa--read-signature-type'."
  (let ((answer (helm-read-answer "Signature type:
(n - Create a normal signature)
(c - Create a cleartext signature)
(d - Create a detached signature)"
                                  '("n" "c" "d"))))
    (helm-acase answer
      ("n" 'normal)
      ("c" 'clear)
      ("d" 'detached))))

;;;###autoload
(define-minor-mode helm-epa-mode
  "Enable helm completion on gpg keys in epa functions."
  :group 'helm-misc
  :global t
  (require 'epa)
  (if helm-epa-mode
      (progn
        (advice-add 'epa--select-keys :override #'helm-epa--select-keys)
        (advice-add 'epa--read-signature-type :override #'helm-epa--read-signature-type))
    (advice-remove 'epa-select-keys #'helm-epa--select-keys)
    (advice-remove 'epa--read-signature-type #'helm-epa--read-signature-type)))

(defun helm-epa-action-transformer (actions _candidate)
  "Helm epa action transformer function."
  (cond ((with-helm-current-buffer
           (derived-mode-p 'message-mode 'mail-mode))
         (helm-append-at-nth
          actions '(("Sign mail with key" . helm-epa-mail-sign)
                    ("Encrypt mail with key" . helm-epa-mail-encrypt))
          3))
        (t actions)))

(defun helm-epa-delete-keys (_candidate)
  "Delete gpg marked keys from helm-epa."
  (let ((context (epg-make-context epa-protocol))
        (keys (helm-marked-candidates)))
    (message "Deleting gpg keys..")
    (condition-case error
	(epg-delete-keys context keys)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (message "Deleting gpg keys done")))
  
(defun helm-epa-encrypt-file (candidate)
  "Select a file to encrypt with key CANDIDATE."
  (let ((file (helm-read-file-name "Encrypt file: "))
        (key (epg-sub-key-id (car (epg-key-sub-key-list candidate))))
        (id  (epg-user-id-string (car (epg-key-user-id-list candidate)))))
    (epa-encrypt-file file candidate)
    (message "File encrypted with key `%s %s'" key id)))

(defun helm-epa-kill-keys-armor (_candidate)
  "Copy marked keys to kill ring."
  (let ((keys (helm-marked-candidates))
        (context (epg-make-context epa-protocol)))
    (with-no-warnings
      (setf (epg-context-armor context) t))
    (condition-case error
	(kill-new (epg-export-keys-to-string context keys))
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))))

(defun helm-epa-mail-sign (candidate)
  "Sign email with key CANDIDATE."
  (let ((key (epg-sub-key-id (car (epg-key-sub-key-list candidate))))
        (id  (epg-user-id-string (car (epg-key-user-id-list candidate))))
        start end mode)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward mail-header-separator nil t)
	  (forward-line))
      (setq epa-last-coding-system-specified
	    (or coding-system-for-write
	        (select-safe-coding-system (point) (point-max))))
      (let ((verbose current-prefix-arg))
        (setq start (point)
              end (point-max)
              mode (if verbose
		       (epa--read-signature-type)
	             'clear))))
    ;; TODO Make non-interactive functions to replace epa-sign-region
    ;; and epa-encrypt-region and inline them.
    (with-no-warnings
      (epa-sign-region start end candidate mode))
    (message "Mail signed with key `%s %s'" key id)))

(defun helm-epa-mail-encrypt (candidate)
  "Encrypt email with key CANDIDATE."
  (let (start end)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward mail-header-separator nil t)
	(forward-line))
      (setq start (point)
            end (point-max))
      (setq epa-last-coding-system-specified
	    (or coding-system-for-write
		(select-safe-coding-system start end))))
    ;; Don't let some read-only text stop us from encrypting.
    (let ((inhibit-read-only t)
          (key (epg-sub-key-id (car (epg-key-sub-key-list candidate))))
          (id  (epg-user-id-string (car (epg-key-user-id-list candidate)))))
      (with-no-warnings
        (epa-encrypt-region start end candidate nil nil))
      (message "Mail encrypted with key `%s %s'" key id))))

;;;###autoload
(defun helm-epa-list-keys ()
  "List all gpg keys.
This is the helm interface for `epa-list-keys'."
  (interactive)
  (helm :sources
        (helm-make-source "Epg list keys" 'helm-epa
          :action-transformer 'helm-epa-action-transformer
          :action 'helm-epa-actions)
        :buffer "*helm epg list keys*"))

(provide 'helm-misc)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-misc.el ends here
