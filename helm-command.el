;;; helm-command.el --- Helm execute-exended-command. -*- lexical-binding: t -*-

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
(require 'helm-mode)
(require 'helm-elisp)


;;; Customize

(defgroup helm-command nil
  "Emacs command related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-M-x-requires-pattern 0
  "Value of requires-pattern for `helm-M-x'.
Show all candidates on startup when 0 (default)."
  :group 'helm-command
  :type 'boolean)

(defcustom helm-M-x-always-save-history nil
  "`helm-M-x' Save command in `extended-command-history' even when it fail."
  :group 'helm-command
  :type  'boolean)

(defcustom helm-M-x-reverse-history nil
  "The history source of `helm-M-x' appear in second position when non--nil."
  :group 'helm-command
  :type 'boolean)

(defcustom helm-M-x-fuzzy-match nil
  "Enable fuzzy matching in `helm-M-x' when non--nil."
  :group 'helm-command
  :type 'boolean)

(defcustom helm-M-x-actions
  '(("Execute" . helm-M-x-execute)
    ("Find command" . helm-find-function)
    ("Info lookup" . (lambda (candidate)
                       (helm-info-lookup-symbol (helm-stringify candidate))))
    ("Describe command" . helm-describe-function))
  "Actions for `helm-M-x'."
  :group 'helm-command
  :type '(alist :key-type string :value-type function))


;;; Faces

(defgroup helm-command-faces nil
  "Customize the appearance of helm-command."
  :prefix "helm-"
  :group 'helm-command
  :group 'helm-faces)

(defface helm-M-x-key '((t (:foreground "orange" :underline t)))
  "Face used in helm-M-x to show keybinding."
  :group 'helm-command-faces)


;;; History

(defvar helm-M-x-input-history nil)

(defun helm-M-x--add-command-to-history (command)
  "Add COMMAND to `extended-command-history'.

COMMAND is only added if it is a valid command."
  (when (commandp (intern-soft command))
    (let ((command (helm-stringify command)))
      (setq extended-command-history
            (cons command
                  (delete command extended-command-history))))))


;;; Mostly obsolete stuff

(cl-defun helm-M-x-get-major-mode-command-alist (mode-map)
  "Return alist of MODE-MAP."
  (when mode-map
    (cl-loop for key being the key-seqs of mode-map using (key-bindings com)
             for str-key  = (key-description key)
             for ismenu   = (string-match "<menu-bar>" str-key)
             unless ismenu collect (cons str-key com))))
(make-obsolete #'helm-M-x-get-major-mode-command-alist nil)

(defun helm-get-mode-map-from-mode (mode)
  "Guess the mode-map name according to MODE.
Some modes don't use conventional mode-map name
so we need to guess mode-map name. e.g python-mode ==> py-mode-map.
Return nil if no mode-map found."
  (cl-loop ;; Start with a conventional mode-map name.
        with mode-map    = (intern-soft (format "%s-map" mode))
        with mode-string = (symbol-name mode)
        with mode-name   = (replace-regexp-in-string "-mode" "" mode-string)
        while (not mode-map)
        for count downfrom (length mode-name)
        ;; Return when no result after parsing entire string.
        when (eq count 0) return nil
        for sub-name = (substring mode-name 0 count)
        do (setq mode-map (intern-soft (format "%s-map" (concat sub-name "-mode"))))
        finally return mode-map))
(make-obsolete #'helm-get-mode-map-from-mode nil)

(defun helm-M-x-current-mode-map-alist ()
  "Return mode-map alist of current `major-mode'."
  (let ((map-sym (helm-get-mode-map-from-mode major-mode)))
    (when (and map-sym (boundp map-sym))
      (helm-M-x-get-major-mode-command-alist (symbol-value map-sym)))))
(make-obsolete #'helm-M-x-current-mode-map-alist nil)

(defun helm-M-x-transformer-1 (candidates &optional sort)
  "Transformer function to show bindings in emacs commands.
Show global bindings and local bindings according to current `major-mode'.
If SORT is non nil sort list with `helm-generic-sort-fn'.
Note that SORT should not be used when fuzzy matching because
fuzzy matching is running its own sort function with a different algorithm."
  (with-helm-current-buffer
    (cl-loop with local-map = (helm-M-x-current-mode-map-alist)
          for cand in candidates
          for local-key  = (car (rassq cand local-map))
          for key        = (substitute-command-keys (format "\\[%s]" cand))
          unless (get (intern (if (consp cand) (car cand) cand)) 'helm-only)
          collect
          (cons (cond ((and (string-match "^M-x" key) local-key)
                       (format "%s (%s)"
                               cand (propertize
                                     local-key
                                     'face 'helm-M-x-key)))
                      ((string-match "^M-x" key) cand)
                      (t (format "%s (%s)"
                                 cand (propertize
                                       key
                                       'face 'helm-M-x-key))))
                cand)
          into ls
          finally return
          (if sort (sort ls #'helm-generic-sort-fn) ls))))
(make-obsolete #'helm-M-x-transformer-1 nil)

(defun helm-M-x-transformer (candidates _source)
  "Transformer function for `helm-M-x' candidates."
  (helm-M-x-transformer-1 candidates (null helm--in-fuzzy)))
(make-obsolete #'helm-M-x-transformer nil)

(defun helm-M-x-transformer-hist (candidates _source)
  "Transformer function for `helm-M-x' candidates."
  (helm-M-x-transformer-1 candidates))
(make-obsolete #'helm-M-x-transformer-hist nil)

(defun helm-M-x--notify-prefix-arg ()
  ;; Notify a prefix-arg set AFTER calling M-x.
  (when prefix-arg
    (with-helm-window
      (helm-display-mode-line (helm-get-current-source) 'force))))

(defun helm-M-x-read-extended-command (&optional collection history)
  "Read command name to invoke in `helm-M-x'.
Helm completion is not provided when executing or defining
kbd macros.
Optional arg COLLECTION is to allow using another COLLECTION
than the default which is OBARRAY."
  (if (or defining-kbd-macro executing-kbd-macro)
      (if helm-mode
          (unwind-protect
               (progn
                 (helm-mode -1)
                 (read-extended-command))
            (helm-mode 1))
          (read-extended-command))
      (let* ((orig-fuzzy-sort-fn helm-fuzzy-sort-fn)
             (helm-fuzzy-sort-fn (lambda (candidates source)
                                   (funcall orig-fuzzy-sort-fn
                                            candidates source 'real)))
             (helm--mode-line-display-prefarg t)
             (tm (run-at-time 1 0.1 'helm-M-x--notify-prefix-arg))
             (helm-move-selection-after-hook
              (cons (lambda () (setq current-prefix-arg nil))
                    helm-move-selection-after-hook)))
        (setq extended-command-history
              (cl-loop for c in extended-command-history
                       when (and c (commandp (intern c)))
                       do (set-text-properties 0 (length c) nil c)
                       and collect c))
        (unwind-protect
             (let ((msg "Error: Specifying a prefix arg before calling `helm-M-x'"))
               (when current-prefix-arg
                 (ding)
                 (message "%s" msg)
                 (while (not (sit-for 1))
                   (discard-input))
                 (user-error msg))
               (setq current-prefix-arg nil)
               (helm-comp-read
                "M-x " (or collection obarray)
                :test 'commandp
                :requires-pattern helm-M-x-requires-pattern
                :name "Emacs Commands"
                :buffer "*helm M-x*"
                :persistent-action (lambda (candidate)
                                     (helm-elisp--persistent-help
                                      candidate 'helm-describe-function))
                :persistent-help "Describe this command"
                :history (or history extended-command-history)
                :reverse-history helm-M-x-reverse-history
                :input-history 'helm-M-x-input-history
                :del-input nil
                :help-message 'helm-M-x-help-message
                :must-match t
                :fuzzy helm-M-x-fuzzy-match
                :nomark t
                :candidates-in-buffer t
                :fc-transformer 'helm-M-x-transformer
                :hist-fc-transformer 'helm-M-x-transformer-hist))
          (cancel-timer tm)
          (setq helm--mode-line-display-prefarg nil)))))
(make-obsolete #'helm-M-x-read-extended-command nil)


;;; Sources

(defun helm-M-x--commands (&optional obarray)
  "Return list of commands from OBARRAY.

If OBARRAY is not supplied, use the default `obarray'."
  (let ((atoms))
    (mapatoms (lambda (atom)
                (if (commandp atom)
                    (push atom atoms)))
              obarray)
    atoms))

(defun helm-M-x--make-candidate (command)
  "Create a `helm-M-x' candidate for a given COMMAND.

Each candidate is of the form (DISPLAY STRING . SYMBOL)."
  (let* ((command (intern-soft command))
         (first-key (where-is-internal command nil t)))
    (if (not first-key)
        (cons (helm-stringify command) command)
      (let* ((key (propertize (key-description first-key)
                              'face 'helm-M-x-key))
             (key-string (if (not (string= "" key))
                             (concat " (" key ")")
                           "")))
        (cons (concat (helm-stringify command) key-string)
              command)))))

(defun helm-M-x--candidates ()
  "Candidates for `helm-M-x--commands-source'."
  (let ((candidates (mapcar #'helm-M-x--make-candidate
                            (helm-M-x--commands))))
    (sort candidates
     (lambda (c1 c2)
       (string< (cdr c1) (cdr c2))))))

(defun helm-M-x--commands-source ()
  "`helm' source for Emacs commands."
  (helm-build-sync-source "Emacs commands"
    :action 'helm-M-x-actions
    :candidates (helm-M-x--candidates)
    :fuzzy-match helm-M-x-fuzzy-match
    :nomark t
    :persistent-action #'helm-describe-function
    :persistent-help "Describe command"
    :requires-pattern helm-M-x-requires-pattern
    :volatile t))

(defun helm-M-x--command-history-source ()
  "`helm' source for Emacs command history."
  (helm-build-sync-source "Emacs commands history"
    :action 'helm-M-x-actions
    :candidates (mapcar #'helm-M-x--make-candidate
                        extended-command-history)
    :fuzzy-match helm-M-x-fuzzy-match
    :nomark t
    :persistent-action #'helm-describe-function
    :persistent-help "Describe command"
    :requires-pattern helm-M-x-requires-pattern))


;;; API

;;;###autoload
(defun helm-M-x-execute (command &optional arg)
  "Execute COMMAND for `helm-M-x'.

If `helm-M-x-always-save-to-history' is non-nil, save command to
history before execution.

Optional parameter ARG specifies the prefix arg to COMMAND."
  (let ((command (intern-soft command))
        (prefix-arg (or arg helm-current-prefix-arg)))
    (when (commandp command)
      (unless helm-M-x-always-save-history
        (command-execute command t))
      (helm-M-x--add-command-to-history command)
      (when helm-M-x-always-save-history
        (command-execute command t)))))

;;;###autoload
(defun helm-M-x (arg &optional command)
  "Preconfigured `helm' for Emacs commands.

Unlike the regular
`execute-extended-command' (\\[execute-extended-command]), the
prefix args need to be passed AFTER starting `helm-M-x'.

Parameter ARG is maintained only for backward compatibility. It
does nothing.

Optional parameter COMMAND is maintained only for
backward compatibility. It does nothing."
  (interactive (list current-prefix-arg nil))
  (cond (command (helm-M-x-execute command arg))
        (arg (user-error "Error: Specifying a prefix arg before calling `helm-M-x'"))
        (t (let ((sources '(helm-M-x--command-history-source
                            helm-M-x--commands-source))
                 (helm--mode-line-display-prefarg t)
                 (tm (run-at-time 1 0.1 'helm-M-x--notify-prefix-arg)))
             (unwind-protect
                 (helm :sources (mapcar #'funcall (if helm-M-x-reverse-history
                                                      (reverse sources)
                                                    sources))
                       :buffer "*helm-M-x*"
                       :prompt "Command: "
                       :history 'helm-M-x-input-history)
               (setq helm--mode-line-display-prefarg nil)
               (cancel-timer tm))))))

(provide 'helm-command)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-command.el ends here
