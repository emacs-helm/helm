;;; helm-command.el --- Helm execute-exended-command. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
  "Enable fuzzy matching in `helm-M-x' when non--nil.")


;;; Faces
;;
;;
(defgroup helm-command-faces nil
  "Customize the appearance of helm-command."
  :prefix "helm-"
  :group 'helm-command
  :group 'helm-faces)

(defface helm-M-x-key '((t (:foreground "orange" :underline t)))
  "Face used in helm-M-x to show keybinding."
  :group 'helm-command-faces)


(defvar helm-M-x-input-history nil)


(cl-defun helm-M-x-get-major-mode-command-alist (mode-map)
  "Return alist of MODE-MAP."
  (when mode-map
    (cl-loop for key being the key-seqs of mode-map using (key-bindings com)
             for str-key  = (key-description key)
             for ismenu   = (string-match "<menu-bar>" str-key)
             unless ismenu collect (cons str-key com))))

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

(defun helm-M-x-current-mode-map-alist ()
  "Return mode-map alist of current `major-mode'."
  (let ((map-sym (helm-get-mode-map-from-mode major-mode)))
    (when (and map-sym (boundp map-sym))
      (helm-M-x-get-major-mode-command-alist (symbol-value map-sym)))))


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

(defun helm-M-x-transformer (candidates _source)
  "Transformer function for `helm-M-x' candidates."
  (helm-M-x-transformer-1 candidates (null helm--in-fuzzy)))

(defun helm-M-x-transformer-hist (candidates _source)
  "Transformer function for `helm-M-x' candidates."
  (helm-M-x-transformer-1 candidates))

(defun helm-M-x--notify-prefix-arg ()
  ;; Notify a prefix-arg set AFTER calling M-x.
  (when prefix-arg
    (with-helm-window
      (helm-display-mode-line (helm-get-current-source) 'force))))

(defun helm-cmd--get-current-function-name ()
  (save-excursion
    (beginning-of-defun)
    (cadr (split-string (buffer-substring-no-properties
                         (point-at-bol) (point-at-eol))))))

(defun helm-cmd--get-preconfigured-commands (&optional dir)
  (let* ((helm-dir (or dir (helm-basedir (locate-library "helm"))))
         (helm-autoload-file (expand-file-name "helm-autoloads.el" helm-dir))
         results)
    (when (file-exists-p helm-autoload-file)
      (with-temp-buffer
        (insert-file-contents helm-autoload-file)
        (while (re-search-forward "Preconfigured" nil t)
          (push (substring (helm-cmd--get-current-function-name) 1) results))))
    results))

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

;;;###autoload
(defun helm-M-x (arg &optional command-name)
  "Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

Unlike regular `M-x' emacs vanilla `execute-extended-command' command,
the prefix args if needed, are passed AFTER starting `helm-M-x'.

You can get help on each command by persistent action."
  (interactive (list current-prefix-arg (helm-M-x-read-extended-command)))
  (let ((sym-com (and (stringp command-name) (intern-soft command-name))))
    ;; Avoid having `this-command' set to *exit-minibuffer.
    (setq this-command sym-com
          ;; Handle C-x z (repeat) Issue #322
          real-this-command sym-com)
    ;; If helm-M-x is called with regular emacs completion (kmacro)
    ;; use the value of arg otherwise use helm-current-prefix-arg.
    (let ((prefix-arg (or helm-current-prefix-arg arg)))
      ;; This ugly construct is to save history even on error.
      (unless helm-M-x-always-save-history
        (command-execute sym-com 'record))
      (setq extended-command-history
            (cons command-name
                  (delete command-name extended-command-history)))
      (when helm-M-x-always-save-history
        (command-execute sym-com 'record)))))


(provide 'helm-command)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-command.el ends here
