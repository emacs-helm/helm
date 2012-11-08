;;; helm-command.el --- Helm execute-exended-command.

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

(require 'cl)
(require 'helm)
(require 'helm-mode)


(defgroup helm-command nil
  "Emacs command related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-M-x-requires-pattern 2
  "Value of requires-pattern for `helm-M-x'.
Set it to 0 to disable requires-pattern in `helm-M-x'."
  :group 'helm-command
  :type 'boolean)

(defcustom helm-M-x-always-save-history nil
  "`helm-M-x' Save command in `extended-command-history' even when it fail."
  :group 'helm-command
  :type  'boolean)


;;; Faces
;;
;;
(defface helm-M-x-key '((t (:foreground "orange" :underline t)))
  "Face used in helm-M-x to show keybinding."
  :group 'helm-command)


(defvar helm-M-x-input-history nil)

(defun* helm-M-x-get-major-mode-command-alist (mode-map)
  "Return alist of MODE-MAP."
  (loop for key being the key-seqs of mode-map using (key-bindings com)
        for str-key  = (key-description key)
        for ismenu   = (string-match "<menu-bar>" str-key)
        unless ismenu collect (cons str-key com)))

(defun helm-get-mode-map-from-mode (mode)
  "Guess the mode-map name according to MODE.
Some modes don't use conventional mode-map name
so we need to guess mode-map name. e.g python-mode ==> py-mode-map.
Return nil if no mode-map found."
  (loop ;; Start with a conventional mode-map name.
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
  (let ((map (helm-get-mode-map-from-mode major-mode)))
    (when (and map (boundp map))
      (helm-M-x-get-major-mode-command-alist (symbol-value map)))))


(defun helm-M-x-transformer (candidates sources)
  "filtered-candidate-transformer to show bindings in emacs commands.
Show global bindings and local bindings according to current `major-mode'."
  (with-helm-current-buffer
    (loop with local-map = (helm-M-x-current-mode-map-alist)
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
                cand) into ls
          finally return
          (sort ls #'(lambda (x y) (string-lessp (car x) (car y)))))))

;;;###autoload
(defun helm-M-x ()
  "Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'."
  (interactive)
  (let* ((history (loop with hist
                        for i in extended-command-history
                        for com = (intern i)
                        when (commandp com)
                        collect i into hist finally return hist))
         command sym-com in-help help-cand
         helm-persistent-action-use-special-display
         (pers-help #'(lambda (candidate)
                        (let ((hbuf (get-buffer (help-buffer)))
                              special-display-buffer-names
                              special-display-regexps)
                          (if (and in-help (string= candidate help-cand))
                              (progn
                                ;; When M-x is started from a help buffer,
                                ;; Don't kill it as it is helm-current-buffer.
                                (unless (equal hbuf helm-current-buffer)
                                  (kill-buffer hbuf)
                                  (set-window-buffer (get-buffer-window hbuf)
                                                     helm-current-buffer))
                                (setq in-help nil))
                              ;; Be sure helm-current-buffer
                              ;; have not a dedicated window.
                              (set-window-dedicated-p
                               (get-buffer-window helm-current-buffer) nil)
                              (describe-function (intern candidate))
                              (message nil)
                              (setq in-help t))
                          (setq help-cand candidate)))))
    (setq command (helm-comp-read
                   "M-x " obarray
                   :test 'commandp
                   :requires-pattern helm-M-x-requires-pattern
                   :name "Emacs Commands"
                   :buffer "*helm M-x*"
                   :persistent-action pers-help
                   :persistent-help "Describe this command"
                   :history history
                   :del-input nil
                   :mode-line helm-mode-line-string
                   :must-match t
                   :candidates-in-buffer t
                   :fc-transformer 'helm-M-x-transformer))
    (setq sym-com (intern command))
    (unless current-prefix-arg
      (setq current-prefix-arg helm-current-prefix-arg))
    ;; Avoid having `this-command' set to *exit-minibuffer.
    (setq this-command sym-com)
    ;; This ugly construct is to save history even on error.
    (unless helm-M-x-always-save-history
      (call-interactively sym-com))
    (setq extended-command-history
          (cons command (delete command history)))
    (when helm-M-x-always-save-history
      (call-interactively sym-com))))

(provide 'helm-command)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-command.el ends here
