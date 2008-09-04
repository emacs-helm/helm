;;; anything-complete.el --- completion with anything
;; $Id: anything-complete.el,v 1.8 2008-09-04 16:54:59 rubikitch Exp $

;; Copyright (C) 2008  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: matching, convenience, anything
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-complete.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Completion with Anything interface.

;; * `anything-lisp-complete-symbol', `anything-lisp-complete-symbol-partial-match':
;;     `lisp-complete-symbol' with `anything'
;; * `anything-apropos': `apropos' with `anything'
;; * `anything-complete-shell-history': complete from .*sh_history
;; * Many anything sources:
;;     [EVAL IT] (occur "defvar anything-c-source")

;;; Installation:

;; Install anything-match-plugin.el
;; M-x install-elisp http://www.emacswiki.org/cgi-bin/wiki/download/anything-match-plugin.el
;;
;; shell-history.el would help you.
;; M-x install-elisp http://www.emacswiki.org/cgi-bin/wiki/download/shell-history.el

;; (require 'anything-complete)
;; ;; Automatically collect symbols by 150 secs
;; (anything-lisp-complete-symbol-set-timer 150)

;;; History:

;; $Log: anything-complete.el,v $
;; Revision 1.8  2008-09-04 16:54:59  rubikitch
;; add commentary
;;
;; Revision 1.7  2008/09/04 08:36:08  rubikitch
;; fixed a bug when `symbol-at-point' is nil.
;;
;; Revision 1.6  2008/09/04 08:29:40  rubikitch
;; remove unneeded code.
;;
;; Revision 1.5  2008/09/04 08:12:05  rubikitch
;; absorb anything-lisp-complete-symbol.el v1.13.
;;
;; Revision 1.4  2008/09/04 07:36:23  rubikitch
;; Use type plug-in instead.
;;
;; Revision 1.3  2008/09/03 04:13:23  rubikitch
;; `anything-c-source-complete-shell-history': deleted requires-pattern
;;
;; Revision 1.2  2008/09/01 22:27:45  rubikitch
;; *** empty log message ***
;;
;; Revision 1.1  2008/09/01 22:23:55  rubikitch
;; Initial revision
;;

;;; History of anything-lisp-complete-symbol.el

;; Revision 1.13  2008/09/04 08:07:18  rubikitch
;; use `anything-complete-target' rather than `alcs-target'.
;;
;; Revision 1.12  2008/09/04 07:50:34  rubikitch
;; add docstrings
;;
;; Revision 1.11  2008/09/04 07:35:02  rubikitch
;; use `add-to-list' to add `anything-type-attributes' entry.
;;
;; Revision 1.10  2008/09/04 01:19:56  rubikitch
;; New source: `anything-c-source-emacs-function-at-point'
;; New source: `anything-c-source-emacs-variable-at-point'
;;
;; Revision 1.9  2008/09/04 00:48:22  rubikitch
;; New action: find-function, find-variable
;;
;; Revision 1.8  2008/09/03 11:02:51  rubikitch
;; do `alcs-make-candidates' after load this file.
;;
;; Revision 1.7  2008/08/29 09:32:46  rubikitch
;; make `alcs-make-candidates' faster
;;
;; Revision 1.6  2008/08/29 09:22:02  rubikitch
;; add command sources.
;; New command: `anything-apropos'
;;
;; Revision 1.5  2008/08/29 02:38:42  rubikitch
;; New command: `anything-lisp-complete-symbol-partial-match'
;;
;; Revision 1.4  2008/08/26 10:42:54  rubikitch
;; integration with `anything-dabbrev-expand'
;;
;; Revision 1.3  2008/08/25 20:45:45  rubikitch
;; export variables
;;
;; Revision 1.2  2008/08/25 20:29:48  rubikitch
;; add requires
;;
;; Revision 1.1  2008/08/25 20:26:09  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-complete-version "$Id: anything-complete.el,v 1.8 2008-09-04 16:54:59 rubikitch Exp $")
(require 'anything-match-plugin)
(require 'thingatpt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  core                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-complete-target "")
(defun ac-candidates-in-buffer ()
  (let ((anything-pattern
         (if (equal "" anything-complete-target)
             anything-pattern
           (concat (if (anything-attr 'prefix-match) "^" "")
                   anything-complete-target " " anything-pattern))))
    (anything-candidates-in-buffer)))

(defun ac-insert (candidate)
  (let ((pt (point)))
    (when (and (search-backward anything-complete-target nil t)
               (string= (buffer-substring (point) pt) anything-complete-target))
      (delete-region (point) pt)))
  (insert candidate))

(add-to-list 'anything-type-attributes
             '(complete
               (candidates-in-buffer . ac-candidates-in-buffer)
               (action . ac-insert)))

(defun anything-complete (sources target &optional limit idle-delay input-idle-delay)
  "Basic completion interface using `anything'."
  (let ((anything-candidate-number-limit (or limit anything-candidate-number-limit))
        (anything-idle-delay (or idle-delay anything-idle-delay))
        (anything-input-idle-delay (or input-idle-delay anything-input-idle-delay))
        (anything-complete-target target))
    (anything sources)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  `lisp-complete-symbol' and `apropos' replacement                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-lisp-complete-symbol-input-idle-delay nil
  "`anything-input-idle-delay' for `anything-lisp-complete-symbol',
`anything-lisp-complete-symbol-partial-match' and `anything-apropos'.")

(defun alcs-create-buffer (name)
  (let ((b (get-buffer-create name)))
    (with-current-buffer b
      (buffer-disable-undo)
      (erase-buffer)
      b)))

(defvar alcs-variables-buffer " *variable symbols*")
(defvar alcs-functions-buffer " *function symbols*")
(defvar alcs-commands-buffer " *command symbols*")
(defvar alcs-symbol-buffer " *other symbols*")

(defun alcs-make-candidates ()
  (message "Collecting symbols...")
  (alcs-create-buffer alcs-variables-buffer)
  (alcs-create-buffer alcs-functions-buffer)
  (alcs-create-buffer alcs-commands-buffer)
  (alcs-create-buffer alcs-symbol-buffer)
  (mapatoms
   (lambda (sym)
     (let ((name (symbol-name sym))
           (fbp (fboundp sym)))
       (cond ((commandp sym) (set-buffer alcs-commands-buffer) (insert name "\n"))
             (fbp (set-buffer alcs-functions-buffer) (insert name "\n")))
       (cond ((boundp sym) (set-buffer alcs-variables-buffer) (insert name "\n"))
             ((not fbp) (set-buffer alcs-symbol-buffer) (insert name "\n"))))))
  (message "Collecting symbols...done"))

(defun anything-lisp-complete-symbol-set-timer (update-period)
  "Update Emacs symbols list when Emacs is idle,
used by `anything-lisp-complete-symbol-set-timer' and `anything-apropos'"
  (run-with-idle-timer update-period t 'alcs-make-candidates))

(defun alcs-init (bufname)
  (declare (special anything-dabbrev-last-target))
  (setq anything-complete-target
        (if (loop for src in (anything-get-sources)
                  thereis (string-match "^dabbrev" (assoc-default 'name src)))
            anything-dabbrev-last-target
          (anything-aif (symbol-at-point) (symbol-name it) "")))
  (anything-candidate-buffer (get-buffer bufname)))

(defun alcs-sort (candidates source)
  (sort candidates #'string<))

(defun alcs-describe-function (name)
  (describe-function (intern name)))
(defun alcs-describe-variable (name)
  (describe-variable (intern name)))
(defun alcs-find-function (name)
  (find-function (intern name)))
(defun alcs-find-variable (name)
  (find-variable (intern name)))

(defvar anything-c-source-complete-emacs-functions
  '((name . "Functions")
    (init . (lambda () (alcs-init alcs-functions-buffer)))
    (prefix-match)
    (candidates-in-buffer . ac-candidates-in-buffer)
    (type . complete-function)))
(defvar anything-c-source-complete-emacs-commands
  '((name . "Commands")
    (init . (lambda () (alcs-init alcs-commands-buffer)))
    (prefix-match)
    (candidates-in-buffer . ac-candidates-in-buffer)
    (type . complete-function)))
(defvar anything-c-source-complete-emacs-variables
  '((name . "Variables")
    (init . (lambda () (alcs-init alcs-variables-buffer)))
    (prefix-match)
    (candidates-in-buffer . ac-candidates-in-buffer)
    (type . complete-variable)))
(defvar anything-c-source-complete-emacs-other-symbols
  '((name . "Other Symbols")
    (init . (lambda () (alcs-init alcs-symbol-buffer)))
    (prefix-match)
    (candidates-in-buffer . ac-candidates-in-buffer)
    (filtered-candidate-transformer . alcs-sort)
    (action . ac-insert)))

(defvar anything-c-source-complete-emacs-functions-partial-match
  '((name . "Functions")
    (init . (lambda () (alcs-init alcs-functions-buffer)))
    (candidates-in-buffer)
    (type . complete-function)))
(defvar anything-c-source-complete-emacs-commands-partial-match
  '((name . "Commands")
    (init . (lambda () (alcs-init alcs-commands-buffer)))
    (candidates-in-buffer)
    (type . complete-function)))
(defvar anything-c-source-complete-emacs-variables-partial-match
  '((name . "Variables")
    (init . (lambda () (alcs-init alcs-variables-buffer)))
    (candidates-in-buffer)
    (type . complete-variable)))

(defvar anything-c-source-apropos-emacs-functions
  '((name . "Apropos Functions")
    (init . (lambda () (alcs-init alcs-functions-buffer)))
    (candidates-in-buffer)
    (requires-pattern . 3)
    (type . apropos-function)))
(defvar anything-c-source-apropos-emacs-commands
  '((name . "Apropos Commands")
    (init . (lambda () (alcs-init alcs-commands-buffer)))
    (candidates-in-buffer)
    (requires-pattern . 3)
    (type . apropos-function)))
(defvar anything-c-source-apropos-emacs-variables
  '((name . "Apropos Variables")
    (init . (lambda () (alcs-init alcs-variables-buffer)))
    (candidates-in-buffer)
    (requires-pattern . 3)
    (type . apropos-variable)))

(defvar anything-c-source-emacs-function-at-point
  '((name . "Function at point")
    (candidates
     . (lambda () (with-current-buffer anything-current-buffer
                    (anything-aif (function-called-at-point)
                        (list (symbol-name it))))))
    (type . apropos-function)))

(defvar anything-c-source-emacs-variable-at-point
  '((name . "Variable at point")
    (candidates
     . (lambda () (with-current-buffer anything-current-buffer
                    (anything-aif (variable-at-point)
                        (unless (equal 0 it) (list (symbol-name it)))))))
    (type . apropos-variable)))

(defvar anything-lisp-complete-symbol-sources
  '(anything-c-source-complete-emacs-commands
    anything-c-source-complete-emacs-functions
    anything-c-source-complete-emacs-variables))

(defvar anything-lisp-complete-symbol-partial-match-sources
  '(anything-c-source-complete-emacs-commands-partial-match
    anything-c-source-complete-emacs-functions-partial-match
    anything-c-source-complete-emacs-variables-partial-match))

(defvar anything-apropos-sources
  '(anything-c-source-apropos-emacs-commands
    anything-c-source-apropos-emacs-functions
    anything-c-source-apropos-emacs-variables))

(add-to-list 'anything-type-attributes
             '(apropos-function
               (filtered-candidate-transformer . alcs-sort)
               (persistent-action . alcs-describe-function)
               (action
                ("Describe Function" . alcs-describe-function)
                ("Find Function" . alcs-find-function))))
(add-to-list 'anything-type-attributes
             '(apropos-variable
               (filtered-candidate-transformer . alcs-sort)
               (persistent-action . alcs-describe-variable)
               (action
                ("Describe Variable" . alcs-describe-variable)
                ("Find Variable" . alcs-find-variable))))
(add-to-list 'anything-type-attributes
             '(complete-function
               (filtered-candidate-transformer . alcs-sort)
               (action . ac-insert)
               (persistent-action . alcs-describe-function)))
(add-to-list 'anything-type-attributes
             '(complete-variable
               (filtered-candidate-transformer . alcs-sort)
               (action . ac-insert)
               (persistent-action . alcs-describe-variable)))

(defun anything-lisp-complete-symbol-1 (update sources input)
  (when (or update (null (get-buffer alcs-variables-buffer)))
    (alcs-make-candidates))
  (let ((anything-lisp-complete-symbol-input-idle-delay anything-input-idle-delay))
    (anything sources input)))

(defun anything-lisp-complete-symbol (update)
  "`lisp-complete-symbol' replacement using `anything'."
  (interactive "P")
  (anything-lisp-complete-symbol-1 update anything-lisp-complete-symbol-sources nil))
(defun anything-lisp-complete-symbol-partial-match (update)
  "`lisp-complete-symbol' replacement using `anything' (partial match)."
  (interactive "P")
  (anything-lisp-complete-symbol-1 update anything-lisp-complete-symbol-partial-match-sources
                                   (anything-aif (symbol-at-point)
                                       (symbol-name it)
                                     "")))
(defun anything-apropos (update)
  "`apropos' replacement using `anything'."
  (interactive "P")
  (anything-lisp-complete-symbol-1 update anything-apropos-sources nil))

(alcs-make-candidates)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  shell history                                                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-complete-shell-history ()
  "Select a command from shell history and insert it."
  (interactive)
  (anything-complete 'anything-c-source-complete-shell-history
                     (or (word-at-point) "")
                     20))
(defvar zsh-p nil)
(defvar anything-c-source-complete-shell-history
  '((name . "Shell History")
    (init . (lambda () (with-current-buffer (anything-candidate-buffer (shell-history-buffer))
                         (revert-buffer t t)
                         (set (make-local-variable 'zsh-p) (shell-history-zsh-extended-history-p)))))
    (get-line . acsh-get-line)
    (search-from-end)
    (type . complete)))

(defun acsh-get-line (s e)
  (let ((extended-history (string= (buffer-substring s (+ s 2)) ": "))
        (single-line (not (string= (buffer-substring (1- e) e) "\\"))))
    (cond ((not zsh-p)
           (buffer-substring s e))
          ((and extended-history single-line)
           (buffer-substring (+ s 15) e))
          (extended-history             ;zsh multi-line / 1st line
           (goto-char e)
           (let ((e2 (1- (if (re-search-forward "^: [0-9]+:[0-9];" nil t)
                             (match-beginning 0)
                           (point-max)))))
             (prog1 (replace-regexp-in-string
                     "\\\\\n" ";" (buffer-substring (+ s 15) e2))
               (goto-char s))))
          (t                   ; zsh multi-line history / not 1st line
           (goto-char s)
           (re-search-backward "^: [0-9]+:[0-9];" nil t)
           (let ((s2 (match-end 0)) e2)
             (goto-char s2)
             (setq e2 (1- (if (re-search-forward "^: [0-9]+:[0-9];" nil t)
                              (match-beginning 0)
                            (point-max))))
             (prog1 (replace-regexp-in-string
                     "\\\\\n" ";" (buffer-substring s2 e2))
               (goto-char s2)))))))


;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "acsh-get-line command")
      (expect "ls"
        (let ((zsh-p t))
          (with-temp-buffer
            (insert ": 1118554690:0;cat ~/.zsh_history\n"
                    ": 1118554690:0;ls\n")
            (forward-line -1)
            (acsh-get-line (point-at-bol) (point-at-eol)))))
      (expect "cd;ls -l"
        (let ((zsh-p t))
          (with-temp-buffer
            (insert ": 1118554690:0;cat ~/.zsh_history\n"
                    ": 1118554690:0;cd\\\n"
                    "ls -l\n"
                    ": 1118554690:0;hoge\n")
            (forward-line -2)
            (acsh-get-line (point-at-bol) (point-at-eol)))))
      (expect "cd;ls -l"
        (let ((zsh-p t))
          (with-temp-buffer
            (insert ": 1118554690:0;cat ~/.zsh_history\n"
                    ": 1118554690:0;cd\\\n"
                    "ls -l\n"
                    ": 1118554690:0;hoge\n")
            (forward-line -3)
            (acsh-get-line (point-at-bol) (point-at-eol)))))
      (expect "cd;ls -l"
        (let ((zsh-p t))
          (with-temp-buffer
            (insert ": 1118554690:0;cat ~/.zsh_history\n"
                    ": 1118554690:0;cd\\\n"
                    "ls -l\n")
            (forward-line -1)
            (acsh-get-line (point-at-bol) (point-at-eol)))))
      (expect "cd;ls -l"
        (let ((zsh-p t))
          (with-temp-buffer
            (insert ": 1118554690:0;cat ~/.zsh_history\n"
                    ": 1118554690:0;cd\\\n"
                    "ls -l\n")
            (forward-line -2)
            (acsh-get-line (point-at-bol) (point-at-eol)))))
      (expect "pwd"
        (let ((zsh-p nil))
          (with-temp-buffer
            (insert "foo\n"
                    "pwd\n")
            (forward-line -1)
            (acsh-get-line (point-at-bol) (point-at-eol)))))
      (desc "acsh-get-line lineno")
      (expect 2
        (let ((zsh-p t))
          (with-temp-buffer
            (insert ": 1118554690:0;cat ~/.zsh_history\n"
                    ": 1118554690:0;cd\\\n"
                    "ls -l\n"
                    ": 1118554690:0;hoge\n")
            (forward-line -2)
            (acsh-get-line (point-at-bol) (point-at-eol))
            (line-number-at-pos))))
      (expect 2
        (let ((zsh-p t))
          (with-temp-buffer
            (insert ": 1118554690:0;cat ~/.zsh_history\n"
                    ": 1118554690:0;cd\\\n"
                    "ls -l\n"
                    ": 1118554690:0;hoge\n")
            (forward-line -3)
            (acsh-get-line (point-at-bol) (point-at-eol))
            (line-number-at-pos))))

      )))

(provide 'anything-complete)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-complete.el")
;;; anything-complete.el ends here
