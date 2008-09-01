;;; anything-complete.el --- completion with anything
;; $Id: anything-complete.el,v 1.2 2008-09-01 22:27:45 rubikitch Exp $

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

;; 

;;; History:

;; $Log: anything-complete.el,v $
;; Revision 1.2  2008-09-01 22:27:45  rubikitch
;; *** empty log message ***
;;
;; Revision 1.1  2008/09/01 22:23:55  rubikitch
;; Initial revision
;;

;;; Code:

(defvar anything-complete-version "$Id: anything-complete.el,v 1.2 2008-09-01 22:27:45 rubikitch Exp $")
(eval-when-compile (require 'cl))
(require 'anything-match-plugin)

(defun anything-compile-source--complete (source)
  (if (assoc 'complete source)
      (append source
              `((candidates-in-buffer
                 . (lambda ()
                     (let ((anything-pattern
                            (if (equal "" anything-complete-target)
                                anything-pattern
                              (concat anything-complete-target " " anything-pattern))))
                       (anything-candidates-in-buffer))))
                (action . anything-complete-insert)))
    source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--complete)

(defun anything-complete-insert (candidate)
  (let ((pt (point)))
    (when (and (search-backward anything-complete-target nil t)
               (string= (buffer-substring (point) pt) anything-complete-target))
      (delete-region (point) pt)))
  (insert candidate))

(require 'thingatpt)
(defun anything-complete-shell-history ()
  (interactive)
  (anything-complete 'anything-c-source-complete-shell-history
                     (or (word-at-point) "")
                     20))

(defvar anything-c-source-complete-shell-history
  '((name . "Shell History")
    (complete)
    (init . (lambda () (with-current-buffer (anything-candidate-buffer (shell-history-buffer))
                         (revert-buffer t t)
                         (set (make-local-variable 'zsh-p) (shell-history-zsh-extended-history-p)))))
    (get-line . acsh-get-line)
    (search-from-end)
    (requires-pattern)
    ))
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


;(with-current-buffer "test" (ff 123 146))
;
; TODO anything-candidates-in-buffer from bottom
;;makepkg
;;;: 1123867258:0;cat <<EOR > Rakefile\
;;;# $Id: anything-complete.el,v 1.2 2008-09-01 22:27:45 rubikitch Exp $\
;;;require 'rake/makepkg'\
;;;EOR


;; (anything-test-candidates 'anything-c-source-complete-shell-history "byte-compile")
;;(with-current-buffer (shell-history-buffer) (shell-history-zsh-extended-history-p))
;; (anything-compile-sources (list anything-c-source-complete-shell-history) '(anything-compile-source--complete))
(defun anything-complete (sources target &optional limit idle-delay input-idle-delay)
  (let ((anything-candidate-number-limit (or limit anything-candidate-number-limit))
        (anything-idle-delay (or idle-delay anything-idle-delay))
        (anything-input-idle-delay (or input-idle-delay anything-input-idle-delay))
        (anything-complete-target target))
    (anything sources)))

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
