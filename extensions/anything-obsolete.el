;;;; anything-obsolete.el --- obsolete functions of anything
;; Time-stamp: <2010-11-18 11:02:25 rubikitch>

;; Copyright (C) 2010  rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: tools, files
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-obsolete.el

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;; Obsolete functions and commands are moved here.
;; 
;; - `read-file-name' and `find-file' replacement
;;     There are here because anything-ized `read-file-name' slows down and has odd interface
;;     If you still want to use them, add to ~/.emacs like this:
;;       (anything-read-string-mode '(string file buffer variable command))
;;     

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-read-file-name-follow-directory'
;;    Follow directory in `anything-read-file-name'.
;;  `anything-find-file'
;;    Replacement of `find-file'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;;; Change log:
;;
;;  Change log of this file is found at
;;  http://repo.or.cz/w/anything-config.git/history/master:/anything-obsolete.el
;;
;;  Change log of this project is found at
;;  http://repo.or.cz/w/anything-config.git?a=shortlog

;;; Code:

(require 'anything)

;; (@* "`read-file-name' compatible read function ")
(defvar anything-read-file-name-map nil)
(defvar arfn-followed nil)
(defvar arfn-dir nil)
(defun anything-read-file-name-map ()
  "Lazy initialization of `anything-read-file-name-map'."
  (unless anything-read-file-name-map
    (setq anything-read-file-name-map (copy-keymap anything-map))
    (define-key anything-read-file-name-map "\C-i" 'anything-read-file-name-follow-directory)
    (define-key anything-read-file-name-map [tab] 'anything-read-file-name-follow-directory))
  anything-read-file-name-map)

(defun anything-read-file-name-follow-directory ()
  "Follow directory in `anything-read-file-name'."
  (interactive)
  ;; These variables are bound by `arfn-sources' or `anything-find-file'.
  (declare (special prompt default-filename require-match predicate additional-attrs))
  (setq arfn-followed t)
  (let* ((sel (anything-get-selection))
         (f (expand-file-name sel arfn-dir)))
    (cond ((and (file-directory-p f) (not (string-match "/\\.$" sel)))
           (with-selected-window (minibuffer-window) (delete-minibuffer-contents))
           (setq anything-pattern "")
           ;;(setq arfn-dir f)
           (anything-set-sources
            (arfn-sources
             prompt f default-filename require-match nil predicate additional-attrs))
           (anything-update))
          ((string-match "^\\(.+\\)/\\([^/]+\\)$" sel)
           (with-selected-window (minibuffer-window)
             (delete-minibuffer-contents)
             (insert (match-string 2 sel)))
           (anything-set-sources
            (arfn-sources
             prompt (expand-file-name (match-string 1 sel) arfn-dir) nil require-match (match-string 2 sel) predicate additional-attrs))
           (anything-update)))))

(defun* anything-read-file-name (prompt &optional dir default-filename require-match initial-input predicate (additional-attrs '((action . identity))))
  "`anything' replacement for `read-file-name'."
  (setq arfn-followed nil)
  (let* ((anything-map (anything-read-file-name-map))
         anything-input-idle-delay
         (result (or (anything-noresume (arfn-sources
                                         prompt dir default-filename require-match
                                         initial-input predicate additional-attrs)
                                        initial-input prompt nil nil "*anything complete*")
                     (keyboard-quit))))
    (when (and require-match
               (not (and (file-exists-p result)
                         (funcall (or predicate 'identity) result))))
      (error "anything-read-file-name: file `%s' is not matched" result))
    (when (stringp result)
      (prog1 result
        (add-to-list 'file-name-history result)
        (setq file-name-history (cons result (delete result file-name-history)))))))

(defun arfn-candidates (dir)
  (if (file-directory-p dir)
      (loop for (f _ _ _ _ _ _ _ _ perm _ _ _) in (directory-files-and-attributes dir t)
            for basename = (file-name-nondirectory f)
            when (string= "d" (substring perm 0 1))
            collect (cons (concat basename "/") f)
            else collect (cons basename f))))

(defun* arfn-sources (prompt dir default-filename require-match initial-input predicate &optional (additional-attrs '((action . identity))))
  (setq arfn-dir dir)
  (let* ((dir (or dir default-directory))
         (transformer-func
          (if predicate
              `(candidate-transformer
                . (lambda (cands)
                    (remove-if-not
                     (lambda (c) (,predicate (if (consp c) (cdr c) c))) cands)))))
         (new-input-source (ac-new-input-source
                            prompt nil
                            (append '((display-to-real . (lambda (f) (expand-file-name f arfn-dir))))
                                    additional-attrs)))
         (history-source (unless require-match
                           `((name . "History")
                             (candidates . file-name-history)
                             (persistent-action . find-file)
                             ,@additional-attrs))))
    `(((name . "Default")
       (candidates . ,(if default-filename (list default-filename)))
       (persistent-action . find-file)
       (filtered-candidate-transformer
        . (lambda (cands source)
            (if (and (not arfn-followed) (string= anything-pattern "")) cands nil)))
       (display-to-real . (lambda (f) (expand-file-name f ,dir)))
       ,@additional-attrs)
      ((name . ,dir)
       (candidates . (lambda () (arfn-candidates ,dir)))
       (persistent-action . find-file)
       ,@additional-attrs
       ,transformer-func)
      ,new-input-source
      ,history-source)))
;; (anything-read-file-name "file: " "~" ".emacs")
;; (anything-read-file-name "file: " "~" ".emacs" t)
;; (anything-read-file-name "file: " "~" )
;; (anything-read-file-name "file: ")
;; (read-file-name "file: " "/tmp")


;;; (@* "find-file compatible command")
(defvar anything-find-file-additional-sources nil)
(defun anything-find-file ()
  "Replacement of `find-file'."
  (interactive)
  (let ((anything-map (anything-read-file-name-map))
        ;; anything-read-file-name-follow-directory uses these variables
        (prompt "Find File: ")
        default-filename require-match predicate
        (additional-attrs '(;; because anything-c-skip-boring-files cannot
                            ;; handle (display . real) candidates
                            (candidate-transformer)
                            (type . file))))
    (anything-other-buffer (append (arfn-sources prompt default-directory
                                                 nil nil nil nil additional-attrs)
                                   anything-find-file-additional-sources)
                           "*anything find-file*")))
;;(anything-find-file)



(provide 'anything-obsolete)

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything-obsolete.el"))
;;; anything-obsolete.el ends here
