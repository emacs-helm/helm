;;; helm-bbdb.el --- Helm interface for bbdb

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

(defvar bbdb-records)
(defvar bbdb-buffer-name)

(declare-function bbdb "ext:bbdb-com")
(declare-function bbdb-current-record "ext:bbdb-com")
(declare-function bbdb-redisplay-one-record "ext:bbdb-com")
(declare-function bbdb-record-net "ext:bbdb-com" (string) t)
(declare-function bbdb-current-record "ext:bbdb-com")
(declare-function bbdb-dwim-net-address "ext:bbdb-com")
(declare-function bbdb-records "ext:bbdb-com"
                  (&optional dont-check-disk already-in-db-buffer))

(defun helm-c-bbdb-candidates ()
  "Return a list of all names in the bbdb database.  The format
is \"Firstname Lastname\"."
  (mapcar (lambda (bbdb-record)
            (replace-regexp-in-string
             "\\s-+$" ""
             (concat (aref bbdb-record 0) " " (aref bbdb-record 1))))
          (bbdb-records)))

(defun helm-c-bbdb-create-contact (actions candidate)
  "Action transformer that returns only an entry to add the
current `helm-pattern' as new contact.  All other actions are
removed."
  (if (string= candidate "*Add to contacts*")
      '(("Add to contacts" . (lambda (actions)
                               (bbdb-create-internal
                                (read-from-minibuffer "Name: " helm-c-bbdb-name)
                                (read-from-minibuffer "Company: ")
                                (read-from-minibuffer "Email: ")
                                nil
                                nil
                                (read-from-minibuffer "Note: ")))))
      actions))

(defun helm-c-bbdb-get-record (candidate)
  "Return record that match CANDIDATE."
  (bbdb candidate nil)
  (set-buffer "*BBDB*")
  (bbdb-current-record))

(defvar helm-c-bbdb-name nil
  "Only for internal use.")

(defvar helm-c-source-bbdb
  '((name . "BBDB")
    (candidates . helm-c-bbdb-candidates)
    (action ("Send a mail" . helm-c-bbdb-compose-mail)
     ("View person's data" . helm-c-bbdb-view-person-action))
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (setq helm-c-bbdb-name helm-pattern)
                                        (if (not candidates)
                                            (list "*Add to contacts*")
                                            candidates)))
    (action-transformer . (lambda (actions candidate)
                            (helm-c-bbdb-create-contact actions candidate))))
  "Needs BBDB.

http://bbdb.sourceforge.net/")

(defun helm-c-bbdb-view-person-action (candidate)
  "View BBDB data of single CANDIDATE or marked candidates."
  (helm-aif (helm-marked-candidates)
      (let ((bbdb-append-records (length it)))
        (dolist (i it)
          (bbdb-redisplay-one-record (helm-c-bbdb-get-record i))))
    (bbdb-redisplay-one-record (helm-c-bbdb-get-record candidate))))

(defun helm-c-bbdb-collect-mail-addresses ()
  "Return a list of all mail addresses of records in bbdb buffer."
  (with-current-buffer bbdb-buffer-name
    (loop for i in bbdb-records
          if (bbdb-record-net (car i))
          collect (bbdb-dwim-net-address (car i)))))

(defun helm-c-bbdb-compose-mail (candidate)
  "Compose a mail with all records of bbdb buffer."
  (helm-c-bbdb-view-person-action candidate)
  (let* ((address-list (helm-c-bbdb-collect-mail-addresses))
         (address-str  (mapconcat 'identity address-list ",\n    ")))
    (compose-mail address-str)))

;;;###autoload
(defun helm-bbdb ()
  "Preconfigured `helm' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/"
  (interactive)
  (helm-other-buffer 'helm-c-source-bbdb "*helm bbdb*"))

(provide 'helm-bbdb)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-bbdb ends here
