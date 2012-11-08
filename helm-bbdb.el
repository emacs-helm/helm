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
(require 'helm-mode)

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
(declare-function bbdb-label-completion-list "ext:bbdb" (field))

(defgroup helm-bbdb nil
  "Commands and function for bbdb."
  :group 'helm)

(defun helm-c-bbdb-candidates ()
  "Return a list of all names in the bbdb database.
The format is \"Firstname Lastname\"."
  (mapcar (lambda (bbdb-record)
            (replace-regexp-in-string
             "\\s-+$" ""
             (concat (aref bbdb-record 0) " " (aref bbdb-record 1))))
          (bbdb-records)))

(defun helm-bbdb-read-phone ()
  "Return a list of vector address objects.
See docstring of `bbdb-create-internal' for more info on address entries."
  (loop with phone-list
        with loc-list = (cons "[Exit when no more]"
                              (bbdb-label-completion-list "phones"))
        with loc ; Defer count
        do (setq loc (helm-comp-read (format "Phone location[%s]: " count)
                                     loc-list
                                     :must-match 'confirm
                                     :default ""))
        while (not (string= loc "[Exit when no more]"))
        for count from 1
        for phone-number = (read-string (format "Phone number (%s): " loc))
        collect (vector loc phone-number) into phone-list
        do (setq loc-list (remove loc loc-list))
        finally return phone-list))

;; TODO move this to helm-utils when finish
(defun helm-read-repeat-string (prompt &optional count)
  "Prompt as many time PROMPT is not empty.
If COUNT is non--nil add a number after each prompt."
  (loop with elm with new-prompt = prompt
        while (not (string= elm ""))
        for n from 1
        do (when count
             (setq new-prompt (concat prompt (int-to-string n) ": ")))
        collect (setq elm (read-string new-prompt)) into lis
        finally return (remove "" lis)))

(defun helm-bbdb-read-address ()
  "Return a list of vector address objects.
See docstring of `bbdb-create-internal' for more info on address entries."
  (loop with address-list
        with loc-list = (cons "[Exit when no more]"
                              (bbdb-label-completion-list "addresses"))
        with loc ; Defer count
        do (setq loc (helm-comp-read
                      (format "Address description[%s]: "
                              (int-to-string count))
                      loc-list
                      :must-match 'confirm
                      :default ""))
        while (not (string= loc "[Exit when no more]"))
        for count from 1
        ;; Create vector
        for lines =  (helm-read-repeat-string "Line" t)
        for city = (read-string "City: ")
        for state = (read-string "State: ")
        for zip = (read-string "ZipCode: ")
        for country = (read-string "Country: ")
        collect (vector loc lines city state zip country) into address-list
        do (setq loc-list (remove loc loc-list))
        finally return address-list))

(defun helm-c-bbdb-create-contact (actions candidate)
  "Action transformer for `helm-c-source-bbdb'.
Returns only an entry to add the current `helm-pattern' as new contact.
All other actions are removed."
  (if (string= candidate "*Add to contacts*")
      '(("Add to contacts"
         . (lambda (actions)
             (bbdb-create-internal
              (read-from-minibuffer "Name: " helm-c-bbdb-name)
              (read-from-minibuffer "Company: ")
              (helm-read-repeat-string "Email " t)
              (helm-bbdb-read-address)
              (helm-bbdb-read-phone)
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
    (action . (("Send a mail" . helm-c-bbdb-compose-mail)
               ("View person's data" . helm-c-bbdb-view-person-action)))
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
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-bbdb ends here
