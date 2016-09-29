;;; helm-imenu.el --- Helm interface for Imenu -*- lexical-binding: t -*-

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
(require 'helm-lib)
(require 'imenu)
(require 'helm-utils)
(require 'helm-help)


(defgroup helm-imenu nil
  "Imenu related libraries and applications for helm."
  :group 'helm)

(defcustom helm-imenu-delimiter " / "
  "Delimit types of candidates and his value in `helm-buffer'."
  :group 'helm-imenu
  :type 'string)

(defcustom helm-imenu-execute-action-at-once-if-one
  #'helm-imenu--execute-action-at-once-p
  "Goto the candidate when only one is remaining."
  :group 'helm-imenu
  :type 'function)

(defcustom helm-imenu-lynx-style-map t
  "Use Arrow keys to jump to occurences."
  :group 'helm-imenu
  :type  'boolean)

(defcustom helm-imenu-all-buffer-assoc nil
  "Major mode association alist for `helm-imenu-in-all-buffers'.
Allow `helm-imenu-in-all-buffers' searching in these associated buffers
even if they are not derived from each other.
The alist is bidirectional, i.e no need to add '((foo . bar) (bar . foo))
only '((foo . bar)) is needed."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'helm-imenu)

;;; keymap
(defvar helm-imenu-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-imenu-next-section)
    (define-key map (kbd "M-<up>")   'helm-imenu-previous-section)
    (when helm-imenu-lynx-style-map
      (define-key map (kbd "<left>")  'helm-maybe-exit-minibuffer)
      (define-key map (kbd "<right>") 'helm-execute-persistent-action))
    (delq nil map)))

(defun helm-imenu-next-or-previous-section (n)
  (with-helm-buffer
    (let* ((fn (lambda ()
                 (car (split-string (helm-get-selection nil t)
                                    helm-imenu-delimiter))))
           (curtype (funcall fn))
           (move-fn (if (> n 0) #'helm-next-line #'helm-previous-line))
           (stop-fn (if (> n 0)
                        #'helm-end-of-source-p
                        #'helm-beginning-of-source-p)))
      (catch 'break
        (while (not (funcall stop-fn))
          (funcall move-fn)
          (unless (string= curtype (funcall fn))
            (throw 'break nil)))))))

(defun helm-imenu-next-section ()
  (interactive)
  (helm-imenu-next-or-previous-section 1))

(defun helm-imenu-previous-section ()
  (interactive)
  (helm-imenu-next-or-previous-section -1))


;;; Internals
(defvar helm-cached-imenu-alist nil)
(make-variable-buffer-local 'helm-cached-imenu-alist)

(defvar helm-cached-imenu-candidates nil)
(make-variable-buffer-local 'helm-cached-imenu-candidates)

(defvar helm-cached-imenu-tick nil)
(make-variable-buffer-local 'helm-cached-imenu-tick)


(defvar helm-source-imenu nil "See (info \"(emacs)Imenu\")")
(defvar helm-source-imenu-all nil)

(defclass helm-imenu-source (helm-source-sync)
  ((candidates :initform 'helm-imenu-candidates)
   (candidate-transformer :initform 'helm-imenu-transformer)
   (persistent-action :initform 'helm-imenu-persistent-action)
   (persistent-help :initform "Show this entry")
   (keymap :initform helm-imenu-map)
   (help-message :initform 'helm-imenu-help-message)
   (action :initform 'helm-imenu-action)))

(defcustom helm-imenu-fuzzy-match nil
  "Enable fuzzy matching in `helm-source-imenu'."
  :group 'helm-imenu
  :type  'boolean
  :set (lambda (var val)
         (set var val)
         (setq helm-source-imenu
               (helm-make-source "Imenu" 'helm-imenu-source
                 :fuzzy-match helm-imenu-fuzzy-match))))

(defun helm-imenu--maybe-switch-to-buffer (candidate)
  (helm-aif (marker-buffer (cdr candidate))
      (switch-to-buffer it)))

(defun helm-imenu--execute-action-at-once-p ()
  (let ((cur (helm-get-selection))
        (mb (with-helm-current-buffer
              (save-excursion
                (goto-char (point-at-bol))
                 (point-marker)))))
    (if (equal (cdr cur) mb)
        (prog1 nil
          (helm-set-pattern "")
          (helm-force-update))
        t)))

(defun helm-imenu-action (candidate)
  "Default action for `helm-source-imenu'."
  (helm-log-run-hook 'helm-goto-line-before-hook)
  (helm-imenu--maybe-switch-to-buffer candidate)
  (imenu candidate)
  ;; If semantic is supported in this buffer
  ;; imenu used `semantic-imenu-goto-function'
  ;; and position have been highlighted,
  ;; no need to highlight again.
  (unless (eq imenu-default-goto-function
              'semantic-imenu-goto-function)
    (helm-highlight-current-line)))

(defun helm-imenu-persistent-action (candidate)
  "Default persistent action for `helm-source-imenu'."
  (helm-imenu--maybe-switch-to-buffer candidate)
  (imenu candidate)
  (helm-highlight-current-line))

(defun helm-imenu-candidates (&optional buffer)
  (with-current-buffer (or buffer helm-current-buffer)
    (let ((tick (buffer-modified-tick)))
      (if (eq helm-cached-imenu-tick tick)
          helm-cached-imenu-candidates
        (setq imenu--index-alist nil)
        (prog1 (setq helm-cached-imenu-candidates
                     (let ((index (imenu--make-index-alist t))) 
                       (helm-imenu--candidates-1
                        (delete (assoc "*Rescan*" index) index))))
          (setq helm-cached-imenu-tick tick))))))

(defun helm-imenu-candidates-in-all-buffers ()
  (let* ((lst (buffer-list))
         (progress-reporter (make-progress-reporter
                             "Imenu indexing buffers..." 1 (length lst))))
    (prog1
        (cl-loop for b in lst
                 for count from 1
                 when
                 (and (with-current-buffer b
                        (derived-mode-p 'prog-mode))
                      (with-current-buffer b
                        (helm-same-major-mode-p helm-current-buffer
                                                helm-imenu-all-buffer-assoc)))
                 do (progress-reporter-update progress-reporter count)
                 and
                 append (with-current-buffer b
                          (helm-imenu-candidates b)))
      (progress-reporter-done progress-reporter))))

(defun helm-imenu--candidates-1 (alist)
  (cl-loop for elm in alist
           nconc (if (imenu--subalist-p elm)
                     (helm-imenu--candidates-1
                      (cl-loop for (e . v) in (cdr elm) collect
                               (cons (propertize
                                      e 'helm-imenu-type (car elm))
                                     ;; If value is an integer, convert it
                                     ;; to a marker, otherwise it is a cons cell
                                     ;; and it will be converted on next recursions.
                                     ;; (Issue #1060) [1]. 
                                     (if (integerp v) (copy-marker v) v))))
                     (and (cdr elm) ; bug in imenu, should not be needed.
                          (setcdr elm (copy-marker (cdr elm))) ; Same as [1].
                          (list elm)))))

(defun helm-imenu--get-prop (item)
  ;; property value of ITEM can have itself
  ;; a property value which have itself a property value
  ;; ...and so on; Return a list of all these
  ;; properties values starting at ITEM.
  (let* ((prop (get-text-property 0 'helm-imenu-type item))
         (lst  (list prop item)))
    (when prop
      (while prop
        (setq prop (get-text-property 0 'helm-imenu-type prop))
        (and prop (push prop lst)))
      lst)))

(defun helm-imenu-transformer (candidates)
  (cl-loop for (k . v) in candidates
        for types = (or (helm-imenu--get-prop k)
                        (list "Function" k))
        for bufname = (buffer-name (marker-buffer v))
        for disp1 = (mapconcat
                     (lambda (x)
                       (propertize
                        x 'face (cond ((string= x "Variables")
                                       'font-lock-variable-name-face)
                                      ((string= x "Function")
                                       'font-lock-function-name-face)
                                      ((string= x "Types")
                                       'font-lock-type-face))))
                     types helm-imenu-delimiter)
        for disp = (propertize disp1 'help-echo bufname)
        collect
        (cons disp (cons k v))))

;;;###autoload
(defun helm-imenu ()
  "Preconfigured `helm' for `imenu'."
  (interactive)
  (unless helm-source-imenu
    (setq helm-source-imenu
          (helm-make-source "Imenu" 'helm-imenu-source
            :fuzzy-match helm-imenu-fuzzy-match)))
  (let ((imenu-auto-rescan t)
        (str (thing-at-point 'symbol))
        (helm-execute-action-at-once-if-one
         helm-imenu-execute-action-at-once-if-one))
    (helm :sources 'helm-source-imenu
          :default (list (concat "\\_<" str "\\_>") str)
          :preselect str
          :buffer "*helm imenu*")))

;;;###autoload
(defun helm-imenu-in-all-buffers ()
  "Preconfigured helm for fetching imenu entries in all buffers with similar mode as current.
A mode is similar as current if it is the same, it is derived i.e `derived-mode-p'
or it have an association in `helm-imenu-all-buffer-assoc'."
  (interactive)
  (unless helm-source-imenu-all
    (setq helm-source-imenu-all
          (helm-make-source "Imenu in all buffers" 'helm-imenu-source
            :candidates 'helm-imenu-candidates-in-all-buffers
            :fuzzy-match helm-imenu-fuzzy-match)))
  (let ((imenu-auto-rescan t)
        (str (thing-at-point 'symbol))
        (helm-execute-action-at-once-if-one
         helm-imenu-execute-action-at-once-if-one))
    (helm :sources 'helm-source-imenu-all
          :default (list (concat "\\_<" str "\\_>") str)
          :preselect (unless (memq 'helm-source-imenu-all
                                   helm-sources-using-default-as-input)
                       str)
          :buffer "*helm imenu all*")))

(provide 'helm-imenu)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-imenu.el ends here
