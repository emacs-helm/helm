;;; helm-misc.el --- Various functions for helm

;; Copyright (C) 2012 ~ 2013 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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


(defgroup helm-misc nil
  "Various Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-time-zone-home-location "Paris"
  "The time zone of your home"
  :group 'helm-misc
  :type 'string)

(defface helm-time-zone-current
    '((t (:foreground "green")))
  "Face used to colorize current time in `helm-world-time'."
  :group 'helm-misc)

(defface helm-time-zone-home
    '((t (:foreground "red")))
  "Face used to colorize home time in `helm-world-time'."
  :group 'helm-misc)


;;; Latex completion
(defun helm-latex-math-candidates ()
  "Collect candidates for latex math completion."
  (declare (special LaTeX-math-menu))
  (loop for i in (cddr LaTeX-math-menu)
        for elm = (loop for s in i when (vectorp s)
                        collect (cons (aref s 0) (aref s 1)))
        append elm))

(defvar helm-source-latex-math
  '((name . "Latex Math Menu")
    (init . (lambda ()
              (with-helm-current-buffer
                (LaTeX-math-mode 1))))
    (candidate-number-limit . 9999)
    (candidates . helm-latex-math-candidates)
    (action . (lambda (candidate)
                (call-interactively candidate)))))


;;;; <Headline Extraction>
(defvar helm-source-fixme
  '((name . "TODO/FIXME/DRY comments")
    (headline . "^.*\\<\\(TODO\\|FIXME\\|DRY\\)\\>.*$")
    (adjust)
    (recenter))
  "Show TODO/FIXME/DRY comments in current file.")

(defvar helm-source-rd-headline
  '((name . "RD HeadLine")
    (headline  "^= \\(.+\\)$" "^== \\(.+\\)$" "^=== \\(.+\\)$" "^==== \\(.+\\)$")
    (condition . (memq major-mode '(rdgrep-mode rd-mode)))
    (migemo)
    (subexp . 1))
  "Show RD headlines.

RD is Ruby's POD.
http://en.wikipedia.org/wiki/Ruby_Document_format")

(defvar helm-source-oddmuse-headline
  '((name . "Oddmuse HeadLine")
    (headline  "^= \\(.+\\) =$" "^== \\(.+\\) ==$"
     "^=== \\(.+\\) ===$" "^==== \\(.+\\) ====$")
    (condition . (memq major-mode '(oddmuse-mode yaoddmuse-mode)))
    (migemo)
    (subexp . 1))
  "Show Oddmuse headlines, such as EmacsWiki.")

(defvar helm-source-emacs-source-defun
  '((name . "Emacs Source DEFUN")
    (headline . "DEFUN\\|DEFVAR")
    (condition . (string-match "/emacs2[0-9].+/src/.+c$"
                  (or buffer-file-name ""))))
  "Show DEFUN/DEFVAR in Emacs C source file.")

(defvar helm-source-emacs-lisp-expectations
  '((name . "Emacs Lisp Expectations")
    (headline . "(desc[ ]\\|(expectations")
    (condition . (eq major-mode 'emacs-lisp-mode)))
  "Show descriptions (desc) in Emacs Lisp Expectations.

http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")

(defvar helm-source-emacs-lisp-toplevels
  '((name . "Emacs Lisp Toplevel / Level 4 Comment / Linkd Star")
    (headline . "^(\\|(@\\*\\|^;;;;")
    (get-line . buffer-substring)
    (condition . (eq major-mode 'emacs-lisp-mode))
    (adjust))
  "Show top-level forms, level 4 comments and linkd stars (optional) in Emacs Lisp.
linkd.el is optional because linkd stars are extracted by regexp.
http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el")


;;; Eev anchors
(defvar helm-source-eev-anchor
  '((name . "Anchors")
    (candidates
     . (lambda ()
         (ignore-errors
           (with-helm-current-buffer
             (loop initially (goto-char (point-min))
                   while (re-search-forward
                          (format ee-anchor-format "\\([^\.].+\\)") nil t)
                   for anchor = (match-string-no-properties 1)
                   collect (cons (format "%5d:%s"
                                         (line-number-at-pos (match-beginning 0))
                                         (format ee-anchor-format anchor))
                                 anchor))))))
    (persistent-action . (lambda (item)
                           (ee-to item)
                           (helm-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . (("Goto link" . ee-to)))))

;;; Jabber Contacts (jabber.el)
(defun helm-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (let (jids)
      (dolist (item (jabber-concat-rosters) jids)
        (when (get item 'connected)
          (push (if (get item 'name)
                    (cons (get item 'name) item)
                    (cons (symbol-name item) item)) jids))))))

(defvar helm-source-jabber-contacts
  '((name . "Jabber Contacts")
    (init . (lambda () (require 'jabber)))
    (candidates . (lambda () (mapcar 'car (helm-jabber-online-contacts))))
    (action . (lambda (x)
                (jabber-chat-with
                 (jabber-read-account)
                 (symbol-name
                  (cdr (assoc x (helm-jabber-online-contacts)))))))))

;;; World time
;;
(defun helm-time-zone-transformer (candidates sources)
  (loop for i in candidates
        collect
        (cond ((string-match (format-time-string "%H:%M" (current-time)) i)
               (propertize i 'face 'helm-time-zone-current))
              ((string-match helm-time-zone-home-location i)
               (propertize i 'face 'helm-time-zone-home))
              (t i))))

(defvar helm-source-time-world
  '((name . "Time World List")
    (init . (lambda ()
              (require 'time)
              (let ((helm-buffer (helm-candidate-buffer 'global)))
                (with-current-buffer helm-buffer
                  (display-time-world-display display-time-world-list)))))
    (candidates-in-buffer)
    (filtered-candidate-transformer . helm-time-zone-transformer)))

;;; LaCarte
;;
(defvar helm-source-lacarte
  '((name . "Lacarte")
    (init . (lambda () (require 'lacarte)))
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (delete '(nil) (lacarte-get-overall-menu-item-alist)))))
    (candidate-number-limit . 9999)
    (action . helm-call-interactively))
  "Needs lacarte.el.

http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el")

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

;; Minibuffer History
;;
;;
(defvar helm-source-minibuffer-history
  '((name . "Minibuffer History")
    (header-name . (lambda (name)
                     (format "%s (%s)" name minibuffer-history-variable)))
    (candidates
     . (lambda ()
         (let ((history (loop for i in
                              (symbol-value minibuffer-history-variable)
                              unless (string= "" i) collect i)))
           (if (consp (car history))
               (mapcar 'prin1-to-string history)
               history))))
    (migemo)
    (multiline)
    (action . (lambda (candidate)
                (delete-minibuffer-contents)
                (insert candidate)))))



;;; Helm ratpoison UI
;;
;;
(defvar helm-source-ratpoison-commands
  '((name . "Ratpoison Commands")
    (init . helm-ratpoison-commands-init)
    (candidates-in-buffer)
    (action ("Execute the command" . helm-ratpoison-commands-execute))
    (display-to-real . helm-ratpoison-commands-display-to-real)
    (candidate-number-limit)))

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
  '((name . "Stumpwm Commands")
    (init . helm-stumpwm-commands-init)
    (candidates-in-buffer)
    (action ("Execute the command" . helm-stumpwm-commands-execute))
    (candidate-number-limit)))

(defun helm-stumpwm-commands-init ()
    (with-current-buffer (helm-candidate-buffer 'global)
      (save-excursion
        (call-process "stumpish" nil (current-buffer) nil "commands"))
      (while (re-search-forward "\\([^ ]+\\) \n?" nil t)
        (replace-match "\\1\n"))
      (goto-char (point-max))))

(defun helm-stumpwm-commands-execute (candidate)
  (call-process "stumpish" nil nil nil  candidate))

;;;###autoload
(defun helm-world-time ()
  "Preconfigured `helm' to show world time."
  (interactive)
  (helm-other-buffer 'helm-source-time-world "*helm world time*"))

;;;###autoload
(defun helm-insert-latex-math ()
  "Preconfigured helm for latex math symbols completion."
  (interactive)
  (helm-other-buffer 'helm-source-latex-math "*helm latex*"))

;;;###autoload
(defun helm-eev-anchors ()
  "Preconfigured `helm' for eev anchors."
  (interactive)
  (helm-other-buffer 'helm-source-eev-anchor "*Helm eev anchors*"))

;;;###autoload
(defun helm-ratpoison-commands ()
  "Preconfigured `helm' to execute ratpoison commands."
  (interactive)
  (helm-other-buffer 'helm-source-ratpoison-commands
                     "*helm ratpoison commands*"))

;;;###autoload
(defun helm-stumpwm-commands()
  (interactive)
  (helm-other-buffer 'helm-source-stumpwm-commands
                     "*helm stumpwm commands*"))


;;;###autoload
(defun helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(helm-source-buffers-list
                       helm-source-recentf
                       helm-source-buffer-not-found)
                     "*helm mini*"))

;;;###autoload
(defun helm-minibuffer-history ()
  "Preconfigured `helm' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (helm-other-buffer 'helm-source-minibuffer-history
                       "*helm minibuffer-history*")))

(provide 'helm-misc)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-misc.el ends here
