;;; helm-misc.el --- Various functions for helm

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
(require 'helm-buffers)
(require 'helm-files)


(defgroup helm-misc nil
  "Various Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-minibuffer-history-key "C-r"
  "The key `helm-minibuffer-history' is bound to in minibuffer local maps."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :group 'helm-config
  :set
  (lambda (var key)
    (dolist (map '(minibuffer-local-completion-map
                   minibuffer-local-filename-completion-map
                   minibuffer-local-filename-must-match-map ; Emacs 23.1.+
                   minibuffer-local-isearch-map
                   minibuffer-local-map
                   minibuffer-local-must-match-filename-map ; Older Emacsen
                   minibuffer-local-must-match-map
                   minibuffer-local-ns-map))
      (when (and (boundp map) (keymapp (symbol-value map)))
        (when (and (boundp var) (symbol-value var))
          (define-key (symbol-value map)
            (read-kbd-macro (symbol-value var)) nil))
        (when key
          (define-key (symbol-value map)
            (read-kbd-macro key) 'helm-minibuffer-history))))
    (set var key)))


;;; Tracker desktop search
(defvar helm-c-source-tracker-search
  '((name . "Tracker Search")
    (candidates . (lambda ()
                    (start-process "tracker-search-process" nil
                                   "tracker-search"
                                   helm-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")

;;; Spotlight (MacOS X desktop search)
(defvar helm-c-source-mac-spotlight
  '((name . "mdfind")
    (candidates
     . (lambda () (start-process "mdfind-process" nil "mdfind" helm-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")

;;; Picklist
(defvar helm-c-source-picklist
  '((name . "Picklist")
    (candidates . (lambda () (mapcar 'car picklist-list)))
    (type . file)))


;;; Latex completion
(defun helm-c-latex-math-candidates ()
  "Collect candidates for latex math completion."
  (declare (special LaTeX-math-menu))
  (loop for i in (cddr LaTeX-math-menu)
        for elm = (loop for s in i when (vectorp s)
                        collect (cons (aref s 0) (aref s 1)))
        append elm))

(defvar helm-c-source-latex-math
  '((name . "Latex Math Menu")
    (init . (lambda ()
              (with-helm-current-buffer
                (LaTeX-math-mode 1))))
    (candidate-number-limit . 9999)
    (candidates . helm-c-latex-math-candidates)
    (action . (lambda (candidate)
                (call-interactively candidate)))))


;;;; <Headline Extraction>
(defvar helm-c-source-fixme
  '((name . "TODO/FIXME/DRY comments")
    (headline . "^.*\\<\\(TODO\\|FIXME\\|DRY\\)\\>.*$")
    (adjust)
    (recenter))
  "Show TODO/FIXME/DRY comments in current file.")

(defvar helm-c-source-rd-headline
  '((name . "RD HeadLine")
    (headline  "^= \\(.+\\)$" "^== \\(.+\\)$" "^=== \\(.+\\)$" "^==== \\(.+\\)$")
    (condition . (memq major-mode '(rdgrep-mode rd-mode)))
    (migemo)
    (subexp . 1))
  "Show RD headlines.

RD is Ruby's POD.
http://en.wikipedia.org/wiki/Ruby_Document_format")

(defvar helm-c-source-oddmuse-headline
  '((name . "Oddmuse HeadLine")
    (headline  "^= \\(.+\\) =$" "^== \\(.+\\) ==$"
     "^=== \\(.+\\) ===$" "^==== \\(.+\\) ====$")
    (condition . (memq major-mode '(oddmuse-mode yaoddmuse-mode)))
    (migemo)
    (subexp . 1))
  "Show Oddmuse headlines, such as EmacsWiki.")

(defvar helm-c-source-emacs-source-defun
  '((name . "Emacs Source DEFUN")
    (headline . "DEFUN\\|DEFVAR")
    (condition . (string-match "/emacs2[0-9].+/src/.+c$"
                  (or buffer-file-name ""))))
  "Show DEFUN/DEFVAR in Emacs C source file.")

(defvar helm-c-source-emacs-lisp-expectations
  '((name . "Emacs Lisp Expectations")
    (headline . "(desc[ ]\\|(expectations")
    (condition . (eq major-mode 'emacs-lisp-mode)))
  "Show descriptions (desc) in Emacs Lisp Expectations.

http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")

(defvar helm-c-source-emacs-lisp-toplevels
  '((name . "Emacs Lisp Toplevel / Level 4 Comment / Linkd Star")
    (headline . "^(\\|(@\\*\\|^;;;;")
    (get-line . buffer-substring)
    (condition . (eq major-mode 'emacs-lisp-mode))
    (adjust))
  "Show top-level forms, level 4 comments and linkd stars (optional) in Emacs Lisp.
linkd.el is optional because linkd stars are extracted by regexp.
http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el")


;;; Eev anchors
(defvar helm-c-source-eev-anchor
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
(defun helm-c-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (let (jids)
      (dolist (item (jabber-concat-rosters) jids)
        (when (get item 'connected)
          (push (if (get item 'name)
                    (cons (get item 'name) item)
                    (cons (symbol-name item) item)) jids))))))

(defvar helm-c-source-jabber-contacts
  '((name . "Jabber Contacts")
    (init . (lambda () (require 'jabber)))
    (candidates . (lambda () (mapcar 'car (helm-c-jabber-online-contacts))))
    (action . (lambda (x)
                (jabber-chat-with
                 (jabber-read-account)
                 (symbol-name
                  (cdr (assoc x (helm-c-jabber-online-contacts)))))))))

;;; World time
;;
(defvar helm-c-source-time-world
  '((name . "Time World List")
    (init . (lambda ()
              (let ((helm-buffer (helm-candidate-buffer 'global)))
                (with-current-buffer helm-buffer
                  (display-time-world-display display-time-world-list)))))
    (candidates-in-buffer)))

;;; LaCarte
;;
(defvar helm-c-source-lacarte
  '((name . "Lacarte")
    (init . (lambda () (require 'lacarte)))
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (delete '(nil) (lacarte-get-overall-menu-item-alist)))))
    (candidate-number-limit . 9999)
    (action . helm-c-call-interactively))
  "Needs lacarte.el.

http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el")

(defun helm-c-call-interactively (cmd-or-name)
  "Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`helm-current-prefix-arg' is used as the command's prefix argument."
  (setq extended-command-history
        (cons (helm-c-stringify cmd-or-name)
              (delete (helm-c-stringify cmd-or-name) extended-command-history)))
  (let ((current-prefix-arg helm-current-prefix-arg)
        (cmd (helm-c-symbolify cmd-or-name)))
    (if (stringp (symbol-function cmd))
        (execute-kbd-macro (symbol-function cmd))
        (setq this-command cmd)
        (call-interactively cmd))))

;; Minibuffer History
;;
;;
(defvar helm-c-source-minibuffer-history
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
    (action . (lambda (candidate)
                (delete-minibuffer-contents)
                (insert candidate)))))



;;; Helm ratpoison UI
;;
;;
(defvar helm-c-source-ratpoison-commands
  '((name . "Ratpoison Commands")
    (init . helm-c-ratpoison-commands-init)
    (candidates-in-buffer)
    (action ("Execute the command" . helm-c-ratpoison-commands-execute))
    (display-to-real . helm-c-ratpoison-commands-display-to-real)
    (candidate-number-limit)))

(defun helm-c-ratpoison-commands-init ()
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

(defun helm-c-ratpoison-commands-display-to-real (display)
  (and (string-match ": " display)
       (substring display (match-end 0))))

(defun helm-c-ratpoison-commands-execute (candidate)
  (call-process "ratpoison" nil nil nil "-ic" candidate))

;;;###autoload
(defun helm-world-time ()
  "Preconfigured `helm' to show world time."
  (interactive)
  (helm-other-buffer 'helm-c-source-time-world "*helm world time*"))

;;;###autoload
(defun helm-c-insert-latex-math ()
  "Preconfigured helm for latex math symbols completion."
  (interactive)
  (helm-other-buffer 'helm-c-source-latex-math "*helm latex*"))

;;;###autoload
(defun helm-eev-anchors ()
  "Preconfigured `helm' for eev anchors."
  (interactive)
  (helm-other-buffer 'helm-c-source-eev-anchor "*Helm eev anchors*"))

;;;###autoload
(defun helm-ratpoison-commands ()
  "Preconfigured `helm' to execute ratpoison commands."
  (interactive)
  (helm-other-buffer 'helm-c-source-ratpoison-commands
                     "*helm ratpoison commands*"))

;;;###autoload
(defun helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (helm-other-buffer '(helm-c-source-buffers-list
                       helm-c-source-recentf
                       helm-c-source-buffer-not-found)
                     "*helm mini*"))

;;;###autoload
(defun helm-minibuffer-history ()
  "Preconfigured `helm' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (helm-other-buffer 'helm-c-source-minibuffer-history
                       "*helm minibuffer-history*")))

(provide 'helm-misc)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-misc.el ends here
