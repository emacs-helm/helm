;;; anything-config.el --- predefined configurations for anything

;; Copyright (C) 2007 Tassilo Horn

;; Maintainer: Tassilo Horn <tassilo@member.fsf.org>

;; Contributors:
;;     Tamas Patrovics
;;     Tassilo Horn <tassilo@member.fsf.org>
;;     Vagn Johansen <gonz808@hotmail.com>
;;     Mathias Dahl <mathias.dahl@gmail.com>
;;     Bill Clementson <billclem@gmail.com>
;;     Stefan Kamphausen <ska@skamphausen.de>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; This package provides predefined configurations for anything.el
;; You can pick the ones you like and use in your own configuration
;; like this:
;;
;;   (require 'anything-config) ; loads anything.el too
;;
;;   (setq anything-sources (list anything-c-source-buffers
;;                                anything-c-source-emacs-commands
;;                                anything-c-source-locate ...))
;;
;;   (setq anything-type-actions (list anything-c-actions-buffer
;;                                     anything-c-actions-file
;;                                     anything-c-actions-command
;;                                     anything-c-actions-function
;;                                     anything-c-actions-sexp ...))
;;
;;   (setq anything-action-transformers
;;         '((buffer   . anything-c-transform-buffer-actions)
;;           (file     . anything-c-transform-file-actions)
;;           (command  . anything-c-transform-command-actions)
;;           (function . anything-c-transform-function-actions)
;;           (sexp     . anything-c-transform-sexp-actions)))
;;
;;   (setq anything-candidate-transformers
;;         '((buffer   . anything-c-transform-buffers)
;;           (file     . anything-c-transform-files)
;;           (command  . anything-c-transform-commands)
;;           (function . anything-c-transform-functions)
;;           (sexp     . anything-c-transform-sexps)))
;;

;;; Startup

(eval-after-load 'anything-config
  '(require 'anything))

;;; Version

(defvar anything-c-version "<2007-08-05 Sun 18:06>"
  "The version of anything-config.el, or better the date of the
last change.")

;;; Keymaps

(defvar anything-c-use-standard-keys t
  "If non-nil the keybindings of anything will be the standard
bindings used in most parts of emacs, e.g. M-p/M-n for minibuffer
history, C-s for isearch, etc.

If it's nil anything uses some bindings that don't conflict with
`iswitchb', e.g. C-p/C-n for the minibuffer history.  If you use
`iswitchb' you probably should say nil here.

This variable has to be set _before_ you require
anything-config.")

(when anything-c-use-standard-keys
  (defvar anything-map
    (let ((map (copy-keymap minibuffer-local-map)))
      (define-key map (kbd "<down>")  'anything-next-line)
      (define-key map (kbd "<up>")    'anything-previous-line)
      (define-key map (kbd "C-n")     'anything-next-line)
      (define-key map (kbd "C-p")     'anything-previous-line)
      (define-key map (kbd "<prior>") 'anything-previous-page)
      (define-key map (kbd "<next>")  'anything-next-page)
      (define-key map (kbd "M-v")     'anything-previous-page)
      (define-key map (kbd "C-v")     'anything-next-page)
      (define-key map (kbd "<right>") 'anything-next-source)
      (define-key map (kbd "<left>")  'anything-previous-source)
      (define-key map (kbd "<RET>")   'anything-exit-minibuffer)
      (define-key map (kbd "C-1")     'anything-select-with-digit-shortcut)
      (define-key map (kbd "C-2")     'anything-select-with-digit-shortcut)
      (define-key map (kbd "C-3")     'anything-select-with-digit-shortcut)
      (define-key map (kbd "C-4")     'anything-select-with-digit-shortcut)
      (define-key map (kbd "C-5")     'anything-select-with-digit-shortcut)
      (define-key map (kbd "C-6")     'anything-select-with-digit-shortcut)
      (define-key map (kbd "C-7")     'anything-select-with-digit-shortcut)
      (define-key map (kbd "C-8")     'anything-select-with-digit-shortcut)
      (define-key map (kbd "C-9")     'anything-select-with-digit-shortcut)
      (define-key map (kbd "<tab>")   'anything-select-action)
      (defalias 'anything-next-history-element     'next-history-element)
      (defalias 'anything-previous-history-element 'previous-history-element)
      (define-key map (kbd "M-p")     'anything-previous-history-element)
      (define-key map (kbd "M-n")     'anything-next-history-element)
      (define-key map (kbd "C-s")     'anything-isearch)
      map)
    "Keymap for anything.")

  (defvar anything-isearch-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<return>")    'anything-isearch-default-action)
      (define-key map (kbd "<tab>")       'anything-isearch-select-action)
      (define-key map (kbd "C-g")         'anything-isearch-cancel)
      (define-key map (kbd "C-s")         'anything-isearch-again)
      (define-key map (kbd "<backspace>") 'anything-isearch-delete)
      ;; add printing chars
      (let ((i ?\s))
        (while (< i 256)
          (define-key map (vector i) 'anything-isearch-printing-char)
          (setq i (1+ i))))
      map)
    "Keymap for anything incremental search."))

;;; Sources

;;;; Match functions

(defun anything-c-match-on-file-name (candidate)
  "Return non-nil if `anything-pattern' matches the
filename (without directory part) of CANDIDATE."
  (string-match anything-pattern (file-name-nondirectory candidate)))

(defun anything-c-match-on-directory-name (candidate)
  "Return non-nil if `anything-pattern' matches the directory
part of CANDIDATE (a file)."
  (let ((dir (file-name-directory candidate)))
    (when dir
      (string-match anything-pattern dir))))

(defun anything-c-string-match (candidate)
  "Return non-nil if `anything-pattern' matches CANDIDATE.
The match is done with `string-match'."
  (string-match anything-pattern candidate))

;;;; Buffers

(defun anything-c-buffer-list ()
  "Return the list of names of buffers with the `anything-buffer'
and hidden buffers filtered out.  The first buffer in the list
will be the last recently used buffer that is not the current
buffer."
  (let ((buffers (remove-if (lambda (name)
                              (or (equal name anything-buffer)
                                  (eq ?\  (aref name 0))))
                            (mapcar 'buffer-name (buffer-list)))))
    (append (cdr buffers) (list (car buffers)))))

(defvar anything-c-source-buffers
  '((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (type . buffer)))

;;;; File name history

(defvar anything-c-source-file-name-history
  '((name . "File Name History")
    (candidates . file-name-history)
    (match . (anything-c-match-on-file-name
              anything-c-match-on-directory-name))
    (type . file)))

;;;; Recentf files

(defvar anything-c-source-recentf
  '((name . "Recentf")
    (candidates . recentf-list)
    (match . (anything-c-match-on-file-name
              anything-c-match-on-directory-name))
    (type . file))
  "See (info \"(emacs)File Conveniences\").")

;;;; Files in current dir

(defvar anything-c-source-files-in-current-dir
  '((name . "Files from Current Directory")
    (init-func . (lambda ()
                   (setq anything-c-default-directory
                         default-directory)))
    (candidates . (lambda ()
                    (directory-files
                     anything-c-default-directory)))
    (type . file)))

;;;; Man Pages

(defvar anything-c-source-man-pages
  `((name . "Manual Pages")
    (candidates . ,(progn
                     (when (require 'woman nil t)
                       (woman-file-name "")
                       (sort (mapcar 'car
                                     woman-topic-all-completions)
                             'string-lessp))))
    (action . (("Show with Woman" . woman)))
    (requires-pattern . 2)))

;;;; Info pages

(defvar anything-c-source-info-pages
  `((name . "Info Pages")
    (candidates . ,(save-window-excursion
                     (save-excursion
                       (require 'info)
                       (Info-find-node "dir" "top")
                       (goto-char (point-min))
                       (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
                             topics)
                         (while (re-search-forward info-topic-regexp nil t)
                           (add-to-list 'topics (match-string-no-properties 1)))
                         (goto-char (point-min))
                         topics))))
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string "^[^:]+: "
                                                                    ""
                                                                    node-str))))))
    (requires-pattern . 2)))

;;;; Complex command history

(defvar anything-c-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda ()
                    (mapcar 'prin1-to-string
                            command-history)))
    (type . sexp)))

;;;; Emacs commands

(defvar anything-c-source-emacs-commands
  '((name . "Emacs Commands")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (commandp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (type . command)
    (requires-pattern . 2))
  "Source for completing and invoking Emacs commands.  A command
is a function with interactive spec that can be invoked with
`M-x'.

To get non-interactive functions listed, use
`anything-c-source-emacs-functions'.")

;;;; Emacs functions

;;;;; Normal

(defvar anything-c-source-emacs-functions
  '((name . "Emacs Functions")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (functionp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (type . function)
    (requires-pattern . 2))
  "Source for completing Emacs functions.")

;;;;; With abbrev expansion similar to my exec-abbrev-cmd.el

(defvar anything-c-function-abbrev-regexp nil
  "Regexp built from the current `anything-pattern' interpreting
it as abbreviation.  Only for internal use.")

(defun anything-c-match-function-by-abbrev (candidate)
  "Return non-nil if `anything-pattern' is an abbreviation of the
function CANDIDATE.

Abbreviations are made by taking the first character from each
word in the function's name, e.g. \"bb\" is an abbrev for
`bury-buffer', \"stb\" is an abbrev for `switch-to-buffer'."
  (string-match anything-c-function-abbrev-regexp candidate))

(defvar anything-c-source-emacs-functions-with-abbrevs
  (append anything-c-source-emacs-functions
          '((match . (anything-c-match-function-by-abbrev
                      anything-c-string-match)))
          '((init-func
             .
             (lambda ()
               (defadvice anything-update
                 (before anything-c-update-function-abbrev-regexp activate)
                 (let ((char-list (append anything-pattern nil))
                       (str "^"))
                   (dolist (c char-list)
                     (setq str (concat str (list c) "[^-]*-")))
                   (setq str (concat (substring str 0 (1- (length str))) "$"))
                   (setq anything-c-function-abbrev-regexp str))))))))

;;;; Bookmarks

(defvar anything-c-source-bookmarks
  '((name . "Bookmarks")
    (candidates . bookmark-all-names)
    (action . (("Jump to Bookmark" . bookmark-jump))))
  "See (info \"(emacs)Bookmarks\").")


;;;; Picklist

(defvar anything-c-source-picklist
  '((name . "Picklist")
    (candidates . (lambda ()
                    (mapcar 'car picklist-list)))
    (type . file)))

;;;; Imenu

(defvar anything-c-source-imenu
  '((name . "Imenu")
    (init-func . (lambda ()
                   (setq anything-c-imenu-current-buffer
                         (current-buffer))))
    (candidates . (lambda ()
                    (condition-case nil
                        (with-current-buffer anything-c-imenu-current-buffer
                          (mapcar (lambda (x)
                                    (cons (car x) x))
                                  ;; leave only top level completions for
                                  ;; simplicity (could be more sophisticated)
                                  (remove-if-not (lambda (x)
                                                   (markerp (cdr x)))
                                                 (imenu--make-index-alist))))
                      (error nil))))
    (action . imenu)))

;;;; File Cache

(defvar anything-c-source-file-cache-initialized nil)

(defvar file-cache-files nil)

(defvar anything-c-source-file-cache
  '((name . "File Cache")
    (init-func . (lambda ()
                   (unless anything-c-source-file-cache-initialized
                     (setq file-cache-files
                           (loop for item in file-cache-alist append
                                 (destructuring-bind (base &rest dirs) item
                                   (loop for dir in dirs collect
                                         (concat dir base)))))
                     (defadvice file-cache-add-file (after file-cache-list activate)
                       (add-to-list 'file-cache-files (expand-file-name file)))
                     (setq anything-c-source-file-cache-initialized t))))
    (candidates . file-cache-files)
    (match . (anything-c-match-on-file-name
              anything-c-match-on-directory-name))
    (type . file)))

;;;; Locate

(defvar anything-c-locate-options (if (eq system-type 'darwin)
                                      '("locate")
                                    '("locate" "-i " "-r"))
  "A list where the `car' is the name of the locat program
followed by options.  The search pattern will be appended, so the
\"-r\" option should be the last option.")

(defvar anything-c-source-locate
  '((name . "Locate")
    (candidates . (lambda ()
                    (apply 'start-process "locate-process" nil
                           (append anything-c-locate-options
                                   (list anything-pattern)))))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with locate.")

;;;; Tracker desktop search

(defvar anything-c-source-tracker-search
  '((name . "Tracker Search")
    (candidates . (lambda ()
                    (start-process "tracker-search-process" nil
                                   "tracker-search"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")

;;;; Spotlight (MacOS X desktop search)

(defvar anything-c-source-mac-spotlight
  '((name . "mdfind")
    (candidates . (lambda ()
                    (start-process "mdfind-process" nil
                                   "mdfind" anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")

;;; Types

;;;; Type Actions

;;;;; Extension macros and functions

(defun anything-c-source-p (source)
  "Return non-nil if SOURCE is a valid anything-source."
  (and (assq 'name       source)
       (assq 'candidates source)
       (or (assq 'action source)
           (assq 'type   source))))

(defmacro anything-c-add-to-actions (var action)
  "Add the given ACTION to the list of actions defined in the
given VAR, which has to be an anything-c-source-FOO or
anything-c-actions-TYPE.  Here're two examples:

    ;; Add an action which puts the selected file's path onto the
    ;; kill ring to the default actions for type file.
    (anything-c-add-to-actions anything-c-actions-file
                                    (\"Put Path on Kill Ring\" . kill-new))

    ;; Add an action which opens the selected manual page with the
    ;; `man' command to the actions defined in the source for manual
    ;; pages.
    (anything-c-add-to-actions anything-c-source-man-pages
                                    (\"Open with man\" . man))

This must be done *before* you set `anything-type-actions' or
`anything-sources' in your ~/.emacs.

The purpose of this function is to allow users to extend the list
of type actions or source actions with actions that won't go into
anything-config.el, because they're specific to a platform or a
user."
  (if (anything-c-source-p (symbol-value var))
      ;; Add action to a anything-c-source-FOO
      (let ((action-list (cdr (assq 'action (symbol-value var)))))
        `(unless (member ',action ',action-list)
           (setq ,var (delete (assq 'action ,var) ,var))
           (push (cons 'action (append ',action-list (list ',action))) ,var)))
    ;; Add action to a anything-c-actions-FOO variable
    `(unless (member ',action (cdr ,var))
       (setq ,var
             (cons (car ,var)
                   (append (cdr ,var)
                           (list ',action)))))))

;;;;; Buffers

(defvar anything-c-actions-buffer
  '(buffer . (("Switch to Buffer" . switch-to-buffer)
              ("Switch to Buffer other Window" . switch-to-buffer-other-window)
              ("Switch to Buffer other Frame" . switch-to-buffer-other-frame)
              ("Display Buffer"   . display-buffer)
              ("Kill Buffer"      . kill-buffer)))
  "Actions for type `buffer'.")

;;;;; Files

(defvar anything-c-external-commands-list nil
  "A list of all external commands the user can execute.  If this
variable is not set by the user, it will be calculated
automatically.")

(defun anything-c-external-commands-list-1 ()
  "Returns a list of all external commands the user can execute.

If `anything-c-external-commands-list' is non-nil it will
return its contents.  Else it calculates all external commands
and sets `anything-c-external-commands-list'.

The code is ripped out of `eshell-complete-commands-list'."
  (if anything-c-external-commands-list
      anything-c-external-commands-list
    (setq anything-c-external-commands-list
          (let* ((paths (split-string (getenv "PATH") path-separator))
                 (cwd (file-name-as-directory
                       (expand-file-name default-directory)))
                 (path "") (comps-in-path ())
                 (file "") (filepath "") (completions ()))
            ;; Go thru each path in the search path, finding completions.
            (while paths
              (setq path (file-name-as-directory
                          (expand-file-name (or (car paths) ".")))
                    comps-in-path
                    (and (file-accessible-directory-p path)
                         (file-name-all-completions "" path)))
              ;; Go thru each completion found, to see whether it should be
              ;; used, e.g. see if it's executable.
              (while comps-in-path
                (setq file (car comps-in-path)
                      filepath (concat path file))
                (if (and (not (member file completions))
                         (or (string-equal path cwd)
                             (not (file-directory-p filepath)))
                         (file-executable-p filepath))
                    (setq completions (cons file completions)))
                (setq comps-in-path (cdr comps-in-path)))
              (setq paths (cdr paths)))
            completions))))

(defun anything-c-file-buffers (filename)
  "Returns a list of those buffer names which correspond to the
file given by FILENAME."
  (let (name ret)
    (dolist (buf (buffer-list) ret)
      (let ((bfn (buffer-file-name buf)))
        (when (and bfn
                   (string= filename bfn))
          (push (buffer-name buf) ret)))
      ret)))

(defun anything-c-delete-file (file)
  "Deletes the given file after querying the user.  Asks to kill
buffers associated with that file, too."
  (if (y-or-n-p (format "Really delete file %s? " file))
      (progn
        (let ((buffers (anything-c-file-buffers file)))
          (delete-file file)
          (dolist (buf buffers)
            (when (y-or-n-p (format "Kill buffer %s, too? " buf))
              (kill-buffer buf)))))
    (message "Nothing deleted.")))

(defvar anything-c-actions-file
  '(file . (("Find File" . find-file)
            ("Find File other Window" . find-file-other-window)
            ("Find File other Frame" . find-file-other-frame)
            ("Open Dired in File's Directory" . (lambda (filename)
                                                  (dired (file-name-directory filename))
                                                  (dired-goto-file filename)))
            ("Delete File" . anything-c-delete-file)
            ("Open File with external Tool" .
             (lambda (file)
               (start-process "anything-c-open-file-externally"
                              nil
                              (completing-read "Program: "
                                               (anything-c-external-commands-list-1))
                              file))))))

;;;;; Commands

(defvar anything-c-actions-command
  '(command . (("Call Interactively" . (lambda (command-name)
                                         (call-interactively (intern command-name))))
               ("Describe Command" . (lambda (command-name)
                                       (describe-function (intern command-name))))
               ("Add Command to the Kill Ring" . kill-new)
               ("Go to the Command's Definition" . (lambda (command-name)
                                                     (find-function (intern command-name)))))))

;;;;; Functions

(defvar anything-c-actions-function
  '(function . (("Describe Command" . (lambda (function-name)
                                        (describe-function (intern function-name))))
                ("Add Command to the Kill Ring" . kill-new)
                ("Go to the Function's Definition" . (lambda (function-name)
                                                       (find-function
                                                        (intern function-name)))))))

;;;;; S-Expressions

(defvar anything-c-actions-sexp
  '(sexp . (("Eval S-Expression" . (lambda (c)
                                     (eval (read c))))
            ("Add S-Expression to the Kill Ring" . kill-new))))

;;;; Action Transformers

;;;;; Buffers

(defvar anything-c-action-transformers-buffer nil
  "A List of transformer functions for buffers.  See
`anything-c-transform-buffer-actions' for details.")

(defun anything-c-transform-buffer-actions (actions candidate)
  "Calls any function in
`anything-c-action-transformers-buffer' with the current
list of ACTIONS and the function CANDIDATE modifying the list of
actions for this function dynamically."
  (dolist (trans anything-c-action-transformers-function)
    (setq actions (funcall trans actions candidate)))
  actions)

;;;;; Files

(defun anything-c-transform-file-open-system-specific (actions candidate)
  "Add action to open the file CANDIDATE with the default
application on that platform, e.g. `open' on Macs and `xdg-open'
on GNU/Linux systems."
  (let (tool)
    (cond ((eq system-type 'gnu/linux)
           (setq tool "xdg-open"))
          ((or (eq system-type 'darwin)  ;; Mac OS X
               (eq system-type 'macos))  ;; Mac OS 9
           (setq tool "open")))
    (append actions `(("Open with platform-specific default tool"
                       .
                       (lambda (file)
                         (start-process "anything-c-open-file-with-default-tool"
                                        nil ,tool file)))))))

(defun anything-c-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file.  Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "el")
          (string= (file-name-extension candidate) "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
    actions))

(defun anything-c-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file.  Else return ACTIONS unmodified."
  (if (or (string= (file-name-extension candidate) "htm")
          (string= (file-name-extension candidate) "html"))
      (append actions '(("Browse with Browser" . browse-url)))
    actions))

(defvar anything-c-action-transformers-file
  '(anything-c-transform-file-load-el
    anything-c-transform-file-browse-url
    anything-c-transform-file-open-system-specific)
  "A List of transformer functions for files.  See
`anything-c-transform-file-actions' for details.")

(defun anything-c-transform-file-actions (actions candidate)
  "Calls any function in
`anything-c-action-transformers-file' with the current list
of ACTIONS and the file CANDIDATE modifying the list of actions
for this file dynamically."
  (dolist (trans anything-c-action-transformers-file)
    (setq actions (funcall trans actions candidate)))
  actions)

;;;;; Commands

(defvar anything-c-action-transformers-command nil
  "A List of transformer functions for commands. See
`anything-c-transform-command-actions' for details.")

(defun anything-c-transform-command-actions (actions candidate)
  "Calls any function in
`anything-c-action-transformers-command' with the current
list of ACTIONS and the command CANDIDATE modifying the list of
actions for this command dynamically."
  (dolist (trans anything-c-action-transformers-command)
    (setq actions (funcall trans actions candidate)))
  actions)

;;;;; Function

(defun anything-c-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command.  Else return ACTIONS unmodified."
  (if (commandp (intern candidate))
      (append actions '(("Call Interactively"
                         .
                         (lambda (c)
                           (call-interactively (intern c))))))
    actions))

(defvar anything-c-action-transformers-function
  '(anything-c-transform-function-call-interactively)
  "A List of transformer functions for functions. See
`anything-c-transform-function-actions' for details.")

(defun anything-c-transform-function-actions (actions candidate)
  "Calls any function in
`anything-c-action-transformers-function' with the current
list of ACTIONS and the function CANDIDATE modifying the list of
actions for this function dynamically."
  (dolist (trans anything-c-action-transformers-function)
    (setq actions (funcall trans actions candidate)))
  actions)

;;;;; S-Expressions

(defun anything-c-transform-sexp-eval-command-sexp (actions candidate)
  "If CANDIDATE's `car' in a command, then add an action to
evaluate it and put it onto the `command-history'."
  (if (commandp (car (read candidate)))
      ;; Make it first entry
      (cons '("Eval and put onto command-history" . (lambda (sexp)
                                                      (let ((sym (read sexp)))
                                                        (eval sym)
                                                        (setq command-history
                                                              (cons sym command-history)))))
            actions)
    actions))

(defvar anything-c-action-transformers-sexp
  '(anything-c-transform-sexp-eval-command-sexp)
  "A List of transformer functions for sexps.  See
`anything-c-transform-sexp-actions' for details.")

(defun anything-c-transform-sexp-actions (actions candidate)
  "Calls any function in
`anything-c-action-transformers-sexp' with the current list
of ACTIONS and the sexp CANDIDATE modifying the list of actions
for this sexp dynamically."
  (dolist (trans anything-c-action-transformers-sexp)
    (setq actions (funcall trans actions candidate)))
  actions)

;;;; Candidate Transformers

;;;;; Buffers

(defvar anything-c-candidate-transformers-buffer nil
  "A list of candidate transformer functions for buffers.")

(defun anything-c-transform-buffers (buffers)
  "Calls any function in
`anything-c-candidate-transformers-buffer' with the current
list BUFFERS modifying it dynamically."
  (dolist (trans anything-c-candidate-transformers-buffer)
    (setq buffers (funcall trans buffers)))
  buffers)

;;;;; Files

(defvar anything-c-boring-file-regexp
  (rx "/" (or ".svn/" "CVS/" "_darcs/" ".git/"))
  "File candidates matching this regular expression will be
filtered from the list of candidates if the
`anything-c-skip-boring-files' candidate transformer is
used, or they will be displayed with face `file-name-shadow' if
`anything-c-shadow-boring-files' is used.")

(defun anything-c-shadow-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
displayed with the `file-name-shadow' face if available."
  (mapcar (lambda (file)
            ;; Add shadow face property to boring files.
            (let ((face (if (facep 'file-name-shadow)
                            'file-name-shadow
                          ;; fall back to default on XEmacs
                          'default)))
              (if (string-match anything-c-boring-file-regexp file)
                  (setq file (propertize file 'face face))))
            file)
          files))

(defun anything-c-skip-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
skipped."
  (let ((filtered-files nil))
    (loop for file in files
          do (when (not (string-match anything-c-boring-file-regexp file))
               (push file filtered-files))
          finally (return (nreverse filtered-files)))))

(defun anything-c-shorten-home-path (files)
  "Replaces /home/user with $HOME."
  (mapcar (lambda (file)
            ;; replace path of HOME directory in paths with the string <home>
            (let ((home (replace-regexp-in-string "\\\\" "/" ; stupid Windows...
                                                  (getenv "HOME"))))
              (if (string-match home file)
                  (cons (replace-match "$HOME" nil nil file) file)
                file)))
          files))

(defvar anything-c-candidate-transformers-file
  '(anything-c-shadow-boring-files
    anything-c-shorten-home-path)
  "A list of candidate transformer functions for files.")

(defun anything-c-transform-files (files)
  "Calls any function in
`anything-c-candidate-transformers-file' with the current
list FILES modifying it dynamically."
  (dolist (trans anything-c-candidate-transformers-file)
    (setq files (funcall trans files)))
  files)

;;;;; Commands

(defvar anything-c-candidate-transformers-command nil
  "A list of candidate transformer functions for commands.")

(defun anything-c-transform-commands (commands)
  "Calls any function in
`anything-c-candidate-transformers-command' with the current
list COMMANDS modifying it dynamically."
  (dolist (trans anything-c-candidate-transformers-command)
    (setq commands (funcall trans commands)))
  commands)

;;;;; Functions

(defvar anything-c-candidate-transformers-function nil
  "A list of candidate transformer functions for functions.")

(defun anything-c-transform-functions (functions)
  "Calls any function in
`anything-c-candidate-transformers-function' with the
current list FUNCTIONS modifying it dynamically."
  (dolist (trans anything-c-candidate-transformers-function)
    (setq functions (funcall trans functions)))
  functions)

;;;;; S-Expressions

(defvar anything-c-candidate-transformers-sexp nil
  "A list of candidate transformer functions for sexps.")

(defun anything-c-transform-sexps (sexps)
  "Calls any function in
`anything-c-candidate-transformers-sexp' with the current
list SEXPS modifying it dynamically."
  (dolist (trans anything-c-candidate-transformers-sexp)
    (setq sexps (funcall trans sexps)))
  sexps)

;;; Provide anything-config

(provide 'anything-config)

;; Local Variables:
;; mode: outline-minor
;; End:

;;; anything-config.el ends here
