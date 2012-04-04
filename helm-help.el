;;; helm-help.el --- Help messages for Helm.

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
(require 'helm)


;;; Embeded documentation.
;;
;;
(defvar helm-mode-line-string "\
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "Help string displayed in mode-line in `helm'.
It can be a string or a list of two args, in this case,
first arg is a string that will be used as name for candidates number,
second arg any string to display in mode line.
If nil, use default `mode-line-format'.")


;;; Global help message - Used by `helm-help'
;;
;;
(defvar helm-help-message
  (lambda ()
    (concat
     "\\<helm-map>"
     "`helm' is an Emacs incremental completion and selection narrowing framework.

Narrow the list by typing some pattern,
Multiple patterns are allowed by splitting by space.
Select with natural Emacs operations, choose with RET.

If you have any problems, press C-c C-x C-b!!
Feel free to send bug reports. I'll fix them.
The steps are described in the beginning of helm.el file.

== Basic Operations ==
C-p, Up: Previous Line
C-n, Down : Next Line
M-v, PageUp : Previous Page
C-v, PageDown : Next Page
Enter : Execute first (default) action / Select
M-< : First Line
M-> : Last Line
M-PageUp, C-M-S-v, C-M-y : Previous Page (other-window)
M-PageDown, C-M-v : Next Page (other-window)

Tab, C-i : Show action list
Left : Previous Source
Right, C-o : Next Source
C-k : Delete pattern
C-z : Persistent Action (Execute action with helm session kept)
C-c C-x C-b: Send a bug report

== Shortcuts For 2nd/3rd Action ==
\\[helm-select-2nd-action-or-end-of-line] : Execute 2nd Action (if the minibuffer cursor is at end of line)
\\[helm-select-3rd-action] : Execute 3rd Action

== Visible Marks ==
Visible marks store candidate. Some actions uses marked candidates.

\\[helm-toggle-visible-mark] : Toggle Visible Mark
\\[helm-prev-visible-mark] : Previous Mark
\\[helm-next-visible-mark] : Next Mark

== Miscellaneous Commands ==
\\[helm-toggle-resplit-window] : Toggle vertical/horizontal split helm window
\\[helm-quit-and-find-file] : Drop into `find-file'
\\[helm-delete-current-selection] : Delete Selected Item (visually)
\\[helm-kill-selection-and-quit] : Set Item Into the kill-ring And Quit
\\[helm-yank-selection] : Yank Selected Item Into Pattern
\\[helm-follow-mode] : Toggle Automatical Execution Of Persistent Action
\\[helm-force-update] : Recalculate And Redisplay Candidates

== Global Commands ==
\\<global-map>\\[helm-resume] revives last `helm' session.
It is very useful, so you should bind any key."))
  "Detailed help message string for `helm'.
It also accepts function or variable symbol.")

(defun helm-help-internal (bufname insert-content-fn)
  "Show long message during `helm' session in BUFNAME.
INSERT-CONTENT-FN is the text to be displayed in BUFNAME."
  (save-window-excursion
    (select-window (helm-window))
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create bufname))
    (erase-buffer)
    (funcall insert-content-fn)
    (setq mode-line-format "%b (SPC,C-v:NextPage  b,M-v:PrevPage  other:Exit)")
    (setq cursor-type nil)
    (goto-char 1)
    (helm-help-event-loop)))

(defun helm-help-event-loop ()
  (ignore-errors
    (loop for event = (read-event) do
          (case event
            ((?\C-v ? )  (scroll-up))
            ((?\M-v ?b) (scroll-down))
            (t (return))))))

;;;###autoload
(defun helm-help ()
  "Help of `helm'."
  (interactive)
  (helm-help-internal
   " *Helm Help*"
   (lambda ()
     (insert (substitute-command-keys
              (helm-interpret-value (or (assoc-default
                                         'help-message
                                         (helm-get-current-source))
                                        helm-help-message))))
     (org-mode))))

;;; `helm-buffer-list' help
;;
;;
(defvar helm-c-buffer-help-message
  "== Helm Buffer ==
\nTips:
You can enter a partial name of major-mode (e.g lisp, sh) to narrow down buffers.
Enter then a space and a pattern to narrow down to buffers matching this pattern.
\nSpecific commands for `helm-buffers-list':
\\<helm-c-buffer-map>
\\[helm-buffer-run-zgrep]\t\t->Grep Buffer(s) works as zgrep too. (C-u grep all buffers but non--file buffers).
\\[helm-buffer-switch-other-window]\t\t->Switch other window.
\\[helm-buffer-switch-other-frame]\t\t->Switch other frame.
\\[helm-buffer-run-query-replace-regexp]\t\t->Query replace regexp in marked buffers.
\\[helm-buffer-run-query-replace]\t\t->Query replace in marked buffers.
\\[helm-buffer-switch-to-elscreen]\t\t->Find buffer in Elscreen.
\\[helm-buffer-run-ediff]\t\t->Ediff current buffer with candidate.  If two marked buffers ediff those buffers.
\\[helm-buffer-run-ediff-merge]\t\t->Ediff merge current buffer with candidate.  If two marked buffers ediff merge those buffers.
\\[helm-buffer-diff-persistent]\t\t->Toggle Diff buffer with saved file without quitting.
\\[helm-buffer-revert-persistent]\t\t->Revert buffer without quitting.
\\[helm-buffer-save-persistent]\t\t->Save buffer without quitting.
\\[helm-buffer-run-kill-buffers]\t\t->Delete marked buffers and quit.
\\[helm-toggle-all-marks]\t\t->Toggle all marks.
\\[helm-mark-all]\t\t->Mark all.
\\[helm-c-buffer-help]\t\t->Display this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-c-buffer-help ()
  "Help command for helm buffers."
  (interactive)
  (let ((helm-help-message helm-c-buffer-help-message))
    (helm-help)))

;;; Find files help (`helm-find-files')
;;
;;
(defvar helm-ff-help-message
  "== Helm Find Files ==
\nTips:
\n- Enter `~/' at end of pattern to quickly reach home directory.
- Enter `/' at end of pattern to quickly reach root of your file system.
- Enter `./' at end of pattern to quickly reach `default-directory' (initial start of session).
- You can complete with partial basename \(e.g \"fb\" will complete \"foobar\"\).
- Use `C-u C-z' to watch an image.
- To browse images directories turn on `helm-follow-mode' and navigate with arrow keys.
- When entered ediff, hitting `C-g' will ask you to use locate to find the file to ediff with.

\nSpecific commands for `helm-find-files':
\\<helm-find-files-map>
\\[helm-ff-run-locate]\t\t->Run Locate on basename of candidate (C-u to specify locate db).
\\[helm-ff-run-grep]\t\t->Run Grep (C-u Recursive).
\\[helm-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[helm-ff-run-zgrep]\t\t->Run zgrep (C-u Recursive).
\\[helm-ff-run-etags]\t\t->Run Etags (C-u use thing-at-point `C-u C-u' reload cache)
\\[helm-ff-run-rename-file]\t\t->Rename File (C-u Follow).
\\[helm-ff-run-copy-file]\t\t->Copy File (C-u Follow).
\\[helm-ff-run-byte-compile-file]\t\t->Byte Compile File (C-u Load).
\\[helm-ff-run-load-file]\t\t->Load File.
\\[helm-ff-run-symlink-file]\t\t->Symlink File.
\\[helm-ff-run-hardlink-file]\t\t->Hardlink file.
\\[helm-ff-run-delete-file]\t\t->Delete File.
\\[helm-ff-run-kill-buffer-persistent]\t\t->Kill buffer candidate without quitting.
\\[helm-ff-persistent-delete]\t\t->Delete file without quitting.
\\[helm-ff-run-switch-to-eshell]\t\t->Switch to Eshell.
\\[helm-ff-run-eshell-command-on-file]\t\t->Eshell command on file (C-u Run on all marked files at once).
\\[helm-ff-run-ediff-file]\t\t->Ediff file.
\\[helm-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[helm-ff-run-complete-fn-at-point]\t\t->Complete file name at point.
\\[helm-ff-run-switch-other-window]\t\t->Switch other window.
\\[helm-ff-run-switch-other-frame]\t\t->Switch other frame.
\\[helm-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\\[helm-ff-rotate-left-persistent]\t\t->Rotate Image Left.
\\[helm-ff-rotate-right-persistent]\t\t->Rotate Image Right.
\\[helm-find-files-down-one-level]\t\t->Go down precedent directory.
\\[helm-ff-run-switch-to-history]\t\t->Switch to helm find-files history.
\\[helm-ff-properties-persistent]\t\t->Show file properties in a tooltip.
\\[helm-mark-all]\t\t->Mark all visibles candidates.
\\[helm-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[helm-unmark-all]\t\t->Unmark all candidates, visibles and invisibles.
\\[helm-ff-run-gnus-attach-files]\t\t->Gnus attach files to message buffer.
\\[helm-ff-run-print-file]\t\t->Print file, (C-u to refresh printers list).
\\[helm-enlarge-window]\t\t->Enlarge helm window.
\\[helm-narrow-window]\t\t->Narrow helm window.
\\[helm-ff-run-toggle-basename]\t\t->Toggle basename/fullpath.
\\[helm-send-bug-report-from-helm]\t\t->Send Bug report.
\\[helm-ff-help]\t\t->Display this help info.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-ff-help ()
  "Help command for `helm-find-files'."
  (interactive)
  (let ((helm-help-message helm-ff-help-message))
    (helm-help)))

;;; Help for `helm-c-read-file-name'
;;
;;
(defvar helm-read-file-name-help-message
  "== Helm read file name Map ==\
\nSpecific commands for helm-c-read-file-name:
\\<helm-c-read-file-map>
\\[helm-find-files-down-one-level]\t\t->Go down precedent directory.
\\[helm-ff-run-toggle-auto-update]\t->Toggle auto expansion of directories.
\\[helm-next-source]\t->Goto next source.
\\[helm-previous-source]\t->Goto previous source.
\\[helm-read-file-name-help]\t\t->Display this help info.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-read-file-name-help ()
  (interactive)
  (let ((helm-help-message helm-read-file-name-help-message))
    (helm-help)))

;;; Generic file help - Used by locate.
;;
;;
(defvar helm-generic-file-help-message
  "== Helm Generic files Map ==\
\nSpecific commands for helm locate and others files sources:
\\<helm-generic-files-map>
\\[helm-ff-run-grep]\t\t->Run grep (C-u recurse).
\\[helm-ff-run-pdfgrep]\t\t->Run Pdfgrep on marked files.
\\[helm-ff-run-delete-file]\t\t->Delete file.
\\[helm-ff-run-ediff-file]\t\t->Ediff file.
\\[helm-ff-run-ediff-merge-file]\t\t->Ediff merge file.
\\[helm-ff-run-switch-other-window]\t\t->Switch other window.
\\[helm-ff-properties-persistent]\t\t->Show file properties.
\\[helm-yank-text-at-point]\t\t->Yank text at point.
\\[helm-ff-run-open-file-externally]\t\t->Open file with external program (C-u to choose).
\nLocate tips:
You can add after writing search pattern any of the locate command line options.
e.g -b, -e, -n <number>...etc.
See Man locate for more infos.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-generic-file-help ()
  (interactive)
  (let ((helm-help-message helm-generic-file-help-message))
    (helm-help)))

;;; Grep help
;;
;;
(defvar helm-grep-help-message
  "== Helm Grep Map ==\
\nHelm Grep tips:
You can start grep with a prefix arg to recurse in subdirectories.
You can use wild card when selecting files (e.g *.el)
You can grep in many differents directories by marking files or wild cards.
You can save your results in a grep-mode buffer, see below.

\nSpecific commands for Helm Grep:
\\<helm-c-grep-map>
\\[helm-c-goto-next-file]\t->Next File.
\\[helm-c-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-c-grep-run-other-window-action]\t\t->Jump other window.
\\[helm-c-grep-run-persistent-action]\t\t->Run persistent action (Same as `C-z').
\\[helm-c-grep-run-default-action]\t\t->Run default action (Same as RET).
\\[helm-c-grep-run-save-buffer]\t\t->Save to a `grep-mode' enabled buffer.
\\[helm-grep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-grep-help ()
  (interactive)
  (let ((helm-help-message helm-grep-help-message))
    (helm-help)))

;;; Pdf grep help
;;
;;
(defvar helm-pdfgrep-help-message
  "== Helm PdfGrep Map ==\
\nSpecific commands for Pdf Grep:
\\<helm-c-pdfgrep-map>
\\[helm-c-goto-next-file]\t->Next File.
\\[helm-c-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-pdfgrep-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-pdfgrep-help ()
  (interactive)
  (let ((helm-help-message helm-pdfgrep-help-message))
    (helm-help)))

;;; Etags help
;;
;;
(defvar helm-etags-help-message
  "== Helm Etags Map ==\
\nSpecific commands for Etags:
\\<helm-c-etags-map>
\\[helm-c-goto-next-file]\t->Next File.
\\[helm-c-goto-precedent-file]\t\t->Precedent File.
\\[helm-yank-text-at-point]\t\t->Yank Text at point in minibuffer.
\\[helm-etags-help]\t\t->Show this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-etags-help ()
  "The help function for etags."
  (interactive)
  (let ((helm-help-message helm-etags-help-message))
    (helm-help)))

;;; Ucs help
;;
;;
(defvar helm-c-ucs-help-message
  "== Helm Ucs ==
\nSpecific commands for `helm-ucs':
\\<helm-c-ucs-map>
\\[helm-c-ucs-persistent-insert]\t->Insert char.
\\[helm-c-ucs-persistent-forward]\t->Forward char.
\\[helm-c-ucs-persistent-backward]\t->Backward char.
\\[helm-c-ucs-persistent-delete]\t->Delete char backward.
\\[helm-c-ucs-help]\t\t->Show this help.

\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-c-ucs-help ()
  "Help command for `helm-ucs'."
  (interactive)
  (let ((helm-help-message helm-c-ucs-help-message))
    (helm-help)))

;;; Bookmark help
;;
;;
(defvar helm-bookmark-help-message
  "== Helm bookmark name Map ==\
\nSpecific commands for bookmarks:
\\<helm-c-bookmark-map>
\\[helm-c-bookmark-run-jump-other-window]\t\t->Jump other window.
\\[helm-c-bookmark-run-delete]\t\t->Delete bookmark.
\\[helm-c-bmkext-run-edit]\t\t->Edit bookmark (only for bmkext).
\\[helm-c-bookmark-help]\t\t->Run this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-c-bookmark-help ()
  "Help command for bookmarks."
  (interactive)
  (let ((helm-help-message helm-bookmark-help-message))
    (helm-help)))

;;; Eshell command on file help
;;
;;
(defvar helm-c-esh-help-message
  "== Helm eshell on file ==
\nTips:

- Passing extra args after filename:

Normally your command or alias will be called with file as argument.

e.g <command> 'candidate_file'

But you can also pass an argument or more after 'candidate_file' like this:

<command> %s [extra_args]\n

'candidate_file' will be inserted at '%s' and your command will look at this:

<command> 'candidate_file' [args]

- Specify many files as args (marked files):

e.g <command> file1 file2 ...

Please restart and use a prefix arg to call `helm-find-files-eshell-command-on-file'.
Otherwise your command will be called many times like this:

<command> file1 <command> file2 etc...

\nSpecific commands for `helm-find-files-eshell-command-on-file':
\\<helm-esh-on-file-map>
\\[helm-esh-help]\t\t->Display this help.
\n== Helm Map ==
\\{helm-map}")

;;;###autoload
(defun helm-esh-help ()
  "Help command for `helm-find-files-eshell-command-on-file'."
  (interactive)
  (let ((helm-help-message helm-c-esh-help-message))
    (helm-help)))


;;; Mode line strings
;;
;;
(defvar helm-buffer-mode-line-string
  '("Buffer(s)" "\
\\<helm-c-buffer-map>\
\\[helm-c-buffer-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
    "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-ff-mode-line-string "\
\\<helm-find-files-map>\
\\[helm-ff-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-read-file-name-mode-line-string "\
\\<helm-c-read-file-map>\
\\[helm-read-file-name-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-generic-file-mode-line-string "\
\\<helm-generic-files-map>\
\\[helm-generic-file-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in Locate.")

(defvar helm-grep-mode-line-string"\
\\<helm-c-grep-map>\
\\[helm-grep-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-do-grep'.")

(defvar helm-pdfgrep-mode-line-string "\
\\<helm-c-pdfgrep-map>\
\\[helm-pdfgrep-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-do-pdfgrep'.")

(defvar helm-etags-mode-line-string "\
\\<helm-c-etags-map>\
\\[helm-etags-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-etags-select'.")

(defvar helm-c-ucs-mode-line-string "\
\\<helm-c-ucs-map>\
\\[helm-c-ucs-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-ucs'.")

(defvar helm-bookmark-mode-line-string
  '("Bookmark(s)" "\
\\<helm-c-bookmark-map>\
\\[helm-c-bookmark-help]:Help \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct")
  "String displayed in mode-line in `helm-c-source-buffers-list'")

(defvar helm-occur-mode-line "\
\\<helm-map>\
\\[helm-help]:Help \
\\<helm-occur-map>\
\\[helm-occur-run-query-replace-regexp]:Query replace regexp \
\\<helm-map>\
\\[helm-select-action]:Act \
\\[helm-exit-minibuffer]/\
\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct")



;;; Attribute Documentation
;;
;;
;;;###autoload
(defun helm-describe-helm-attribute (helm-attribute)
  "Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol."
  (interactive (list (intern
                      (completing-read
                       "Describe helm attribute: "
                       (mapcar 'symbol-name helm-additional-attributes)
                       nil t))))
  (with-output-to-temp-buffer "*Help*"
    (princ (get helm-attribute 'helm-attrdoc))))

(helm-document-attribute 'name "mandatory"
  "  The name of the source. It is also the heading which appears
  above the list of matches from the source. Must be unique.")

(helm-document-attribute 'header-name "optional"
  "  A function returning the display string of the header. Its
  argument is the name of the source. This attribute is useful to
  add an additional information with the source name.")

(helm-document-attribute 'candidates "mandatory if candidates-in-buffer attribute is not provided"
  "  Specifies how to retrieve candidates from the source. It can
  either be a variable name, a function called with no parameters
  or the actual list of candidates.

  The list must be a list whose members are strings, symbols
  or (DISPLAY . REAL) pairs.

  In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
  in the Helm buffer, but the REAL one is used as action
  argument when the candidate is selected. This allows a more
  readable presentation for candidates which would otherwise be,
  for example, too long or have a common part shared with other
  candidates which can be safely replaced with an abbreviated
  string for display purposes.

  Note that if the (DISPLAY . REAL) form is used then pattern
  matching is done on the displayed string, not on the real
  value.

  If the candidates have to be retrieved asynchronously (for
  example, by an external command which takes a while to run)
  then the function should start the external command
  asynchronously and return the associated process object.
  Helm will take care of managing the process (receiving the
  output from it, killing it if necessary, etc.). The process
  should return candidates matching the current pattern (see
  variable `helm-pattern'.)

  Note that currently results from asynchronous sources appear
  last in the helm buffer regardless of their position in
  `helm-sources'.")

(helm-document-attribute 'action "mandatory if type attribute is not provided"
  "  It is a list of (DISPLAY . FUNCTION) pairs or FUNCTION.
  FUNCTION is called with one parameter: the selected candidate.

  An action other than the default can be chosen from this list
  of actions for the currently selected candidate (by default
  with TAB). The DISPLAY string is shown in the completions
  buffer and the FUNCTION is invoked when an action is
  selected. The first action of the list is the default.")

(helm-document-attribute 'coerce "optional"
  "  It's a function called with one argument: the selected candidate.

  This function is intended for type convertion.
  In normal case, the selected candidate (string) is passed to action function.
  If coerce function is specified, it is called just before action function.

  Example: converting string to symbol
    (coerce . intern)")

(helm-document-attribute 'type "optional if action attribute is provided"
  "  Indicates the type of the items the source returns.

  Merge attributes not specified in the source itself from
  `helm-type-attributes'.

  This attribute is implemented by plug-in.")

(helm-document-attribute 'init "optional"
  "  Function called with no parameters when helm is started. It
  is useful for collecting current state information which can be
  used to create the list of candidates later.

  For example, if a source needs to work with the current
  directory then it can store its value here, because later
  helm does its job in the minibuffer and in the
  `helm-buffer' and the current directory can be different
  there.")

(helm-document-attribute 'delayed-init "optional"
  "  Function called with no parameters before candidate function is
  called.  It is similar with `init' attribute, but its
  evaluation is deferred. It is useful to combine with ")

(helm-document-attribute 'match "optional"
  "  List of functions called with one parameter: a candidate. The
  function should return non-nil if the candidate matches the
  current pattern (see variable `helm-pattern').

  This attribute allows the source to override the default
  pattern matching based on `string-match'. It can be used, for
  example, to implement a source for file names and do the
  pattern matching on the basename of files, since it's more
  likely one is typing part of the basename when searching for a
  file, instead of some string anywhere else in its path.

  If the list contains more than one function then the list of
  matching candidates from the source is constructed by appending
  the results after invoking the first function on all the
  potential candidates, then the next function, and so on. The
  matching candidates supplied by the first function appear first
  in the list of results and then results from the other
  functions, respectively.

  This attribute has no effect for asynchronous sources (see
  attribute `candidates'), since they perform pattern matching
  themselves.")

(helm-document-attribute 'candidate-transformer "optional"
  "  It's a function or a list of functions called with one argument
  when the completion list from the source is built. The argument
  is the list of candidates retrieved from the source. The
  function should return a transformed list of candidates which
  will be used for the actual completion.  If it is a list of
  functions, it calls each function sequentially.

  This can be used to transform or remove items from the list of
  candidates.

  Note that `candidates' is run already, so the given transformer
  function should also be able to handle candidates with (DISPLAY
  . REAL) format.")

(helm-document-attribute 'filtered-candidate-transformer "optional"
  "  It has the same format as `candidate-transformer', except the
  function is called with two parameters: the candidate list and
  the source.

  This transformer is run on the candidate list which is already
  filtered by the current pattern. While `candidate-transformer'
  is run only once, it is run every time the input pattern is
  changed.

  It can be used to transform the candidate list dynamically, for
  example, based on the current pattern.

  In some cases it may also be more efficent to perform candidate
  transformation here, instead of with `candidate-transformer'
  even if this transformation is done every time the pattern is
  changed.  For example, if a candidate set is very large then
  `candidate-transformer' transforms every candidate while only
  some of them will actually be dislpayed due to the limit
  imposed by `helm-candidate-number-limit'.

  Note that `candidates' and `candidate-transformer' is run
  already, so the given transformer function should also be able
  to handle candidates with (DISPLAY . REAL) format.

  This option has no effect for asynchronous sources. (Not yet,
  at least.")

(helm-document-attribute 'action-transformer "optional"
  "  It's a function or a list of functions called with two
  arguments when the action list from the source is
  assembled. The first argument is the list of actions, the
  second is the current selection.  If it is a list of functions,
  it calls each function sequentially.

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate.")

(helm-document-attribute 'pattern-transformer "optional"
  "  It's a function or a list of functions called with one argument
  before computing matches. Its argument is `helm-pattern'.
  Functions should return transformed `helm-pattern'.

  It is useful to change interpretation of `helm-pattern'.")

(helm-document-attribute 'delayed "optional"
  "  Candidates from the source are shown only if the user stops
  typing and is idle for `helm-idle-delay' seconds.")

(helm-document-attribute 'volatile "optional"
  "  Indicates the source assembles the candidate list dynamically,
  so it shouldn't be cached within a single Helm
  invocation. It is only applicable to synchronous sources,
  because asynchronous sources are not cached.")

(helm-document-attribute 'requires-pattern "optional"
  "  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates.")

(helm-document-attribute 'persistent-action "optional"
  "  Function called with one parameter; the selected candidate.

  An action performed by `helm-execute-persistent-action'.
  If none, use the default action.")

(helm-document-attribute 'candidates-in-buffer "optional"
  "  Shortcut attribute for making and narrowing candidates using
  buffers.  This newly-introduced attribute prevents us from
  forgetting to add volatile and match attributes.

  See docstring of `helm-candidates-in-buffer'.

  (candidates-in-buffer) is equivalent of three attributes:
    (candidates . helm-candidates-in-buffer)
    (volatile)
    (match identity)

  (candidates-in-buffer . candidates-function) is equivalent of:
    (candidates . candidates-function)
    (volatile)
    (match identity)

  This attribute is implemented by plug-in.")

(helm-document-attribute 'search "optional"
  "  List of functions like `re-search-forward' or `search-forward'.
  Buffer search function used by `helm-candidates-in-buffer'.
  By default, `helm-candidates-in-buffer' uses `re-search-forward'.
  This attribute is meant to be used with
  (candidates . helm-candidates-in-buffer) or
  (candidates-in-buffer) in short.")

(helm-document-attribute 'search-from-end "optional"
  "  Make `helm-candidates-in-buffer' search from the end of buffer.
  If this attribute is specified, `helm-candidates-in-buffer' uses
  `re-search-backward' instead.")

(helm-document-attribute 'get-line "optional"
  "  A function like `buffer-substring-no-properties' or `buffer-substring'.
  This function converts point of line-beginning and point of line-end,
  which represents a candidate computed by `helm-candidates-in-buffer'.
  By default, `helm-candidates-in-buffer' uses
  `buffer-substring-no-properties'.")

(helm-document-attribute 'display-to-real "optional"
  "  Function called with one parameter; the selected candidate.

  The function transforms the selected candidate, and the result
  is passed to the action function.  The display-to-real
  attribute provides another way to pass other string than one
  shown in Helm buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if REAL
  can be generated from DISPLAY, display-to-real is more
  convenient and faster.")

(helm-document-attribute 'real-to-display "optional"
  "  Function called with one parameter; the selected candidate.

  The inverse of display-to-real attribute.

  The function transforms the selected candidate, which is passed
  to the action function, for display.  The real-to-display
  attribute provides the other way to pass other string than one
  shown in Helm buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if
  DISPLAY can be generated from REAL, real-to-display is more
  convenient.

  Note that DISPLAY parts returned from candidates /
  candidate-transformer are IGNORED as the name `display-to-real'
  says.")

(helm-document-attribute 'cleanup "optional"
  "  Function called with no parameters when *helm* buffer is closed. It
  is useful for killing unneeded candidates buffer.

  Note that the function is executed BEFORE performing action.")

(helm-document-attribute 'candidate-number-limit "optional"
  "  Override `helm-candidate-number-limit' only for this source.")

(helm-document-attribute 'accept-empty "optional"
  "  Pass empty string \"\" to action function.")

(helm-document-attribute 'dummy "optional"
  "  Set `helm-pattern' to candidate. If this attribute is
  specified, The candidates attribute is ignored.

  This attribute is implemented by plug-in.
  This plug-in implies disable-shortcuts plug-in.")

(helm-document-attribute 'multiline "optional"
  "  Enable to selection multiline candidates.")

(helm-document-attribute 'update "optional"
  (substitute-command-keys
   "  Function called with no parameters when \
\\<helm-map>\\[helm-force-update] is pressed."))

(helm-document-attribute 'mode-line "optional"
  "  source local `helm-mode-line-string'. (included in `mode-line-format')
  It accepts also variable/function name.")

(helm-document-attribute 'header-line "optional"
  "  source local `header-line-format'.
  It accepts also variable/function name. ")

(helm-document-attribute
    'resume "optional"
  "  Function called with no parameters when `helm-resume' is started.")

(helm-document-attribute 'keymap "optional"
  "  Specific keymap for this source.
  It is useful to have a keymap per source when using more than one source.
  Otherwise, a keymap can be set per command with `helm' argument KEYMAP.
  NOTE: when a source have `helm-map' as keymap attr,
  the global value of `helm-map' will override the actual local one.")

(helm-document-attribute 'help-message "optional"
  "  Help message for this source.
  If not present, `helm-help-message' value will be used.")


(provide 'helm-help)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-help.el ends here
