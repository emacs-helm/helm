;;; helm-config.el --- Applications library for `helm.el'

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

;;; Require
;;
;;
(require 'cl)
(require 'helm)
(require 'helm-vars)
(require 'helm-utils)
(require 'helm-plugin)
(require 'helm-buffers)
(require 'helm-net)
(require 'helm-external)
(require 'helm-files)
(require 'helm-locate)
(require 'helm-elisp)
(require 'helm-eshell)
(require 'helm-regexp)
(require 'helm-grep)
(require 'helm-mode)
(require 'helm-ring)
(require 'helm-command)
(require 'helm-eval)
(require 'helm-tags)
(require 'helm-adaptative)
(require 'helm-imenu)
(require 'helm-bookmark)
(require 'helm-org)
(require 'helm-info)
(require 'helm-man)
(require 'helm-color)
(require 'helm-font)
(require 'helm-sys)
(require 'helm-misc)
(require 'helm-w3m nil t)
(require 'helm-firefox nil t)
(require 'helm-bmkext nil t)
(require 'helm-apt nil t)
(require 'helm-gentoo nil t)
(require 'helm-bbdb nil t)
(require 'helm-emms nil t)
(require 'yaoddmuse nil t)
(require 'helm-call-tree nil t)
(require 'helm-descbinds nil t)
(require 'helm-slime nil t)
(eval-when-compile (require 'semantic nil t))
(require 'helm-match-plugin)



;;; Declare external functions
;;
;;
(declare-function gnus-dired-attach "ext:gnus-dired.el" (files-to-attach))
(declare-function image-dired-display-image "image-dired.el" (file &optional original-size))
(declare-function image-dired-update-property "image-dired.el" (prop value))
(declare-function woman-file-name-all-completions "woman.el" (topic))
(declare-function Man-getpage-in-background "man.el" (topic))
(declare-function simple-call-tree-analyze "ext:simple-call-tree.el" (&optional test))
(declare-function yaoddmuse-update-pagename "ext:yaoddmuse.el" (&optional unforced))
(declare-function yaoddmuse-get-library-list "ext:yaoddmuse.el" (&optional dirs string))
(declare-function org-get-current-options "ext:org-exp.el")
(declare-function emms-streams "ext:emms-streams")
(declare-function emms-stream-delete-bookmark "ext:emms-streams")
(declare-function emms-stream-add-bookmark "ext:emms-streams" (name url fd type))
(declare-function emms-stream-save-bookmarks-file "ext:emms-streams")
(declare-function emms-stream-quit "ext:emms-streams")
(declare-function with-current-emms-playlist "ext:emms" (&rest body))
(declare-function emms-playlist-tracks-in-region "ext:emms" (beg end))
(declare-function emms-playlist-first "ext:emms")
(declare-function emms-playlist-mode-play-smart "ext:emms-playlist-mode")
(declare-function term-line-mode "term")
(declare-function term-char-mode "term")
(declare-function term-send-input "term")
(declare-function term-send-eof "term")
(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info.el" (filename nodename &optional no-going-back))
(declare-function elscreen-find-screen-by-buffer "ext:elscreen.el" (buffer &optional create))
(declare-function elscreen-find-file "ext:elscreen.el" (filename))
(declare-function elscreen-goto "ext:elscreen.el" (screen))
(declare-function semantic-format-tag-summarize "ext:format.el" (tag &optional parent color) t)
(declare-function semantic-tag-components "ext:tag.el" (tag) t)
(declare-function semantic-go-to-tag "ext:tag-file.el" (tag) t)
(declare-function semantic-tag-type "ext:tag-file.el" (tag) t)
(declare-function semantic-tag-class "ext:tag-file.el" (tag) t)
(declare-function bbdb "ext:bbdb-com")
(declare-function bbdb-current-record "ext:bbdb-com")
(declare-function bbdb-redisplay-one-record "ext:bbdb-com")
(declare-function bbdb-record-net "ext:bbdb-com" (string) t)
(declare-function bbdb-current-record "ext:bbdb-com")
(declare-function bbdb-dwim-net-address "ext:bbdb-com")
(declare-function bbdb-records "ext:bbdb-com"
                  (&optional dont-check-disk already-in-db-buffer))
(declare-function eshell-read-aliases-list "em-alias")
(declare-function eshell-send-input "esh-mode" (&optional use-region queue-p no-newline))
(declare-function eshell-bol "esh-mode")
(declare-function eldoc-current-symbol "eldoc")
(declare-function eldoc-get-fnsym-args-string "eldoc" (sym &optional index))
(declare-function eldoc-get-var-docstring "eldoc" (sym))
(declare-function eldoc-fnsym-in-current-sexp "eldoc")
(declare-function find-library-name "find-func.el" (library))
(declare-function secure-hash "ext:fns.c" (algorithm object &optional start end binary))
(declare-function w32-shell-execute "ext:w32fns.c" (operation document &optional parameters show-flag))
(declare-function undo-tree-restore-state-from-register "ext:undo-tree.el" (register))


;;; Helm-command-map
;;
;;
(defvar helm-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a")         'helm-c-apropos)
    (define-key map (kbd "e")         'helm-c-etags-select)
    (define-key map (kbd "l")         'helm-locate)
    (define-key map (kbd "s")         'helm-surfraw)
    (define-key map (kbd "r")         'helm-regexp)
    (define-key map (kbd "w")         'helm-w3m-bookmarks)
    (define-key map (kbd "x")         'helm-firefox-bookmarks)
    (define-key map (kbd "#")         'helm-emms)
    (define-key map (kbd "m")         'helm-man-woman)
    (define-key map (kbd "t")         'helm-top)
    (define-key map (kbd "i")         'helm-imenu)
    (define-key map (kbd "<tab>")     'helm-lisp-completion-at-point)
    (define-key map (kbd "p")         'helm-list-emacs-process)
    (define-key map (kbd "C-x r b")   'helm-c-pp-bookmarks)
    (define-key map (kbd "M-y")       'helm-show-kill-ring)
    (define-key map (kbd "C-c <SPC>") 'helm-all-mark-rings)
    (define-key map (kbd "C-x C-f")   'helm-find-files)
    (define-key map (kbd "f")         'helm-for-files)
    (define-key map (kbd "C-:")       'helm-eval-expression-with-eldoc)
    (define-key map (kbd "C-,")       'helm-calcul-expression)
    (define-key map (kbd "M-x")       'helm-M-x)
    (define-key map (kbd "C-x C-w")   'helm-write-file)
    (define-key map (kbd "C-x i")     'helm-insert-file)
    (define-key map (kbd "M-s o")     'helm-occur)
    (define-key map (kbd "M-g s")     'helm-do-grep)
    (define-key map (kbd "c")         'helm-colors)
    (define-key map (kbd "F")         'helm-select-xfont)
    (define-key map (kbd "C-c f")     'helm-recentf)
    (define-key map (kbd "C-c g")     'helm-google-suggest)
    (define-key map (kbd "h i")       'helm-info-at-point)
    (define-key map (kbd "h r")       'helm-info-emacs)
    (define-key map (kbd "h g")       'helm-info-gnus)
    (define-key map (kbd "C-x C-b")   'helm-buffers-list)
    (define-key map (kbd "C-c C-b")   'helm-browse-code)
    (define-key map (kbd "C-x r i")   'helm-register)
    (define-key map (kbd "C-c C-x")   'helm-c-run-external-command)
    map))

;; Don't override the keymap we just defined with an empty
;; keymap.  This also protect bindings changed by the user.
(defvar helm-command-prefix)
;;;###autoload
(define-prefix-command 'helm-command-prefix)
(fset 'helm-command-prefix helm-command-map)
(setq  helm-command-prefix helm-command-map)


;;; Menu
;;
;;
(easy-menu-define nil global-map
  "`helm' menu"
  '("Helm"
    ["Find any Files/Buffers" helm-for-files t]
    ["Helm Everywhere (Toggle)" helm-mode t]
    "----"
    ("Files:"
     ["Find files" helm-find-files t]
     ["Recent Files" helm-recentf t]
     ["Locate" helm-locate t]
     ["Bookmarks" helm-c-pp-bookmarks t])
    ("Buffers:"
     ["Find buffers" helm-buffers-list t])
    ("Commands:"
     ["Emacs Commands" helm-M-x t]
     ["Externals Commands" helm-c-run-external-command t])
    ("Help:"
     ["Helm Apropos" helm-c-apropos t])
    ("Info:"
     ["Info at point" helm-info-at-point t]
     ["Emacs Manual index" helm-info-emacs t]
     ["Gnus Manual index" helm-info-gnus t])
    ("Org:"
     ["Org keywords" helm-org-keywords t]
     ["Org headlines" helm-org-headlines t])
    ("Tools:"
     ["Occur" helm-occur t]
     ["Grep" helm-do-grep t]
     ["Etags" helm-c-etags-select t]
     ["Lisp complete at point" helm-lisp-completion-at-point t]
     ["Browse Kill ring" helm-show-kill-ring t]
     ["Browse register" helm-register t]
     ["Browse code" helm-browse-code t]
     ["Mark Ring" helm-all-mark-rings t]
     ["Regexp handler" helm-regexp t]
     ["Colors & Faces" helm-colors t]
     ["Show xfonts" helm-select-xfont t]
     ["Ucs Symbols" helm-ucs t]
     ["Imenu" helm-imenu t]
     ["Google Suggest" helm-google-suggest t]
     ["Eval expression" helm-eval-expression-with-eldoc t]
     ["Calcul expression" helm-calcul-expression t]
     ["Man pages" helm-man-woman t]
     ["Top externals process" helm-top t]
     ["Emacs internals process" helm-list-emacs-process t])
    "----"
    ["Prefered Options" helm-configuration t]))

;;; Helm map add ons
;;
;;
(define-key helm-map (kbd "C-x C-f") 'helm-quit-and-find-file)
(define-key helm-map (kbd "M-m")     'helm-toggle-all-marks)
(define-key helm-map (kbd "C-w")     'helm-yank-text-at-point)


;;; Specialized keymaps
;;
;;
(defvar helm-esh-on-file-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?")    'helm-esh-help)
    map)
  "Keymap for `helm-find-files-eshell-command-on-file'.")


;;; Embeded documentation.
;;
;;
;;; Global help message - Used by `helm-help'
;;
;;
(setq helm-help-message
      (lambda ()
        (concat
         "\\<helm-map>"
         "`helm' is QuickSilver-like candidate-selection framework.

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
It is very useful, so you should bind any key.

Single source is executed by \\[helm-call-source].")))

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

(defun helm-esh-help ()
  "Help command for `helm-find-files-eshell-command-on-file'."
  (interactive)
  (let ((helm-help-message helm-c-esh-help-message))
    (helm-help)))


;;; Mode line strings
;;
;;
(defvar helm-buffer-mode-line-string
  '("Buffer(s)"
    "\\<helm-c-buffer-map>\
\\[helm-c-buffer-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
    "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-ff-mode-line-string
  "\\<helm-find-files-map>\
\\[helm-ff-help]:Help, \
\\[helm-send-bug-report-from-helm]:BugReport, \
\\<helm-map>\
\\[helm-select-action]:Acts, \
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-read-file-name-mode-line-string
  "\\<helm-c-read-file-map>\
\\[helm-read-file-name-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct"
  "String displayed in mode-line in `helm-c-source-find-files'")

(defvar helm-generic-file-mode-line-string
  "\\<helm-generic-files-map>\
\\[helm-generic-file-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in Locate.")

(defvar helm-grep-mode-line-string
  "\\<helm-c-grep-map>\
\\[helm-grep-help]:Help,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in `helm-do-grep'.")

(defvar helm-pdfgrep-mode-line-string
  "\\<helm-c-pdfgrep-map>\
\\[helm-pdfgrep-help]:Help,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in `helm-do-pdfgrep'.")

(defvar helm-etags-mode-line-string
  "\\<helm-c-etags-map>\
\\[helm-etags-help]:Help,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
  "String displayed in mode-line in `helm-c-etags-select'.")


(defvar helm-c-ucs-mode-line-string
  "\\<helm-c-ucs-map>\
\\[helm-c-ucs-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct."
  "String displayed in mode-line in `helm-ucs'.")

(defvar helm-bookmark-mode-line-string
  '("Bookmark(s)"
    "\\<helm-c-bookmark-map>\
\\[helm-c-bookmark-help]:Help, \
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport."
    "String displayed in mode-line in `helm-c-source-buffers-list'"))

(defvar helm-occur-mode-line
  "\\<helm-map>\
\\[helm-help]:Help,\
\\<helm-occur-map>\
\\[helm-occur-run-query-replace-regexp]:Query replace regexp,\
\\<helm-map>\
\\[helm-select-action]:Acts,\
\\[helm-exit-minibuffer]/\\[helm-select-2nd-action-or-end-of-line]/\
\\[helm-select-3rd-action]:NthAct,\
\\[helm-send-bug-report-from-helm]:BugReport.")


;;;; <File>
;;
;;
;;; File Cache
(defvar helm-c-file-cache-initialized-p nil)

(defvar helm-c-file-cache-files nil)

(defvar helm-c-source-file-cache
  `((name . "File Cache")
    (init
     . (lambda ()
         (require 'filecache nil t)
         (unless helm-c-file-cache-initialized-p
           (setq helm-c-file-cache-files
                 (loop for item in file-cache-alist append
                       (destructuring-bind (base &rest dirs) item
                         (loop for dir in dirs collect
                               (concat dir base)))))
           (defadvice file-cache-add-file (after file-cache-list activate)
             (add-to-list 'helm-c-file-cache-files (expand-file-name file)))
           (setq helm-c-file-cache-initialized-p t))))
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (candidates . helm-c-file-cache-files)
    (match helm-c-match-on-basename)
    (type . file)))


;;; Recentf files
;;
;;
(defvar helm-c-source-recentf
  `((name . "Recentf")
    (init . (lambda ()
              (require 'recentf)
              (or recentf-mode (recentf-mode 1))))
    ;; Needed for filenames with capitals letters.
    (disable-shortcuts)
    (candidates . recentf-list)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (match helm-c-match-on-basename)
    (type . file))
  "See (info \"(emacs)File Conveniences\").
Set `recentf-max-saved-items' to a bigger value if default is too small.")

;;; ffap
(eval-when-compile (require 'ffap))
(defvar helm-c-source-ffap-guesser
  `((name . "File at point")
    (init . (lambda () (require 'ffap)))
    (candidates . (lambda ()
                    (helm-aif
                        (with-helm-current-buffer
                          (ffap-guesser))
                        (list it))))
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file)))

;;; ffap with line number
(defun helm-c-ffap-file-line-at-point ()
  "Get (FILENAME . LINENO) at point."
  (helm-aif (let (ffap-alist) (ffap-file-at-point))
      (save-excursion
        (beginning-of-line)
        (when (and (search-forward it nil t)
                   (looking-at ":\\([0-9]+\\)"))
          (cons it (string-to-number (match-string 1)))))))

(defun helm-c-ffap-line-candidates ()
  (with-helm-current-buffer
    (helm-attrset 'ffap-line-location (helm-c-ffap-file-line-at-point)))
  (helm-aif (helm-attr 'ffap-line-location)
      (destructuring-bind (file . line) it
        (list (cons (format "%s (line %d)" file line) file)))))

;;; Goto line after opening file by `helm-c-source-ffap-line'.
(defun helm-c-ffap-line-goto-line ()
  (when (car (helm-attr 'ffap-line-location))
    (unwind-protect
         (ignore-errors
           (with-selected-window
               (get-buffer-window
                (get-file-buffer (car (helm-attr 'ffap-line-location))))
             (helm-goto-line (cdr (helm-attr 'ffap-line-location)))))
      (helm-attrset 'ffap-line-location nil))))
(add-hook 'helm-after-action-hook 'helm-c-ffap-line-goto-line)
(add-hook 'helm-after-persistent-action-hook 'helm-c-ffap-line-goto-line)

(defvar helm-c-source-ffap-line
  `((name . "File/Lineno at point")
    (init . (lambda () (require 'ffap)))
    (candidates . helm-c-ffap-line-candidates)
    (keymap . ,helm-map)
    (type . file)))

;;; list of files gleaned from every dired buffer
(defun helm-c-files-in-all-dired-candidates ()
  (save-excursion
    (mapcan
     (lambda (dir)
       (cond ((listp dir)               ;filelist
              dir)
             ((equal "" (file-name-nondirectory dir)) ;dir
              (directory-files dir t))
             (t                         ;wildcard
              (file-expand-wildcards dir t))))
     (delq nil
           (mapcar (lambda (buf)
                     (set-buffer buf)
                     (when (eq major-mode 'dired-mode)
                       (if (consp dired-directory)
                           (cdr dired-directory) ;filelist
                           dired-directory))) ;dir or wildcard
                   (buffer-list))))))
;; (dired '("~/" "~/.emacs-custom.el" "~/.emacs.bmk"))

(defvar helm-c-source-files-in-all-dired
  '((name . "Files in all dired buffer.")
    (candidates . helm-c-files-in-all-dired-candidates)
    (type . file)))


;;; Visible Bookmarks
;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")


;; http://d.hatena.ne.jp/grandVin/20080911/1221114327
(defvar helm-c-source-bm
  '((name . "Visible Bookmarks")
    (init . helm-c-bm-init)
    (candidates-in-buffer)
    (type . line))
  "Needs bm.el.

http://www.nongnu.org/bm/")

(defun helm-c-bm-init ()
  "Init function for `helm-c-source-bm'."
  (when (require 'bm nil t)
    (with-no-warnings
      (let ((bookmarks (bm-lists))
            (buf (helm-candidate-buffer 'global)))
        (dolist (bm (sort* (append (car bookmarks) (cdr bookmarks))
                           '< :key 'overlay-start))
          (let ((start (overlay-start bm))
                (end (overlay-end bm))
                (annotation (or (overlay-get bm 'annotation) "")))
            (unless (< (- end start) 1) ; org => (if (< (- end start) 2)
              (let ((str (format "%5d: [%s]: %s\n"
                                 (line-number-at-pos start)
                                 annotation
                                 (buffer-substring start (1- end)))))
                (with-current-buffer buf (insert str))))))))))


;;;; <Library>
;;; Elisp library scan
;;
;;
(defvar helm-c-source-elisp-library-scan
  '((name . "Elisp libraries (Scan)")
    (init . (helm-c-elisp-library-scan-init))
    (candidates-in-buffer)
    (action ("Find library"
             . (lambda (candidate) (find-file (find-library-name candidate))))
     ("Find library other window"
      . (lambda (candidate)
          (find-file-other-window (find-library-name candidate))))
     ("Load library"
      . (lambda (candidate) (load-library candidate))))))

(defun helm-c-elisp-library-scan-init ()
  "Init helm buffer status."
  (let ((helm-buffer (helm-candidate-buffer 'global))
        (library-list (helm-c-elisp-library-scan-list)))
    (with-current-buffer helm-buffer
      (dolist (library library-list)
        (insert (format "%s\n" library))))))

(defun helm-c-elisp-library-scan-list (&optional dirs string)
  "Do completion for file names passed to `locate-file'.
DIRS is directory to search path.
STRING is string to match."
  ;; Use `load-path' as path when ignore `dirs'.
  (or dirs (setq dirs load-path))
  ;; Init with blank when ignore `string'.
  (or string (setq string ""))
  ;; Get library list.
  (let ((string-dir (file-name-directory string))
        ;; File regexp that suffix match `load-file-rep-suffixes'.
        (match-regexp (format "^.*\\.el%s$" (regexp-opt load-file-rep-suffixes)))
        name
        names)
    (dolist (dir dirs)
      (unless dir
        (setq dir default-directory))
      (if string-dir
          (setq dir (expand-file-name string-dir dir)))
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions
                       (file-name-nondirectory string) dir))
          ;; Suffixes match `load-file-rep-suffixes'.
          (setq name (if string-dir (concat string-dir file) file))
          (if (string-match match-regexp name)
              (add-to-list 'names name)))))
    names))


;;; Semantic
;;
;;
(defvar helm-semantic-candidates nil)

(defun helm-semantic-construct-candidates (tags depth)
  (when (require 'semantic nil t)
    (apply
     'append
     (mapcar
      (lambda (tag)
        (if (listp tag)
            (let ((type (semantic-tag-type tag))
                  (class (semantic-tag-class tag)))
              (if (or (and (stringp type)
                           (or (string= type "class")
                               (string= type "namespace")))
                      (eq class 'function)
                      (eq class 'variable))
                  (cons (cons (concat (make-string (* depth 2) ?\s)
                                      (semantic-format-tag-summarize tag nil t))
                              tag)
                        (helm-semantic-construct-candidates
                         (semantic-tag-components tag) (1+ depth)))))))
      tags))))

(defun helm-semantic-default-action (candidate)
  (let ((tag (cdr (assoc candidate helm-semantic-candidates))))
    (semantic-go-to-tag tag)))

(defvar helm-c-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (setq helm-semantic-candidates
                    (ignore-errors (helm-semantic-construct-candidates
                                    (semantic-fetch-tags) 0)))))
    (candidates . (lambda ()
                    (if helm-semantic-candidates
                        (mapcar 'car helm-semantic-candidates))))
    (persistent-action . (lambda (elm)
                           (helm-semantic-default-action elm)
                           (helm-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . helm-semantic-default-action)
    "Needs semantic in CEDET.

http://cedet.sourceforge.net/semantic.shtml
http://cedet.sourceforge.net/"))


;;; Call source.
(defvar helm-source-select-buffer "*helm source select*")
(defvar helm-c-source-call-source
  `((name . "Call helm source")
    (candidate-number-limit)
    (candidates
     . (lambda ()
         (loop for vname in (all-completions "helm-c-source-" obarray)
               for var = (intern vname)
               for name = (ignore-errors (assoc-default 'name (symbol-value var)))
               if name collect
               (cons (format "%s `%s'"
                             name (propertize vname 'face 'font-lock-variable-name-face))
                     var))))
    (action
     . (("Invoke helm with selected source"
         .
         (lambda (candidate)
           (setq helm-candidate-number-limit 9999)
           (helm candidate nil nil nil nil
                 helm-source-select-buffer)))
        ("Describe variable" . describe-variable)
        ("Find variable" . find-variable)))
    (persistent-action . describe-variable)
    (persistent-help . "Show description of this source")))

(defun helm-call-source-from-helm ()
  "Call helm source within `helm' session."
  (interactive)
  (setq helm-input-idle-delay 0)
  (helm-set-sources '(helm-c-source-call-source)))



;;; Helm browse code.
(defun helm-c-browse-code-get-line (beg end)
  "Select line if it match the regexp corresponding to current `major-mode'.
Line is parsed for BEG position to END position."
  (let ((str-line (buffer-substring beg end))
        (regexp   (assoc-default major-mode
                                 helm-c-browse-code-regexp-alist))
        (num-line (if (string= helm-pattern "") beg (1- beg))))
    (when (and regexp (string-match regexp str-line))
      (format "%4d:%s" (line-number-at-pos num-line) str-line))))


(defvar helm-c-source-browse-code
  '((name . "Browse code")
    (init . (lambda ()
              (helm-candidate-buffer helm-current-buffer)
              (with-helm-current-buffer
                (jit-lock-fontify-now))))
    (candidate-number-limit . 9999)
    (candidates-in-buffer)
    (get-line . helm-c-browse-code-get-line)
    (type . line)
    (recenter)))


;; Do many actions for input
(defvar helm-c-source-create
  '((name . "Create")
    (dummy)
    (action)
    (action-transformer . helm-create--actions))
  "Do many create actions from `helm-pattern'.
See also `helm-create--actions'.")

(defun helm-create-from-helm ()
  "Run `helm-create' from `helm' as a fallback."
  (interactive)
  (helm-run-after-quit 'helm-create nil helm-pattern))

(defun helm-create--actions (&rest ignored)
  "Default actions for `helm-create' / `helm-c-source-create'."
  (remove-if-not
   (lambda (pair) (and (consp pair) (functionp (cdr pair))))
   (append helm-create--actions-private
           '(("find-file" . find-file)
             ("find-file other window" . find-file-other-window)
             ("New buffer" . helm-c-switch-to-buffer)
             ("New buffer other window" . switch-to-buffer-other-window)
             ("Bookmark Set" . bookmark-set)
             ("Set Register" .
              (lambda (x) (set-register (read-char "Register: ") x)))
             ("Insert Linkd star" . linkd-insert-star)
             ("Insert Linkd Tag" . linkd-insert-tag)
             ("Insert Linkd Link" . linkd-insert-link)
             ("Insert Linkd Lisp" . linkd-insert-lisp)
             ("Insert Linkd Wiki" . linkd-insert-wiki)
             ("Google Search" . google)))))


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


;;; Elscreen
;;
;;
(defun helm-find-buffer-on-elscreen (candidate)
  "Open buffer in new screen, if marked buffers open all in elscreens."
  (helm-require-or-error 'elscreen 'helm-find-buffer-on-elscreen)
  (helm-aif (helm-marked-candidates)
      (dolist (i it)
        (let ((target-screen (elscreen-find-screen-by-buffer
                              (get-buffer i) 'create)))
          (elscreen-goto target-screen)))
    (let ((target-screen (elscreen-find-screen-by-buffer
                          (get-buffer candidate) 'create)))
      (elscreen-goto target-screen))))

(defun helm-elscreen-find-file (file)
  (helm-require-or-error 'elscreen 'helm-elscreen-find-file)
  (elscreen-find-file file))

(defvar helm-c-source-elscreen
  '((name . "Elscreen")
    (candidates
     . (lambda ()
         (if (cdr (elscreen-get-screen-to-name-alist))
             (sort
              (loop for sname in (elscreen-get-screen-to-name-alist)
                    append (list (format "[%d] %s" (car sname) (cdr sname))))
              #'(lambda (a b) (compare-strings a nil nil b nil nil))))))
    (action
     . (("Change Screen" .
                         (lambda (candidate)
                           (elscreen-goto (- (aref candidate 1) (aref "0" 0)))))
        ("Kill Screen(s)" .
                          (lambda (candidate)
                            (dolist (i (helm-marked-candidates))
                              (elscreen-goto (- (aref i 1) (aref "0" 0)))
                              (elscreen-kill))))
        ("Only Screen" .
                       (lambda (candidate)
                         (elscreen-goto (- (aref candidate 1) (aref "0" 0)))
                         (elscreen-kill-others)))))))


;;; Generic action functions
;;
;;
(helm-document-attribute 'default-directory "type . file-line"
  "`default-directory' to interpret file.")
(helm-document-attribute 'before-jump-hook "type . file-line / line"
  "Function to call before jumping to the target location.")
(helm-document-attribute 'after-jump-hook "type . file-line / line"
  "Function to call after jumping to the target location.")
(helm-document-attribute 'adjust "type . file-line"
  "Search around line matching line contents.")
(helm-document-attribute 'recenter "type . file-line / line"
  "`recenter' after jumping.")
(helm-document-attribute 'target-file "type . line"
  "Goto line of target-file.")

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

;;;###autoload
(defun helm-c-set-variable (var)
  "Set value to VAR interactively."
  (interactive)
  (let ((sym (helm-c-symbolify var)))
    (set sym (eval-minibuffer (format "Set %s: " var)
                              (prin1-to-string (symbol-value sym))))))
;; (setq hh 12)
;; (helm-c-set-variable 'hh)


;;; Actions Transformers
;;
;;
;;; Files
(defun helm-c-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file.  Else return ACTIONS unmodified."
  (if (member (file-name-extension candidate) '("el" "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
      actions))

(defun helm-c-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file or URL.  Else return ACTIONS unmodified."
  (let ((browse-action '("Browse with Browser" . browse-url)))
    (cond ((string-match "^http\\|^ftp" candidate)
           (cons browse-action actions))
          ((string-match "\\.html?$" candidate)
           (append actions (list browse-action)))
          (t actions))))

;;; Function
(defun helm-c-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command.  Else return ACTIONS unmodified."
  (if (commandp (intern-soft candidate))
      (append actions '(("Call Interactively"
                         .
                         helm-c-call-interactively)))
      actions))

;;;; S-Expressions
(defun helm-c-transform-sexp-eval-command-sexp (actions candidate)
  "If CANDIDATE's `car' is a command, then add an action to
evaluate it and put it onto the `command-history'."
  (if (commandp (car (read candidate)))
      ;; Make it first entry
      (cons '("Eval and put onto command-history" .
              (lambda (sexp)
                (let ((sym (read sexp)))
                  (eval sym)
                  (setq command-history
                        (cons sym command-history)))))
            actions)
      actions))


;;; Functions
(defun helm-c-mark-interactive-functions (functions)
  "Mark interactive functions (commands) with (i) after the function name."
  (let (list)
    (loop for function in functions
          do (push (cons (concat function
                                 (when (commandp (intern-soft function)) " (i)"))
                         function)
                   list)
          finally (return (nreverse list)))))


;;; Outliner
;;
;;
(defvar helm-outline-goto-near-line-flag t)
(defvar helm-outline-using nil)
(defun helm-after-update-hook--outline ()
  (if (and (eq helm-outline-using t)
           (eq helm-outline-goto-near-line-flag t))
      (helm-outline-goto-near-line)))
(add-hook 'helm-after-update-hook 'helm-after-update-hook--outline)

(defun helm-outline-goto-near-line ()
  (with-helm-window
    ;; TODO need consideration whether to update position by every input.
    (when t ; (equal helm-pattern "")
      (helm-goto-line 2)
      (let ((lineno (with-helm-current-buffer
                      (line-number-at-pos (car helm-current-position)))))
        (block exit
          (while (<= (progn (skip-chars-forward " ")
                            (or (number-at-point) lineno))
                     lineno)
            (forward-line 1)
            (when (eobp)
              (forward-line -1)
              (return-from exit))))
        (forward-line -1)
        (and (bobp) (forward-line 1))
        (and (helm-pos-header-line-p) (forward-line -2))
        (helm-mark-current-line)))))


;;; Type Attributes
;;
;;
(define-helm-type-attribute 'buffer
    `((action
       ("Switch to buffer" . helm-c-switch-to-buffer)
       ,(and (locate-library "popwin") '("Switch to buffer in popup window" . popwin:popup-buffer))
       ("Switch to buffer other window" . switch-to-buffer-other-window)
       ("Switch to buffer other frame" . switch-to-buffer-other-frame)
       ,(and (locate-library "elscreen") '("Display buffer in Elscreen" . helm-find-buffer-on-elscreen))
       ("Query replace regexp" . helm-c-buffer-query-replace-regexp)
       ("Query replace" . helm-c-buffer-query-replace)
       ("View buffer" . view-buffer)
       ("Display buffer"   . display-buffer)
       ("Grep buffers (C-u grep all buffers)" . helm-c-zgrep-buffers)
       ("Revert buffer(s)" . helm-revert-marked-buffers)
       ("Insert buffer" . insert-buffer)
       ("Kill buffer(s)" . helm-kill-marked-buffers)
       ("Diff with file" . diff-buffer-with-file)
       ("Ediff Marked buffers" . helm-ediff-marked-buffers)
       ("Ediff Merge marked buffers" . (lambda (candidate)
                                         (helm-ediff-marked-buffers candidate t))))
      (persistent-help . "Show this buffer")
      (candidate-transformer helm-c-skip-boring-buffers
                             helm-c-transform-buffer-display-string))
  "Buffer or buffer name.")

(define-helm-type-attribute 'file
    `((action
       ("Find file" . helm-find-many-files)
       ,(and (locate-library "popwin") '("Find file in popup window" . popwin:find-file))
       ("Find file as root" . helm-find-file-as-root)
       ("Find file other window" . find-file-other-window)
       ("Find file other frame" . find-file-other-frame)
       ("Open dired in file's directory" . helm-c-open-dired)
       ("Grep File(s) `C-u recurse'" . helm-find-files-grep)
       ("Zgrep File(s) `C-u Recurse'" . helm-ff-zgrep)
       ("Pdfgrep File(s)" . helm-ff-pdfgrep)
       ("Checksum File" . helm-ff-checksum)
       ("Ediff File" . helm-find-files-ediff-files)
       ("Ediff Merge File" . helm-find-files-ediff-merge-files)
       ("View file" . view-file)
       ("Insert file" . insert-file)
       ("Delete file(s)" . helm-delete-marked-files)
       ("Open file externally (C-u to choose)" . helm-c-open-file-externally)
       ("Open file with default tool" . helm-c-open-file-with-default-tool)
       ("Find file in hex dump" . hexl-find-file))
      (persistent-help . "Show this file")
      (action-transformer helm-c-transform-file-load-el
                          helm-c-transform-file-browse-url)
      (candidate-transformer helm-c-w32-pathname-transformer
                             helm-c-skip-current-file
                             helm-c-skip-boring-files
                             helm-c-shorten-home-path))
  "File name.")

(let ((actions '(("Describe command" . describe-function)
                 ("Add command to kill ring" . helm-c-kill-new)
                 ("Go to command's definition" . find-function)
                 ("Debug on entry" . debug-on-entry)
                 ("Cancel debug on entry" . cancel-debug-on-entry)
                 ("Trace function" . trace-function)
                 ("Trace function (background)" . trace-function-background)
                 ("Untrace function" . untrace-function))))
  (define-helm-type-attribute 'command
      `((action ("Call interactively" . helm-c-call-interactively)
                ,@actions)
        (coerce . helm-c-symbolify)
        (persistent-action . describe-function))
    "Command. (string or symbol)")

  (define-helm-type-attribute 'function
      `((action . ,actions)
        (action-transformer helm-c-transform-function-call-interactively)
        (candidate-transformer helm-c-mark-interactive-functions)
        (coerce . helm-c-symbolify))
    "Function. (string or symbol)"))

(define-helm-type-attribute 'variable
    '((action ("Describe variable" . describe-variable)
       ("Add variable to kill ring" . helm-c-kill-new)
       ("Go to variable's definition" . find-variable)
       ("Set variable" . helm-c-set-variable))
      (coerce . helm-c-symbolify))
  "Variable.")

(define-helm-type-attribute 'sexp
    '((action ("Eval s-expression" . (lambda (c) (eval (read c))))
       ("Add s-expression to kill ring" . kill-new))
      (action-transformer helm-c-transform-sexp-eval-command-sexp))
  "String representing S-Expressions.")

(define-helm-type-attribute 'bookmark
    `((coerce . helm-bookmark-get-bookmark-from-name)
      (action
       ("Jump to bookmark" . helm-c-bookmark-jump)
       ("Jump to BM other window" . bookmark-jump-other-window)
       ("Bookmark edit annotation" . bookmark-edit-annotation)
       ("Bookmark show annotation" . bookmark-show-annotation)
       ("Delete bookmark(s)" . helm-delete-marked-bookmarks)
       ,@(and (locate-library "bookmark-extensions")
              `(("Edit Bookmark" . bmkext-edit-bookmark)))
       ("Rename bookmark" . bookmark-rename)
       ("Relocate bookmark" . bookmark-relocate))
      (keymap . ,helm-c-bookmark-map)
      (mode-line . helm-bookmark-mode-line-string))
  "Bookmark name.")

(define-helm-type-attribute 'line
    '((display-to-real . helm-c-display-to-real-line)
      (action ("Go to Line" . helm-c-action-line-goto)))
  "LINENO:CONTENT string, eg. \"  16:foo\".

Optional `target-file' attribute is a name of target file.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.")

(define-helm-type-attribute 'file-line
    `((filtered-candidate-transformer helm-c-filtered-candidate-transformer-file-line)
      (multiline)
      (action ("Go to" . helm-c-action-file-line-goto)))
  "FILENAME:LINENO:CONTENT string, eg. \"~/.emacs:16:;; comment\".

Optional `default-directory' attribute is a default-directory
FILENAME is interpreted.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.")

(define-helm-type-attribute 'timer
    '((real-to-display . helm-c-timer-real-to-display)
      (action ("Cancel Timer" . cancel-timer)
       ("Describe Function" . (lambda (tm) (describe-function (timer--function tm))))
       ("Find Function" . (lambda (tm) (find-function (timer--function tm)))))
      (persistent-action . (lambda (tm) (describe-function (timer--function tm))))
      (persistent-help . "Describe Function"))
  "Timer.")


;;; Preconfigured Helm
;;
;;
;;;###autoload
(defun helm-mini ()
  "Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (helm-other-buffer '(helm-c-source-buffers-list
                       helm-c-source-recentf
                       helm-c-source-buffer-not-found)
                     "*helm mini*"))
;;;###autoload
(defun helm-for-files ()
  "Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-prefered-list'."
  (interactive)
  (helm-other-buffer helm-for-files-prefered-list "*helm for files*"))

;;;###autoload
(defun helm-recentf ()
  "Preconfigured `helm' for `recentf'."
  (interactive)
  (helm-other-buffer 'helm-c-source-recentf "*helm recentf*"))

;;;###autoload
(defun helm-minibuffer-history ()
  "Preconfigured `helm' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (helm-other-buffer 'helm-c-source-minibuffer-history
                       "*helm minibuffer-history*")))

;;;###autoload
(defun helm-bm-list ()
  "Preconfigured `helm' for visible bookmarks.

Needs bm.el

http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el"
  (interactive)
  (let ((helm-outline-using t))
    (helm-other-buffer 'helm-c-source-bm "*helm bm list*")))

;;;###autoload
(defun helm-browse-code ()
  "Preconfigured helm to browse code."
  (interactive)
  (helm :sources 'helm-c-source-browse-code
        :buffer "*helm browse code*"
        :default (thing-at-point 'symbol)))

;;;###autoload
(defun helm-call-source ()
  "Preconfigured `helm' to call helm source."
  (interactive)
  (helm :sources 'helm-c-source-call-source
        :buffer helm-source-select-buffer))

;;;###autoload
(defun helm-create (&optional string initial-input)
  "Preconfigured `helm' to do many create actions from STRING.
See also `helm-create--actions'."
  (interactive)
  (setq string (or string (read-string "Create Helm: " initial-input)))
  (helm :sources '(((name . "Helm Create")
                    (header-name . (lambda (_) (format "Action for \"%s\"" string)))
                    (candidates . helm-create--actions)
                    (candidate-number-limit)
                    (action . (lambda (func) (funcall func string)))))))



;;; Unit tests are now in ../developer-tools/unit-test-helm-config.el.

(provide 'helm-config)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-config.el ends here
