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
(require 'easymenu)


(defgroup helm-config nil
  "Various configurations for Helm."
  :group 'helm)

(defcustom helm-command-prefix-key "C-x c"
  "The key `helm-command-prefix' is bound to in the global map."
  :type '(choice (string :tag "Key") (const :tag "no binding"))
  :group 'helm-config
  :set
  (lambda (var key)
    (when (and (boundp var) (symbol-value var))
      (define-key (current-global-map)
        (read-kbd-macro (symbol-value var)) nil))
    (when key
      (define-key (current-global-map)
        (read-kbd-macro key) 'helm-command-prefix))
    (set var key)))

;;; Command Keymap
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
    (define-key map (kbd "8")         'helm-ucs)
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
(easy-menu-add-item nil '("Tools") 
 '("Helm"
  ["Find any Files/Buffers" helm-for-files t]
  ["Helm Everywhere (Toggle)" helm-mode t]
  "----"
  ("Files"
   ["Find files" helm-find-files t]
   ["Recent Files" helm-recentf t]
   ["Locate" helm-locate t]
   ["Bookmarks" helm-c-pp-bookmarks t])
  ("Buffers"
   ["Find buffers" helm-buffers-list t])
  ("Commands"
   ["Emacs Commands" helm-M-x t]
   ["Externals Commands" helm-c-run-external-command t])
  ("Help"
   ["Helm Apropos" helm-c-apropos t])
  ("Info"
   ["Info at point" helm-info-at-point t]
   ["Emacs Manual index" helm-info-emacs t]
   ["Gnus Manual index" helm-info-gnus t])
  ("Org"
   ["Org keywords" helm-org-keywords t]
   ["Org headlines" helm-org-headlines t])
  ("Tools"
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
  ["Prefered Options" helm-configuration t])
 "Spell Checking")

(easy-menu-add-item nil '("Tools") '("----") "Spell Checking")

;;;###autoload
(defun helm-configuration ()
  "Customize `helm'."
  (interactive)
  (customize-group "helm"))


;;; Start of automatically extracted autoloads.


;;;### (autoloads (helm-send-bug-report-from-helm helm-send-bug-report
;;;;;;  helm-follow-mode helm-kill-selection-and-quit helm-yank-selection
;;;;;;  helm-prev-visible-mark helm-next-visible-mark helm-display-all-visible-marks
;;;;;;  helm-toggle-all-marks helm-unmark-all helm-mark-all helm-toggle-visible-mark
;;;;;;  helm-scroll-other-window-down helm-scroll-other-window helm-execute-persistent-action
;;;;;;  helm-select-2nd-action-or-end-of-line helm-select-4th-action
;;;;;;  helm-select-3rd-action helm-select-2nd-action helm-enlarge-window
;;;;;;  helm-narrow-window helm-toggle-resplit-window helm-delete-minibuffer-contents
;;;;;;  helm-delete-current-selection helm-debug-output helm-keyboard-quit
;;;;;;  helm-exit-minibuffer helm-confirm-and-exit-minibuffer helm-next-source
;;;;;;  helm-previous-source helm-end-of-buffer helm-beginning-of-buffer
;;;;;;  helm-next-page helm-previous-page helm-next-line helm-previous-line
;;;;;;  helm-select-action helm-force-update helm-other-buffer helm-at-point
;;;;;;  helm-resume-window-only helm-resume helm-open-last-log) "helm"
;;;;;;  "helm.el" (20351 61929))
;;; Generated autoloads from helm.el

(autoload 'helm-open-last-log "helm" "\
Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to \"*Helm Log*\" buffer.

\(fn)" t nil)

(autoload 'helm-resume "helm" "\
Resurrect previously invoked `helm'.
Called with a prefix arg, allow choosing among all existing
helm buffers.  i.e choose among various helm sessions.

\(fn &optional (any-buffer helm-last-buffer) BUFFER-PATTERN (any-resume t))" t nil)

(autoload 'helm-resume-window-only "helm" "\


\(fn &optional (any-buffer helm-last-buffer) BUFFER-PATTERN)" t nil)

(autoload 'helm-at-point "helm" "\
Call helm with symbol at point as initial input.
ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT and ANY-BUFFER
are same args as in `helm'.

\(fn &optional ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER)" t nil)

(autoload 'helm-other-buffer "helm" "\
Simplified interface of `helm' with other `helm-buffer'.
Call `helm' with only ANY-SOURCES and ANY-BUFFER as args.

\(fn ANY-SOURCES ANY-BUFFER)" nil nil)

(autoload 'helm-force-update "helm" "\
Force recalculation and update of candidates.
If current source has `update' attribute, a function without argument,
call it before update.

\(fn &optional PRESELECT)" t nil)

(autoload 'helm-select-action "helm" "\
Select an action for the currently selected candidate.
If action buffer is selected, back to the helm buffer.

\(fn)" t nil)

(autoload 'helm-previous-line "helm" "\
Move selection to the previous line.

\(fn)" t nil)

(autoload 'helm-next-line "helm" "\
Move selection to the next line.

\(fn)" t nil)

(autoload 'helm-previous-page "helm" "\
Move selection back with a pageful.

\(fn)" t nil)

(autoload 'helm-next-page "helm" "\
Move selection forward with a pageful.

\(fn)" t nil)

(autoload 'helm-beginning-of-buffer "helm" "\
Move selection at the top.

\(fn)" t nil)

(autoload 'helm-end-of-buffer "helm" "\
Move selection at the bottom.

\(fn)" t nil)

(autoload 'helm-previous-source "helm" "\
Move selection to the previous source.

\(fn)" t nil)

(autoload 'helm-next-source "helm" "\
Move selection to the next source.

\(fn)" t nil)

(autoload 'helm-confirm-and-exit-minibuffer "helm" "\
Maybe ask for confirmation when exiting helm.
It is similar to `minibuffer-complete-and-exit' adapted to helm.
If `minibuffer-completion-confirm' value is 'confirm,
send in minibuffer confirm message and exit on next hit.
If `minibuffer-completion-confirm' value is t,
don't exit and send message 'no match'.

\(fn)" t nil)

(autoload 'helm-exit-minibuffer "helm" "\
Select the current candidate by exiting the minibuffer.

\(fn)" t nil)

(autoload 'helm-keyboard-quit "helm" "\
Quit minibuffer in helm.
If action buffer is displayed, kill it.

\(fn)" t nil)

(autoload 'helm-debug-output "helm" "\
Show all helm-related variables at this time.

\(fn)" t nil)

(autoload 'helm-delete-current-selection "helm" "\
Delete the currently selected item.

\(fn)" t nil)

(autoload 'helm-delete-minibuffer-contents "helm" "\
Same as `delete-minibuffer-contents' but this is a command.

\(fn)" t nil)

(autoload 'helm-toggle-resplit-window "helm" "\
Toggle resplit helm window, vertically or horizontally.

\(fn)" t nil)

(autoload 'helm-narrow-window "helm" "\
Narrow helm window.

\(fn)" t nil)

(autoload 'helm-enlarge-window "helm" "\
Enlarge helm window.

\(fn)" t nil)

(autoload 'helm-select-2nd-action "helm" "\
Select the 2nd action for the currently selected candidate.

\(fn)" t nil)

(autoload 'helm-select-3rd-action "helm" "\
Select the 3rd action for the currently selected candidate.

\(fn)" t nil)

(autoload 'helm-select-4th-action "helm" "\
Select the 4th action for the currently selected candidate.

\(fn)" t nil)

(autoload 'helm-select-2nd-action-or-end-of-line "helm" "\
Select the 2nd action for the currently selected candidate.
This happen when point is at the end of minibuffer.
Otherwise goto the end of minibuffer.

\(fn)" t nil)

(autoload 'helm-execute-persistent-action "helm" "\
Perform the associated action ATTR without quitting helm.
ATTR default is 'persistent-action', but it can be helm else.
In this case you have to add this new attribute to your source.
When `helm-samewindow' and ONEWINDOW are non--nil,
the helm window is never split in persistent action.

\(fn &optional (attr (quote persistent-action)) ONEWINDOW)" t nil)

(autoload 'helm-scroll-other-window "helm" "\
Scroll other window (not *Helm* window) upward.

\(fn)" t nil)

(autoload 'helm-scroll-other-window-down "helm" "\
Scroll other window (not *Helm* window) downward.

\(fn)" t nil)

(autoload 'helm-toggle-visible-mark "helm" "\
Toggle helm visible mark at point.

\(fn)" t nil)

(autoload 'helm-mark-all "helm" "\
Mark all visible unmarked candidates in current source.

\(fn)" t nil)

(autoload 'helm-unmark-all "helm" "\
Unmark all candidates in all sources of current helm session.

\(fn)" t nil)

(autoload 'helm-toggle-all-marks "helm" "\
Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current helm session

\(fn)" t nil)

(autoload 'helm-display-all-visible-marks "helm" "\
Show all `helm' visible marks strings.
Only useful for debugging.

\(fn)" t nil)

(autoload 'helm-next-visible-mark "helm" "\
Move next helm visible mark.
If PREV is non-nil move to precedent.

\(fn &optional PREV)" t nil)

(autoload 'helm-prev-visible-mark "helm" "\
Move previous helm visible mark.

\(fn)" t nil)

(autoload 'helm-yank-selection "helm" "\
Set minibuffer contents to current selection.

\(fn)" t nil)

(autoload 'helm-kill-selection-and-quit "helm" "\
Store current selection to kill ring.
You can paste it by typing \\[yank].

\(fn)" t nil)

(autoload 'helm-follow-mode "helm" "\
Execute persistent action everytime the cursor is moved when enabled.

\(fn &optional ARG)" t nil)

(autoload 'helm-send-bug-report "helm" "\
Send a bug report of helm.el.

\(fn)" t nil)

(autoload 'helm-send-bug-report-from-helm "helm" "\
Send a bug report of helm.el in helm session.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-c-reset-adaptative-history) "helm-adaptative"
;;;;;;  "helm-adaptative.el" (20349 20936))
;;; Generated autoloads from helm-adaptative.el

(autoload 'helm-c-reset-adaptative-history "helm-adaptative" "\
Delete all `helm-c-adaptive-history' and his file.
Useful when you have a old or corrupted `helm-c-adaptive-history-file'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-apt) "helm-apt" "helm-apt.el" (20345 62057))
;;; Generated autoloads from helm-apt.el

(autoload 'helm-apt "helm-apt" "\
Preconfigured `helm' : frontend of APT package manager.
With a prefix arg reload cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-bbdb) "helm-bbdb" "helm-bbdb.el" (20345 62057))
;;; Generated autoloads from helm-bbdb.el

(autoload 'helm-bbdb "helm-bbdb" "\
Preconfigured `helm' for BBDB.

Needs BBDB.

http://bbdb.sourceforge.net/

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-bookmark-ext helm-c-bmkext-run-edit) "helm-bmkext"
;;;;;;  "helm-bmkext.el" (20345 62057))
;;; Generated autoloads from helm-bmkext.el

(autoload 'helm-c-bmkext-run-edit "helm-bmkext" "\
Run `bmkext-edit-bookmark' from keyboard.

\(fn)" t nil)

(autoload 'helm-bookmark-ext "helm-bmkext" "\
Preconfigured `helm' for bookmark-extensions sources.
Needs bookmark-ext.el:
<http://mercurial.intuxication.org/hg/emacs-bookmark-extension>.
Contain also `helm-c-source-google-suggest'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-c-pp-bookmarks helm-bookmarks helm-c-bookmark-run-delete
;;;;;;  helm-c-bookmark-run-jump-other-window) "helm-bookmark" "helm-bookmark.el"
;;;;;;  (20347 17170))
;;; Generated autoloads from helm-bookmark.el

(autoload 'helm-c-bookmark-run-jump-other-window "helm-bookmark" "\
Jump to bookmark from keyboard.

\(fn)" t nil)

(autoload 'helm-c-bookmark-run-delete "helm-bookmark" "\
Delete bookmark from keyboard.

\(fn)" t nil)

(autoload 'helm-bookmarks "helm-bookmark" "\
Preconfigured `helm' for bookmarks.

\(fn)" t nil)

(autoload 'helm-c-pp-bookmarks "helm-bookmark" "\
Preconfigured `helm' for bookmarks (pretty-printed).

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-buffers-list helm-buffer-run-ediff helm-buffer-switch-to-elscreen
;;;;;;  helm-buffer-switch-other-frame helm-buffer-switch-other-window
;;;;;;  helm-buffer-run-query-replace helm-buffer-run-query-replace-regexp
;;;;;;  helm-buffer-run-zgrep helm-buffer-run-grep helm-buffer-run-kill-buffers
;;;;;;  helm-buffer-save-persistent helm-buffer-revert-persistent
;;;;;;  helm-buffer-diff-persistent) "helm-buffers" "helm-buffers.el"
;;;;;;  (20345 62057))
;;; Generated autoloads from helm-buffers.el

(autoload 'helm-buffer-diff-persistent "helm-buffers" "\
Toggle diff buffer without quitting helm.

\(fn)" t nil)

(autoload 'helm-buffer-revert-persistent "helm-buffers" "\
Revert buffer without quitting helm.

\(fn)" t nil)

(autoload 'helm-buffer-save-persistent "helm-buffers" "\
Save buffer without quitting helm.

\(fn)" t nil)

(autoload 'helm-buffer-run-kill-buffers "helm-buffers" "\
Run kill buffer action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-grep "helm-buffers" "\
Run Grep action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-zgrep "helm-buffers" "\
Run Grep action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-query-replace-regexp "helm-buffers" "\
Run Query replace regexp action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-query-replace "helm-buffers" "\
Run Query replace action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-switch-other-window "helm-buffers" "\
Run switch to other window action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-switch-other-frame "helm-buffers" "\
Run switch to other frame action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-switch-to-elscreen "helm-buffers" "\
Run switch to elscreen  action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffer-run-ediff "helm-buffers" "\
Run ediff action from `helm-c-source-buffers-list'.

\(fn)" t nil)

(autoload 'helm-buffers-list "helm-buffers" "\
Preconfigured `helm' to list buffers.
It is an enhanced version of `helm-for-buffers'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-simple-call-tree) "helm-call-tree" "helm-call-tree.el"
;;;;;;  (20345 62057))
;;; Generated autoloads from helm-call-tree.el

(autoload 'helm-simple-call-tree "helm-call-tree" "\
Preconfigured `helm' for simple-call-tree. List function relationships.

Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-colors) "helm-color" "helm-color.el" (20351
;;;;;;  62676))
;;; Generated autoloads from helm-color.el

(autoload 'helm-colors "helm-color" "\
Preconfigured `helm' for color.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-M-x) "helm-command" "helm-command.el" (20347
;;;;;;  17170))
;;; Generated autoloads from helm-command.el

(autoload 'helm-M-x "helm-command" "\
Preconfigured `helm' for Emacs commands.
It is `helm' replacement of regular `M-x' `execute-extended-command'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-complex-command-history helm-timers helm-manage-advice
;;;;;;  helm-c-apropos helm-lisp-completion-or-file-name-at-point
;;;;;;  helm-lisp-completion-at-point-or-indent helm-c-complete-file-name-at-point
;;;;;;  helm-lisp-completion-at-point) "helm-elisp" "helm-elisp.el"
;;;;;;  (20345 62057))
;;; Generated autoloads from helm-elisp.el

(autoload 'helm-lisp-completion-at-point "helm-elisp" "\
Helm lisp symbol completion at point.

\(fn)" t nil)

(autoload 'helm-c-complete-file-name-at-point "helm-elisp" "\
Complete file name at point.

\(fn)" t nil)

(autoload 'helm-lisp-completion-at-point-or-indent "helm-elisp" "\
First call indent and second call complete lisp symbol.
The second call should happen before `helm-lisp-completion-or-indent-delay',
after this delay, next call will indent again.
After completion, next call is always indent.
See that like click and double mouse click.
One hit indent, two quick hits maybe indent and complete.

\(fn ARG)" t nil)

(autoload 'helm-lisp-completion-or-file-name-at-point "helm-elisp" "\
Complete lisp symbol or filename at point.
Filename completion happen if filename is started in
or between double quotes.

\(fn)" t nil)

(autoload 'helm-c-apropos "helm-elisp" "\
Preconfigured helm to describe commands, functions, variables and faces.

\(fn)" t nil)

(autoload 'helm-manage-advice "helm-elisp" "\
Preconfigured `helm' to disable/enable function advices.

\(fn)" t nil)

(autoload 'helm-timers "helm-elisp" "\
Preconfigured `helm' for timers.

\(fn)" t nil)

(autoload 'helm-complex-command-history "helm-elisp" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (helm-elscreen) "helm-elscreen" "helm-elscreen.el"
;;;;;;  (20345 62057))
;;; Generated autoloads from helm-elscreen.el

(autoload 'helm-elscreen "helm-elscreen" "\
Preconfigured helm to list elscreen.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-emms) "helm-emms" "helm-emms.el" (20345 62057))
;;; Generated autoloads from helm-emms.el

(autoload 'helm-emms "helm-emms" "\
Preconfigured `helm' for emms sources.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-eshell-history helm-esh-pcomplete) "helm-eshell"
;;;;;;  "helm-eshell.el" (20345 62057))
;;; Generated autoloads from helm-eshell.el

(autoload 'helm-esh-pcomplete "helm-eshell" "\
Preconfigured helm to provide helm completion in eshell.

\(fn)" t nil)

(autoload 'helm-eshell-history "helm-eshell" "\
Preconfigured helm for eshell history.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-calcul-expression helm-eval-expression-with-eldoc
;;;;;;  helm-eval-expression) "helm-eval" "helm-eval.el" (20347 59940))
;;; Generated autoloads from helm-eval.el

(autoload 'helm-eval-expression "helm-eval" "\
Preconfigured helm for `helm-c-source-evaluation-result'.

\(fn ARG)" t nil)

(autoload 'helm-eval-expression-with-eldoc "helm-eval" "\
Preconfigured helm for `helm-c-source-evaluation-result' with `eldoc' support. 

\(fn)" t nil)

(autoload 'helm-calcul-expression "helm-eval" "\
Preconfigured helm for `helm-c-source-calculation-result'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-c-run-external-command) "helm-external" "helm-external.el"
;;;;;;  (20345 62057))
;;; Generated autoloads from helm-external.el

(autoload 'helm-c-run-external-command "helm-external" "\
Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-c-external-commands-list'.

\(fn PROGRAM)" t nil)

;;;***

;;;### (autoloads (helm-recentf helm-for-files helm-dired-hardlink-file
;;;;;;  helm-dired-symlink-file helm-dired-copy-file helm-dired-rename-file
;;;;;;  helm-insert-file helm-write-file helm-find-files helm-c-copy-files-async
;;;;;;  helm-dired-mode helm-ff-rotate-right-persistent helm-ff-rotate-left-persistent
;;;;;;  helm-ff-run-kill-buffer-persistent helm-ff-persistent-delete
;;;;;;  helm-ff-properties-persistent helm-find-files-down-one-level
;;;;;;  helm-ff-run-toggle-basename helm-ff-run-print-file helm-ff-run-etags
;;;;;;  helm-ff-run-gnus-attach-files helm-ff-run-locate helm-ff-run-open-file-externally
;;;;;;  helm-ff-run-switch-other-frame helm-ff-run-switch-other-window
;;;;;;  helm-ff-run-switch-to-eshell helm-ff-run-complete-fn-at-point
;;;;;;  helm-ff-run-delete-file helm-ff-run-hardlink-file helm-ff-run-symlink-file
;;;;;;  helm-ff-run-ediff-merge-file helm-ff-run-ediff-file helm-ff-run-eshell-command-on-file
;;;;;;  helm-ff-run-load-file helm-ff-run-byte-compile-file helm-ff-run-rename-file
;;;;;;  helm-ff-run-copy-file helm-ff-run-zgrep helm-ff-run-pdfgrep
;;;;;;  helm-ff-run-grep helm-ff-run-switch-to-history helm-ff-run-toggle-auto-update)
;;;;;;  "helm-files" "helm-files.el" (20350 41523))
;;; Generated autoloads from helm-files.el

(autoload 'helm-ff-run-toggle-auto-update "helm-files" "\


\(fn)" t nil)

(autoload 'helm-ff-run-switch-to-history "helm-files" "\
Run Switch to history action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-grep "helm-files" "\
Run Grep action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-pdfgrep "helm-files" "\
Run Pdfgrep action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-zgrep "helm-files" "\
Run Grep action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-copy-file "helm-files" "\
Run Copy file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-rename-file "helm-files" "\
Run Rename file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-byte-compile-file "helm-files" "\
Run Byte compile file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-load-file "helm-files" "\
Run Load file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-eshell-command-on-file "helm-files" "\
Run eshell command on file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-ediff-file "helm-files" "\
Run Ediff file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-ediff-merge-file "helm-files" "\
Run Ediff merge file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-symlink-file "helm-files" "\
Run Symlink file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-hardlink-file "helm-files" "\
Run Hardlink file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-delete-file "helm-files" "\
Run Delete file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-complete-fn-at-point "helm-files" "\
Run complete file name action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-switch-to-eshell "helm-files" "\
Run switch to eshell action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-switch-other-window "helm-files" "\
Run switch to other window action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-switch-other-frame "helm-files" "\
Run switch to other frame action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-open-file-externally "helm-files" "\
Run open file externally command action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-locate "helm-files" "\
Run locate action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-gnus-attach-files "helm-files" "\
Run gnus attach files command action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-etags "helm-files" "\
Run Etags command action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-print-file "helm-files" "\
Run Print file action from `helm-c-source-find-files'.

\(fn)" t nil)

(autoload 'helm-ff-run-toggle-basename "helm-files" "\


\(fn)" t nil)

(autoload 'helm-find-files-down-one-level "helm-files" "\
Go down one level like unix command `cd ..'.
If prefix numeric arg is given go ARG level down.

\(fn ARG)" t nil)

(autoload 'helm-ff-properties-persistent "helm-files" "\
Show properties without quitting helm.

\(fn)" t nil)

(autoload 'helm-ff-persistent-delete "helm-files" "\
Delete current candidate without quitting.

\(fn)" t nil)

(autoload 'helm-ff-run-kill-buffer-persistent "helm-files" "\
Execute `helm-ff-kill-buffer-fname' whitout quitting.

\(fn)" t nil)

(autoload 'helm-ff-rotate-left-persistent "helm-files" "\
Rotate image left without quitting helm.

\(fn)" t nil)

(autoload 'helm-ff-rotate-right-persistent "helm-files" "\
Rotate image right without quitting helm.

\(fn)" t nil)

(defvar helm-dired-mode "Enable helm completion in Dired functions.\nBindings affected are C, R, S, H.\nThis is deprecated for Emacs24+ users, use `helm-mode' instead." "\
Non-nil if Helm-Dired mode is enabled.
See the command `helm-dired-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-dired-mode'.")

(custom-autoload 'helm-dired-mode "helm-files" nil)

(autoload 'helm-dired-mode "helm-files" "\
Toggle Helm-Dired mode on or off.
With a prefix argument ARG, enable Helm-Dired mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{helm-dired-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'helm-c-copy-files-async "helm-files" "\
Preconfigured helm to copy file list FLIST to DEST asynchronously.

\(fn)" t nil)

(autoload 'helm-find-files "helm-files" "\
Preconfigured `helm' for helm implementation of `find-file'.
Called with a prefix arg show history if some.
Don't call it from programs, use `helm-find-files-1' instead.
This is the starting point for nearly all actions you can do on files.

\(fn ARG)" t nil)

(autoload 'helm-write-file "helm-files" "\
Preconfigured `helm' providing completion for `write-file'.

\(fn)" t nil)

(autoload 'helm-insert-file "helm-files" "\
Preconfigured `helm' providing completion for `insert-file'.

\(fn)" t nil)

(autoload 'helm-dired-rename-file "helm-files" "\
Preconfigured `helm' to rename files from dired.

\(fn)" t nil)

(autoload 'helm-dired-copy-file "helm-files" "\
Preconfigured `helm' to copy files from dired.

\(fn)" t nil)

(autoload 'helm-dired-symlink-file "helm-files" "\
Preconfigured `helm' to symlink files from dired.

\(fn)" t nil)

(autoload 'helm-dired-hardlink-file "helm-files" "\
Preconfigured `helm' to hardlink files from dired.

\(fn)" t nil)

(autoload 'helm-for-files "helm-files" "\
Preconfigured `helm' for opening files.
Run all sources defined in `helm-for-files-prefered-list'.

\(fn)" t nil)

(autoload 'helm-recentf "helm-files" "\
Preconfigured `helm' for `recentf'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-firefox-bookmarks) "helm-firefox" "helm-firefox.el"
;;;;;;  (20347 17170))
;;; Generated autoloads from helm-firefox.el

(autoload 'helm-firefox-bookmarks "helm-firefox" "\
Preconfigured `helm' for firefox bookmark.
You will have to enable html bookmarks in firefox:
open about:config in firefox and double click on this line to enable value to true:

user_pref(\"browser.bookmarks.autoExportHTML\", false);

You should have now:

user_pref(\"browser.bookmarks.autoExportHTML\", true);

After closing firefox, you will be able to browse you bookmarks.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-ucs helm-select-xfont) "helm-font" "helm-font.el"
;;;;;;  (20345 62057))
;;; Generated autoloads from helm-font.el

(autoload 'helm-select-xfont "helm-font" "\
Preconfigured `helm' to select Xfont.

\(fn)" t nil)

(autoload 'helm-ucs "helm-font" "\
Preconfigured helm for `ucs-names' math symbols.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-gentoo) "helm-gentoo" "helm-gentoo.el" (20347
;;;;;;  17170))
;;; Generated autoloads from helm-gentoo.el

(autoload 'helm-gentoo "helm-gentoo" "\
Preconfigured `helm' for gentoo linux.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-do-pdfgrep helm-do-zgrep helm-do-grep helm-c-grep-run-save-buffer
;;;;;;  helm-c-grep-run-other-window-action helm-c-grep-run-default-action
;;;;;;  helm-c-grep-run-persistent-action helm-c-goto-next-file helm-c-goto-precedent-file)
;;;;;;  "helm-grep" "helm-grep.el" (20345 62057))
;;; Generated autoloads from helm-grep.el

(autoload 'helm-c-goto-precedent-file "helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-c-goto-next-file "helm-grep" "\
Go to precedent file in helm grep/etags buffers.

\(fn)" t nil)

(autoload 'helm-c-grep-run-persistent-action "helm-grep" "\
Run grep persistent action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-c-grep-run-default-action "helm-grep" "\
Run grep default action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-c-grep-run-other-window-action "helm-grep" "\
Run grep goto other window action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-c-grep-run-save-buffer "helm-grep" "\
Run grep save results action from `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-do-grep "helm-grep" "\
Preconfigured helm for grep.
Contrarily to Emacs `grep' no default directory is given, but
the full path of candidates in ONLY.
That allow to grep different files not only in `default-directory' but anywhere
by marking them (C-<SPACE>). If one or more directory is selected
grep will search in all files of these directories.
You can use also wildcard in the base name of candidate.
If a prefix arg is given use the -r option of grep.
The prefix arg can be passed before or after start.
See also `helm-do-grep-1'.

\(fn)" t nil)

(autoload 'helm-do-zgrep "helm-grep" "\
Preconfigured helm for zgrep.

\(fn)" t nil)

(autoload 'helm-do-pdfgrep "helm-grep" "\
Preconfigured helm for pdfgrep.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-describe-helm-attribute helm-esh-help helm-c-bookmark-help
;;;;;;  helm-c-ucs-help helm-etags-help helm-pdfgrep-help helm-grep-help
;;;;;;  helm-generic-file-help helm-read-file-name-help helm-ff-help
;;;;;;  helm-c-buffer-help helm-help) "helm-help" "helm-help.el"
;;;;;;  (20347 63906))
;;; Generated autoloads from helm-help.el

(autoload 'helm-help "helm-help" "\
Help of `helm'.

\(fn)" t nil)

(autoload 'helm-c-buffer-help "helm-help" "\
Help command for helm buffers.

\(fn)" t nil)

(autoload 'helm-ff-help "helm-help" "\
Help command for `helm-find-files'.

\(fn)" t nil)

(autoload 'helm-read-file-name-help "helm-help" "\


\(fn)" t nil)

(autoload 'helm-generic-file-help "helm-help" "\


\(fn)" t nil)

(autoload 'helm-grep-help "helm-help" "\


\(fn)" t nil)

(autoload 'helm-pdfgrep-help "helm-help" "\


\(fn)" t nil)

(autoload 'helm-etags-help "helm-help" "\
The help function for etags.

\(fn)" t nil)

(autoload 'helm-c-ucs-help "helm-help" "\
Help command for `helm-ucs'.

\(fn)" t nil)

(autoload 'helm-c-bookmark-help "helm-help" "\
Help command for bookmarks.

\(fn)" t nil)

(autoload 'helm-esh-help "helm-help" "\
Help command for `helm-find-files-eshell-command-on-file'.

\(fn)" t nil)

(autoload 'helm-describe-helm-attribute "helm-help" "\
Display the full documentation of HELM-ATTRIBUTE.
HELM-ATTRIBUTE should be a symbol.

\(fn HELM-ATTRIBUTE)" t nil)

;;;***

;;;### (autoloads (helm-imenu) "helm-imenu" "helm-imenu.el" (20345
;;;;;;  62057))
;;; Generated autoloads from helm-imenu.el

(autoload 'helm-imenu "helm-imenu" "\
Preconfigured `helm' for `imenu'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-info-at-point) "helm-info" "helm-info.el"
;;;;;;  (20345 62057))
;;; Generated autoloads from helm-info.el

(autoload 'helm-info-at-point "helm-info" "\
Preconfigured `helm' for searching info at point.
With a prefix-arg insert symbol at point.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-locate) "helm-locate" "helm-locate.el" (20345
;;;;;;  62057))
;;; Generated autoloads from helm-locate.el

(autoload 'helm-locate "helm-locate" "\
Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See 'man locate' for valid options.

You can specify a specific database with prefix argument ARG (C-u).
Many databases can be used: navigate and mark them.
See also `helm-locate-with-db'.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-man-woman) "helm-man" "helm-man.el" (20345
;;;;;;  62057))
;;; Generated autoloads from helm-man.el

(autoload 'helm-man-woman "helm-man" "\
Preconfigured `helm' for Man and Woman pages.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-mp-toggle-match-plugin) "helm-match-plugin"
;;;;;;  "helm-match-plugin.el" (20345 62057))
;;; Generated autoloads from helm-match-plugin.el

(autoload 'helm-mp-toggle-match-plugin "helm-match-plugin" "\
Turn on/off multiple regexp matching in helm.
i.e helm-match-plugin.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-minibuffer-history helm-mini helm-ratpoison-commands
;;;;;;  helm-eev-anchors helm-c-insert-latex-math helm-world-time)
;;;;;;  "helm-misc" "helm-misc.el" (20345 62057))
;;; Generated autoloads from helm-misc.el

(autoload 'helm-world-time "helm-misc" "\
Preconfigured `helm' to show world time.

\(fn)" t nil)

(autoload 'helm-c-insert-latex-math "helm-misc" "\
Preconfigured helm for latex math symbols completion.

\(fn)" t nil)

(autoload 'helm-eev-anchors "helm-misc" "\
Preconfigured `helm' for eev anchors.

\(fn)" t nil)

(autoload 'helm-ratpoison-commands "helm-misc" "\
Preconfigured `helm' to execute ratpoison commands.

\(fn)" t nil)

(autoload 'helm-mini "helm-misc" "\
Preconfigured `helm' lightweight version (buffer -> recentf).

\(fn)" t nil)

(autoload 'helm-minibuffer-history "helm-misc" "\
Preconfigured `helm' for `minibuffer-history'.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-mode) "helm-mode" "helm-mode.el" (20347 60209))
;;; Generated autoloads from helm-mode.el

(defvar helm-mode nil "\
Non-nil if Helm mode is enabled.
See the command `helm-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `helm-mode'.")

(custom-autoload 'helm-mode "helm-mode" nil)

(autoload 'helm-mode "helm-mode" "\
Toggle generic helm completion.

All functions in Emacs that use `completing-read'
or `read-file-name' and friends will use helm interface
when this mode is turned on.
However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can turn it on with `helm-mode'.

Some crap emacs functions may not be supported,
e.g `ffap-alternate-file' and maybe others
You can add such functions to `helm-completing-read-handlers-alist'
with a nil value.

Note: This mode will work only partially on Emacs23.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads (helm-yahoo-suggest helm-google-suggest helm-surfraw)
;;;;;;  "helm-net" "helm-net.el" (20345 62057))
;;; Generated autoloads from helm-net.el

(autoload 'helm-surfraw "helm-net" "\
Preconfigured `helm' to search PATTERN with search ENGINE.

\(fn PATTERN ENGINE)" t nil)

(autoload 'helm-google-suggest "helm-net" "\
Preconfigured `helm' for google search with google suggest.

\(fn)" t nil)

(autoload 'helm-yahoo-suggest "helm-net" "\
Preconfigured `helm' for Yahoo searching with Yahoo suggest.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-org-headlines helm-org-keywords) "helm-org"
;;;;;;  "helm-org.el" (20345 62057))
;;; Generated autoloads from helm-org.el

(autoload 'helm-org-keywords "helm-org" "\
Preconfigured `helm' for org keywords.

\(fn)" t nil)

(autoload 'helm-org-headlines "helm-org" "\
Preconfigured helm to show org headlines.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-browse-code helm-occur helm-regexp) "helm-regexp"
;;;;;;  "helm-regexp.el" (20345 62057))
;;; Generated autoloads from helm-regexp.el

(autoload 'helm-regexp "helm-regexp" "\
Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp.

\(fn)" t nil)

(autoload 'helm-occur "helm-regexp" "\
Preconfigured Helm for Occur source.
If region is active, search only in region,
otherwise search in whole buffer.

\(fn)" t nil)

(autoload 'helm-browse-code "helm-regexp" "\
Preconfigured helm to browse code.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-show-kill-ring helm-register helm-all-mark-rings
;;;;;;  helm-global-mark-ring helm-mark-ring) "helm-ring" "helm-ring.el"
;;;;;;  (20347 64034))
;;; Generated autoloads from helm-ring.el

(autoload 'helm-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-global-mark-ring "helm-ring" "\
Preconfigured `helm' for `helm-c-source-global-mark-ring'.

\(fn)" t nil)

(autoload 'helm-all-mark-rings "helm-ring" "\
Preconfigured `helm' for `helm-c-source-global-mark-ring' and `helm-c-source-mark-ring'.

\(fn)" t nil)

(autoload 'helm-register "helm-ring" "\
Preconfigured `helm' for Emacs registers.

\(fn)" t nil)

(autoload 'helm-show-kill-ring "helm-ring" "\
Preconfigured `helm' for `kill-ring'.
It is drop-in replacement of `yank-pop'.
You may bind this command to M-y.
First call open the kill-ring browser, next calls move to next line.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-xrandr-set helm-list-emacs-process helm-top)
;;;;;;  "helm-sys" "helm-sys.el" (20347 60250))
;;; Generated autoloads from helm-sys.el

(autoload 'helm-top "helm-sys" "\
Preconfigured `helm' for top command.

\(fn)" t nil)

(autoload 'helm-list-emacs-process "helm-sys" "\
Preconfigured `helm' for emacs process.

\(fn)" t nil)

(autoload 'helm-xrandr-set "helm-sys" "\


\(fn)" t nil)

;;;***

;;;### (autoloads (helm-c-etags-select) "helm-tags" "helm-tags.el"
;;;;;;  (20345 62057))
;;; Generated autoloads from helm-tags.el

(autoload 'helm-c-etags-select "helm-tags" "\
Preconfigured helm for etags.
Called with one prefix arg use symbol at point as initial input.
Called with two prefix arg reinitialize cache.
If tag file have been modified reinitialize cache.

\(fn ARG)" t nil)

;;;***

;;;### (autoloads (helm-yank-text-at-point helm-w32-shell-execute-open-file
;;;;;;  helm-quit-and-find-file helm-show-all-in-this-source-only)
;;;;;;  "helm-utils" "helm-utils.el" (20351 62637))
;;; Generated autoloads from helm-utils.el

(autoload 'helm-show-all-in-this-source-only "helm-utils" "\
Show only current source of this helm session with all its candidates.
With a numeric prefix arg show only the ARG number of candidates.

\(fn ARG)" t nil)

(autoload 'helm-quit-and-find-file "helm-utils" "\
Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory.

\(fn)" t nil)

(autoload 'helm-w32-shell-execute-open-file "helm-utils" "\


\(fn FILE)" t nil)

(autoload 'helm-yank-text-at-point "helm-utils" "\
Yank text at point in minibuffer.

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-w3m-bookmarks) "helm-w3m" "helm-w3m.el" (20347
;;;;;;  17170))
;;; Generated autoloads from helm-w3m.el

(autoload 'helm-w3m-bookmarks "helm-w3m" "\
Preconfigured `helm' for w3m bookmark.

Needs w3m and emacs-w3m.

http://w3m.sourceforge.net/
http://emacs-w3m.namazu.org/

\(fn)" t nil)

;;;***

;;;### (autoloads (helm-yaoddmuse-emacswiki-post-library helm-yaoddmuse-emacswiki-edit-or-view
;;;;;;  helm-yaoddmuse-cache-pages) "helm-yaoddmuse" "helm-yaoddmuse.el"
;;;;;;  (20345 62057))
;;; Generated autoloads from helm-yaoddmuse.el

(autoload 'helm-yaoddmuse-cache-pages "helm-yaoddmuse" "\
Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'.

\(fn &optional LOAD)" t nil)

(autoload 'helm-yaoddmuse-emacswiki-edit-or-view "helm-yaoddmuse" "\
Preconfigured `helm' to edit or view EmacsWiki page.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

(autoload 'helm-yaoddmuse-emacswiki-post-library "helm-yaoddmuse" "\
Preconfigured `helm' to post library to EmacsWiki.

Needs yaoddmuse.el.

http://www.emacswiki.org/emacs/download/yaoddmuse.el

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("helm-pkg.el" "helm-plugin.el") (20351
;;;;;;  63022 643587))

;;;***

;;; End of automatically extracted autoloads.

(provide 'helm-config)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm-config.el ends here
