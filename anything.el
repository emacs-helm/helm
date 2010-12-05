;;;; anything.el --- open anything / QuickSilver-like candidate-selection framework

;; Copyright (C) 2007              Tamas Patrovics
;;               2008, 2009, 2010  rubikitch <rubikitch@ruby-lang.org>

;; Author: Tamas Patrovics
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: files, frames, help, matching, outlines, processes, tools, convenience, anything
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything.el
;; Site: http://www.emacswiki.org/cgi-bin/emacs/Anything
(defvar anything-version nil)
(setq anything-version "1.287")

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
;; Start with M-x anything, narrow the list by typing some pattern,
;; select with up/down/pgup/pgdown/C-p/C-n/C-v/M-v, choose with enter,
;; left/right moves between sources. With TAB actions can be selected
;; if the selected candidate has more than one possible action.
;;
;; Note that anything.el provides only the framework and some example
;; configurations for demonstration purposes. See anything-config.el
;; for practical, polished, easy to use configurations which can be
;; used to assemble a custom personalized configuration. And many
;; other configurations are in the EmacsWiki.
;; 
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-config.el
;; http://www.emacswiki.org/cgi-bin/emacs/AnythingSources
;;
;; Maintainer's configuration is in the EmacsWiki. It would tell you
;; many tips to write smart sources!
;;
;; http://www.emacswiki.org/cgi-bin/emacs/RubikitchAnythingConfiguration
;;
;; Here is Japanese translation of `anything-sources' attributes. Thanks.
;; http://d.hatena.ne.jp/sirocco634/20091012/1255336649

;;; Bug Report:
;;
;; If you have problems, send a bug report via C-c C-x C-b in anything session (best)
;; or M-x anything-send-bug-report outside anything session.
;; I implemented bug report feature because I want to know your current state.
;; It helps me to solve problems easily.
;; The step is:
;;  0) Setup mail in Emacs, the easiest way is:
;;       (setq user-mail-address "your@mail.address")
;;       (setq user-full-name "Your Full Name")
;;       (setq smtpmail-smtp-server "your.smtp.server.jp")
;;       (setq mail-user-agent 'message-user-agent)
;;       (setq message-send-mail-function 'message-smtpmail-send-it)
;;  1) Be sure to use the LATEST version of anything.el.
;;  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
;;  3) Use Lisp version instead of compiled one: (load "anything.el")
;;  4) Do it!
;;  5) If you got an error, please do not close *Backtrace* buffer.
;;  6) Type C-c C-x C-b (anything session, best!) 
;;     or M-x anything-send-bug-report (outside)
;;     then M-x insert-buffer *Backtrace* (if you got error)
;;  7) Describe the bug using a precise recipe.
;;  8) Type C-c C-c to send.
;;  # If you are a Japanese, please write in Japanese:-)


;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-open-last-log'
;;    Open anything log file of last anything session.
;;  `anything'
;;    Select anything. In Lisp program, some optional arguments can be used.
;;  `anything-resume'
;;    Resurrect previously invoked `anything'.
;;  `anything-at-point'
;;    Same as `anything' except when C-u is pressed, the initial input is the symbol at point.
;;  `anything-force-update'
;;    Recalculate and update candidates.
;;  `anything-select-action'
;;    Select an action for the currently selected candidate.
;;  `anything-previous-line'
;;    Move selection to the previous line.
;;  `anything-next-line'
;;    Move selection to the next line.
;;  `anything-previous-page'
;;    Move selection back with a pageful.
;;  `anything-next-page'
;;    Move selection forward with a pageful.
;;  `anything-beginning-of-buffer'
;;    Move selection at the top.
;;  `anything-end-of-buffer'
;;    Move selection at the bottom.
;;  `anything-previous-source'
;;    Move selection to the previous source.
;;  `anything-next-source'
;;    Move selection to the next source.
;;  `anything-select-with-prefix-shortcut'
;;    Invoke default action with prefix shortcut.
;;  `anything-select-with-digit-shortcut'
;;    Invoke default action with digit/alphabet shortcut.
;;  `anything-exit-minibuffer'
;;    Select the current candidate by exiting the minibuffer.
;;  `anything-help'
;;    Help of `anything'.
;;  `anything-debug-output'
;;    Show all anything-related variables at this time.
;;  `anything-delete-current-selection'
;;    Delete the currently selected item.
;;  `anything-delete-minibuffer-contents'
;;    Same as `delete-minibuffer-contents' but this is a command.
;;  `anything-toggle-resplit-window'
;;    Toggle resplit anything window, vertically or horizontally.
;;  `anything-select-2nd-action'
;;    Select the 2nd action for the currently selected candidate.
;;  `anything-select-3rd-action'
;;    Select the 3rd action for the currently selected candidate.
;;  `anything-select-4th-action'
;;    Select the 4th action for the currently selected candidate.
;;  `anything-select-2nd-action-or-end-of-line'
;;    Select the 2nd action for the currently selected candidate if the point is at the end of minibuffer.
;;  `anything-execute-persistent-action'
;;    If a candidate is selected then perform the associated action without quitting anything.
;;  `anything-scroll-other-window'
;;    Scroll other window (not *Anything* window) upward.
;;  `anything-scroll-other-window-down'
;;    Scroll other window (not *Anything* window) downward.
;;  `anything-toggle-visible-mark'
;;    Toggle anything visible mark at point.
;;  `anything-display-all-visible-marks'
;;    Show all `anything' visible marks strings.
;;  `anything-next-visible-mark'
;;    Move next anything visible mark.
;;  `anything-prev-visible-mark'
;;    Move previous anything visible mark.
;;  `anything-quit-and-find-file'
;;    Drop into `find-file' from `anything' like `iswitchb-find-file'.
;;  `anything-yank-selection'
;;    Set minibuffer contents to current selection.
;;  `anything-kill-selection-and-quit'
;;    Store current selection to kill ring.
;;  `anything-follow-mode'
;;    If this mode is on, persistent action is executed everytime the cursor is moved.
;;  `anything-migrate-sources'
;;    Help to migrate to new `anything' way.
;;  `anything-isearch'
;;    Start incremental search within results. (UNMAINTAINED)
;;  `anything-isearch-printing-char'
;;    Add printing char to the pattern.
;;  `anything-isearch-again'
;;    Search again for the current pattern
;;  `anything-isearch-delete'
;;    Undo last event.
;;  `anything-isearch-default-action'
;;    Execute the default action for the selected candidate.
;;  `anything-isearch-select-action'
;;    Choose an action for the selected candidate.
;;  `anything-isearch-cancel'
;;    Cancel Anything isearch.
;;  `anything-iswitchb-setup'
;;    Integrate anything completion into iswitchb (UNMAINTAINED).
;;  `anything-iswitchb-cancel-anything'
;;    Cancel anything completion and return to standard iswitchb.
;;  `anything-describe-anything-attribute'
;;    Display the full documentation of ANYTHING-ATTRIBUTE (a symbol).
;;  `anything-send-bug-report'
;;    Send a bug report of anything.el.
;;  `anything-send-bug-report-from-anything'
;;    Send a bug report of anything.el in anything session.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;

;; You can extend `anything' by writing plug-ins. As soon as
;; `anything' is invoked, `anything-sources' is compiled into basic
;; attributes, then compiled one is used during invocation.
;;
;; The oldest built-in plug-in is `type' attribute: appends
;; appropriate element of `anything-type-attributes'. Second built-in
;; plug-in is `candidates-in-buffer': selecting a line from candidates
;; buffer.
;;
;; To write a plug-in:
;; 1. Define a compiler: anything-compile-source--*
;; 2. Add compier function to `anything-compile-source-functions'.
;; 3. (optional) Write helper functions.
;;
;; Anything plug-ins are found in the EmacsWiki.
;;
;; http://www.emacswiki.org/cgi-bin/emacs/AnythingPlugins

;; Tested on Emacs 22/23.
;;
;;
;; Thanks to Vagn Johansen for ideas.
;; Thanks to Stefan Kamphausen for fixes and XEmacs support.
;; Thanks to Tassilo Horn for fixes.
;; Thanks to Drew Adams for various fixes (frame, isearch, customization, etc.)
;; Thanks to IMAKADO for candidates-in-buffer idea.
;; Thanks to Tomohiro MATSUYAMA for multiline patch.
;;

;;; (@* "Index")

;;  If you have library `linkd.el', load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections  Linkd mode will
;;  highlight this Index.  You can get `linkd.el' here:
;;  http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el
;;


;;; (@* "INCOMPATIBLE CHANGES")

;; v1.277
;; 
;;   Default setting of `anything-save-configuration-functions' is changed.
;;   Anything saves/restores window configuration instead of frame configuration now.
;;   The default is changed because flickering is occurred in some environment.
;;   
;;   If you want to save and restore frame configuration, set this variable to
;;    '(set-frame-configuration . current-frame-configuration)
;;
;; v1.276
;;
;;   Fitting frame is disabled by default, because some flickering occurred
;;   in some environment.  To enable fitting, set both
;;   `anything-inhibit-fit-frame-flag' and `fit-frame-inhibit-fitting' to
;;   nil.
;;
;; v1.114
;;
;;   `anything-attr' returns nil when the source attribute is defined
;;   but the value of attribute is nil, eg. (volatile) cell. Use
;;   `anything-attr-defined' when testing whether the attribute is
;;   defined.

;;; (@* "Tips")

;;
;; `anything' accepts keyword arguments. See docstring.
;; [EVAL IT] (describe-function 'anything)

;; 
;; `anything-enable-shortcuts' enables us to select candidate easily.
;; If 'prefix then they can be selected using <prefix-key> <alnum>. 
;; The prefix key is `anything-select-with-prefix-shortcut'.
;; If the <prefix-key> is a letter, pressing twice inputs the letter itself.
;; e.g.
;;  (setq anything-enable-shortcuts 'prefix)
;;  (define-key anything-map \"@\" 'anything-select-with-prefix-shortcut)

;;
;; You can edit current selection using `anything-edit-current-selection'.
;; It is useful after persistent-action.

;;
;; For `anything' users, setting `anything-sources' directly and
;; invoke M-x anything is obsolete way for now. Try M-x
;; `anything-migrate-sources'!

;;
;; If you want to create anything sources, yasnippet would help you.
;; http://yasnippet.googlecode.com/
;;
;; Then get the snippet from
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-source.yasnippet
;;
;; Put it in ~/.emacs.d/plugins/yasnippet/snippets/text-mode/emacs-lisp-mode/


;;
;; `anything-interpret-value' is useful function to interpret value
;; like `candidates' attribute.
;;
;; (anything-interpret-value "literal")            ; => "literal"
;; (anything-interpret-value (lambda () "lambda")) ; => "lambda"
;; (let ((source '((name . "lambda with source name"))))
;;   (anything-interpret-value
;;    (lambda () anything-source-name)
;;    source))                             ; => "lambda with source name"
;; (flet ((f () "function symbol"))
;;   (anything-interpret-value 'f))        ; => "function symbol"
;; (let ((v "variable symbol"))
;;   (anything-interpret-value 'v))        ; => "variable symbol"
;; (anything-interpret-value 'unbounded-1) ; error

;;
;; Now symbols are acceptable as candidates. So you do not have to use
;; `symbol-name' function. The source is much simpler. For example,
;; `apropos-internal' returns a list of symbols.
;; 
;;   (anything
;;    '(((name . "Commands")
;;       (candidates . (lambda () (apropos-internal anything-pattern 'commandp)))
;;       (volatile)
;;       (action . describe-function))))

;;
;; To mark a candidate, press C-SPC as normal Emacs marking. To go to
;; marked candidate, press M-[ or M-].

;;
;; `anything-map' is now Emacs-standard key bindings by default. If
;; you are using `iswitchb', execute `anything-iswitchb-setup'. Then
;; some key bindings are adjusted to `iswitchb'. Note that
;; anything-iswitchb is not maintained.

;;
;; There are many `anything' applications, using `anything' for
;; selecting candidate. In this case, if there is one candidate or no
;; candidate, popping up *anything* buffer is irritating. If one
;; candidate, you want to select it at once. If no candidate, you want
;; to quit `anything'. Set `anything-execute-action-at-once-if-one'
;; and `anything-quit-if-no-candidate' to non-nil to remedy it. Note
;; that setting these variables GLOBALLY is bad idea because of
;; delayed sources. These are meant to be let-binded.
;; See anything-etags.el for example.
;;
;; [EVAL IT] (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/anything-etags.el")
;;
;; ex.
;; (let ((anything-execute-action-at-once-if-one t)
;;       (anything-quit-if-no-candidate (lambda () (message "No candidate"))))
;;    (anything temporary-sources input))

;;
;; `set-frame-configuration' arises flickering. If you hate
;; flickering, eval:
;; (setq anything-save-configuration-functions
;;    '(set-window-configuration . current-window-configuration))
;; at the cost of restoring frame configuration (only window configuration).

;;
;; `anything-delete-current-selection' deletes the current line.
;; It is useful when deleting a candidate in persistent action.
;; eg. `kill-buffer'.
;;
;; [EVAL IT] (describe-function 'anything-delete-current-selection)

;;
;; `anything-attr' gets the attribute. `anything-attrset' sets the
;; attribute. `anything-attr-defined' tests whether the attribute is
;; defined. They handles source-local variables.
;;
;; [EVAL IT] (describe-function 'anything-attr)
;; [EVAL IT] (describe-function 'anything-attrset)
;; [EVAL IT] (describe-function 'anything-attr-defined)

;;
;; `anything-sources' accepts many attributes to make your life easier.
;; Now `anything-sources' accepts a list of symbols.
;;
;; [EVAL IT] (describe-variable 'anything-sources)

;;
;; `anything' has optional arguments. Now you do not have to let-bind
;; `anything-sources'.
;;
;; [EVAL IT] (describe-function 'anything)

;;
;; `anything-resume' resumes last `anything' session. Now you do not
;; have to retype pattern.
;;
;; [EVAL IT] (describe-function 'anything-resume)

;;
;; `anything-execute-persistent-action' executes action without
;; quitting `anything'. When popping up a buffer in other window by
;; persistent action, you can scroll with `anything-scroll-other-window' and
;; `anything-scroll-other-window-down'. See also `anything-sources' docstring.
;;
;; [EVAL IT] (describe-function 'anything-execute-persistent-action)
;; [EVAL IT] (describe-variable 'anything-sources)

;;
;; `anything-select-2nd-action', `anything-select-3rd-action' and
;; `anything-select-4th-action' select other than default action
;; without pressing Tab.

;;
;; Using `anything-candidate-buffer' and the candidates-in-buffer
;; attribute is much faster than traditional "candidates and match"
;; way. And `anything-current-buffer-is-modified' avoids to
;; recalculate candidates for unmodified buffer. See docstring of
;; them.
;;
;; [EVAL IT] (describe-function 'anything-candidate-buffer)
;; [EVAL IT] (describe-function 'anything-candidates-in-buffer)
;; [EVAL IT] (describe-function 'anything-current-buffer-is-modified)

;;
;; `anything-current-buffer' and `anything-buffer-file-name' stores
;; `(current-buffer)' and `buffer-file-name' in the buffer `anything'
;; is invoked. Use them freely.
;;
;; [EVAL IT] (describe-variable 'anything-current-buffer)
;; [EVAL IT] (describe-variable 'anything-buffer-file-name)

;;
;; `anything-completing-read' and `anything-read-file-name' are
;; experimental implementation. If you are curious, type M-x
;; anything-read-string-mode. It is a minor mode and toggles on/off.

;;
;; Use `anything-test-candidates' to test your handmade anything
;; sources. It simulates contents of *anything* buffer with pseudo
;; `anything-sources' and `anything-pattern', without side-effect. So
;; you can unit-test your anything sources! Let's TDD!
;;
;; [EVAL IT] (describe-function 'anything-test-candidates)
;;
;; There are many unit-testing framework in Emacs Lisp. See the EmacsWiki.
;; http://www.emacswiki.org/cgi-bin/emacs/UnitTesting
;;
;; There is an unit-test by Emacs Lisp Expectations at the tail of this file.
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el
;; http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el

;;
;; If you want to create anything sources, see anything-config.el.
;; It is huge collection of sources. You can learn from examples.


;; (@* "TODO")
;;
;;   - process status indication
;;
;;   - async sources doesn't honor digit-shortcut-count
;;
;;   - anything-candidate-number-limit can't be nil everywhere

;; (@* "HISTORY")
;;
;;  Change log of this file is found at
;;  http://repo.or.cz/w/anything-config.git/history/master:/anything.el
;;
;;  Change log of this project is found at
;;  http://repo.or.cz/w/anything-config.git?a=shortlog

(require 'cl)
;; (require 'anything-match-plugin nil t)

;; (@* "User Configuration")

;; This is only an example. Customize it to your own taste!
(defvar anything-sources
  `(((name . "Buffers")
     (candidates
      . (lambda ()
          (remove-if (lambda (name)
                       (or (equal name anything-buffer)
                           (eq ?\  (aref name 0))))
                     (mapcar 'buffer-name (buffer-list)))))
     (type . buffer))

    ((name . "File Name History")
     (candidates . file-name-history)
     (match (lambda (candidate)
              ;; list basename matches first
              (string-match 
               anything-pattern 
               (file-name-nondirectory candidate)))

            (lambda (candidate)                                     
              ;; and then directory part matches
              (let ((dir (file-name-directory candidate)))
                (if dir
                    (string-match anything-pattern dir)))))
     (type . file))

    ((name . "Files from Current Directory")
     (init . (lambda ()
               (setq anything-default-directory
                     default-directory)))
     (candidates . (lambda ()
                     (directory-files
                      anything-default-directory)))
     (type . file))

    ((name . "Complex Command History")
     (candidates . (lambda ()
                     (mapcar 'prin1-to-string
                             command-history)))
     (action . (("Repeat Complex Command" . 
                 (lambda (c)
                   (eval (read c))))))
     (delayed)))
  "The source of candidates for anything.
It accepts symbols:
 (setq anything-sources (list anything-c-foo anything-c-bar))
can be written as
 (setq anything-sources '(anything-c-foo anything-c-bar))
The latter is recommended because if you change anything-c-* variable,
you do not have to update `anything-sources'.

You are STRONGLY recommended to define a command which calls
`anything' or `anything-other-buffer' with argument rather than
to set `anything-sources' externally.

If you want to change `anything-sources' during `anything' invocation,
use `anything-set-sources', never use `setq'.

Attributes:

")


;; This value is only provided as an example. Customize it to your own
;; taste!
(defvar anything-type-attributes
  '((file (action . (("Find File" . find-file)
                     ("Delete File" .
                      (lambda (file)
                        (if (y-or-n-p (format "Really delete file %s? " file))
                            (delete-file file)))))))
    (buffer (action . (("Switch to Buffer" . switch-to-buffer)
                       ("Pop to Buffer"    . pop-to-buffer)
                       ("Display Buffer"   . display-buffer)
                       ("Kill Buffer"      . kill-buffer)))))
  "It's a list of (TYPE ATTRIBUTES ...). ATTRIBUTES are the same
  as attributes for `anything-sources'. TYPE connects the value
  to the appropriate sources in `anything-sources'.

  This allows specifying common attributes for several
  sources. For example, sources which provide files can specify
  common attributes with a `file' type.")


(defvaralias 'anything-enable-digit-shortcuts 'anything-enable-shortcuts
  "Alphabet shortcuts are usable now. Then `anything-enable-digit-shortcuts' should be renamed.
`anything-enable-digit-shortcuts' is retained for compatibility.")
(defvar anything-enable-shortcuts nil
  "*Whether to use digit/alphabet shortcut to select the first nine matches.
If t then they can be selected using Ctrl+<number>.

If 'prefix then they can be selected using <prefix-key> <alnum>. 
The prefix key is `anything-select-with-prefix-shortcut'.
If the <prefix-key> is a letter, pressing twice inputs the letter itself.
e.g.
 (setq anything-enable-shortcuts 'prefix)
 (define-key anything-map \"@\" 'anything-select-with-prefix-shortcut)
 
If 'alphabet then they can be selected using Shift+<alphabet> (deprecated).
It is not recommended because you cannot input capital letters in pattern.

Keys (digit/alphabet) are listed in `anything-shortcut-keys-alist'.")

(defvar anything-shortcut-keys-alist
  '((alphabet . "asdfghjklzxcvbnmqwertyuiop")
    (prefix   . "asdfghjklzxcvbnmqwertyuiop1234567890")
    (t        . "123456789")))

(defvar anything-display-source-at-screen-top t
  "*If t, `anything-next-source' and `anything-previous-source'
  display candidates at the top of screen.")

(defvar anything-candidate-number-limit 50
  "*Do not show more candidates than this limit from individual
  sources. It is usually pointless to show hundreds of matches
  when the pattern is empty, because it is much simpler to type a
  few characters to narrow down the list of potential candidates.

  Set it to nil if you don't want this limit.")


(defvar anything-idle-delay 0.5
  "*The user has to be idle for this many seconds, before
  candidates from delayed sources are collected. This is useful
  for sources involving heavy operations (like launching external
  programs), so that candidates from the source are not retrieved
  unnecessarily if the user keeps typing.

  It also can be used to declutter the results anything displays,
  so that results from certain sources are not shown with every
  character typed, only if the user hesitates a bit.")


(defvar anything-input-idle-delay 0.1
  "The user has to be idle for this many seconds, before ALL candidates are collected.
Unlink `anything-input-idle', it is also effective for non-delayed sources.
If nil, candidates are collected immediately. ")


(defvar anything-samewindow nil
  "If t then Anything doesn't pop up a new window, it uses the
current window to show the candidates.")


(defvar anything-source-filter nil
  "A list of source names to be displayed. Other sources won't
appear in the search results. If nil then there is no filtering.
See also `anything-set-source-filter'.")


(defvar anything-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'anything-next-line)
    (define-key map (kbd "<up>") 'anything-previous-line)
    (define-key map (kbd "C-n")     'anything-next-line)
    (define-key map (kbd "C-p")     'anything-previous-line)
    (define-key map (kbd "<prior>") 'anything-previous-page)
    (define-key map (kbd "<next>") 'anything-next-page)
    (define-key map (kbd "M-v")     'anything-previous-page)
    (define-key map (kbd "C-v")     'anything-next-page)
    (define-key map (kbd "M-<")     'anything-beginning-of-buffer)
    (define-key map (kbd "M->")     'anything-end-of-buffer)
    (define-key map (kbd "<right>") 'anything-next-source)
    (define-key map (kbd "<left>") 'anything-previous-source)
    (define-key map (kbd "<RET>") 'anything-exit-minibuffer)
    (define-key map (kbd "C-1") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-2") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-3") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-4") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-5") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-6") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-7") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-8") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-9") 'anything-select-with-digit-shortcut)
    (loop for c from ?A to ?Z do
          (define-key map (make-string 1 c) 'anything-select-with-digit-shortcut))
    (define-key map (kbd "C-i") 'anything-select-action)
    (define-key map (kbd "C-z") 'anything-execute-persistent-action)
    (define-key map (kbd "C-e") 'anything-select-2nd-action-or-end-of-line)
    (define-key map (kbd "C-j") 'anything-select-3rd-action)
    (define-key map (kbd "C-o") 'anything-next-source)
    (define-key map (kbd "C-M-v") 'anything-scroll-other-window)
    (define-key map (kbd "M-<next>") 'anything-scroll-other-window)
    (define-key map (kbd "C-M-y") 'anything-scroll-other-window-down)
    (define-key map (kbd "C-M-S-v") 'anything-scroll-other-window-down)
    (define-key map (kbd "M-<prior>") 'anything-scroll-other-window-down)
    (define-key map (kbd "C-SPC") 'anything-toggle-visible-mark)
    (define-key map (kbd "M-[") 'anything-prev-visible-mark)
    (define-key map (kbd "M-]") 'anything-next-visible-mark)
    (define-key map (kbd "C-k") 'anything-delete-minibuffer-contents)

    (define-key map (kbd "C-s") 'anything-isearch)
    (define-key map (kbd "C-r") 'undefined)
    (define-key map (kbd "C-t") 'anything-toggle-resplit-window)
    (define-key map (kbd "C-x C-f") 'anything-quit-and-find-file)

    (define-key map (kbd "C-c C-d") 'anything-delete-current-selection)
    (define-key map (kbd "C-c C-y") 'anything-yank-selection)
    (define-key map (kbd "C-c C-k") 'anything-kill-selection-and-quit)
    (define-key map (kbd "C-c C-f") 'anything-follow-mode)
    (define-key map (kbd "C-c C-u") 'anything-force-update)

    ;; Debugging command
    (define-key map "\C-c\C-x\C-d" 'anything-debug-output)
    (define-key map "\C-c\C-x\C-m" 'anything-display-all-visible-marks)
    (define-key map "\C-c\C-x\C-b" 'anything-send-bug-report-from-anything)
    ;; Use `describe-mode' key in `global-map'
    (dolist (k (where-is-internal 'describe-mode global-map))
      (define-key map k 'anything-help))
    ;; the defalias is needed because commands are bound by name when
    ;; using iswitchb, so only commands having the prefix anything-
    ;; get rebound
    (defalias 'anything-previous-history-element 'previous-history-element)
    (defalias 'anything-next-history-element 'next-history-element)
    (define-key map (kbd "M-p") 'anything-previous-history-element)
    (define-key map (kbd "M-n") 'anything-next-history-element)
    map)
  "Keymap for anything.

If you execute `anything-iswitchb-setup', some keys are modified.
See `anything-iswitchb-setup-keys'.")

(defvar anything-isearch-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-global-map))
    (define-key map (kbd "<return>") 'anything-isearch-default-action)
    (define-key map (kbd "<RET>") 'anything-isearch-default-action)
    (define-key map (kbd "C-i") 'anything-isearch-select-action)
    (define-key map (kbd "C-g") 'anything-isearch-cancel)
    (define-key map (kbd "M-s") 'anything-isearch-again)
    (define-key map (kbd "<backspace>") 'anything-isearch-delete)
    ;; add printing chars
    (loop for i from 32 below 256 do
          (define-key map (vector i) 'anything-isearch-printing-char))
    map)
  "Keymap for anything incremental search.")


(defgroup anything nil
  "Open anything." :prefix "anything-" :group 'convenience)

(defface anything-header 
  '((t (:inherit header-line))) 
  "Face for header lines in the anything buffer." :group 'anything)

(defvar anything-header-face 'anything-header
  "Face for header lines in the anything buffer.")

(defface anything-isearch-match '((t (:background "Yellow")))
  "Face for isearch in the anything buffer." :group 'anything)

(defvar anything-isearch-match-face 'anything-isearch-match
  "Face for matches during incremental search.")

(defvar anything-selection-face 'highlight
  "Face for currently selected item.")

(defvar anything-iswitchb-idle-delay 1
  "Show anything completions if the user is idle that many
  seconds after typing.")

(defvar anything-iswitchb-dont-touch-iswithcb-keys nil
  "If t then those commands are not bound from `anything-map'
  under iswitchb which would override standard iswithcb keys.

This allows an even more seamless integration with iswitchb for
those who prefer using iswitchb bindings even if the anything
completions buffer is popped up.

Note that you can bind alternative keys for the same command in
`anything-map', so that you can use different keys for anything
under iswitchb. For example, I bind the character \ to
`anything-exit-minibuffer' which key is just above Enter on my
keyboard. This way I can switch buffers with Enter and choose
anything completions with \.")

;;----------------------------------------------------------------------

(defvar anything-buffer "*anything*"
  "Buffer showing completions.")

(defvar anything-action-buffer "*anything action*"
  "Buffer showing actions.")

(defvar anything-selection-overlay nil
  "Overlay used to highlight the currently selected item.")

(defvar anything-isearch-overlay nil
  "Overlay used to highlight the current match during isearch.")

(defvar anything-digit-overlays nil
  "Overlays for digit shortcuts. See `anything-enable-shortcuts'.")

(defvar anything-candidate-cache nil
  "Holds the available candidate withing a single anything invocation.")

(defvar anything-pattern
  "The input pattern used to update the anything buffer.")

(defvar anything-input
  "The input typed in the candidates panel.")

(defvar anything-async-processes nil
  "List of information about asynchronous processes managed by anything.")

(defvar anything-digit-shortcut-count 0
  "Number of digit shortcuts shown in the anything buffer.")

(defvar anything-before-initialize-hook nil
  "Run before anything initialization.
This hook is run before init functions in `anything-sources'.")

(defvar anything-after-initialize-hook nil
  "Run after anything initialization.
Global variables are initialized and the anything buffer is created.
But the anything buffer has no contents. ")

(defvar anything-update-hook nil
  "Run after the anything buffer was updated according the new input pattern.
This hook is run at the beginning of buffer.
The first candidate is selected after running this hook.
See also `anything-after-update-hook'.")

(defvar anything-after-update-hook nil
  "Run after the anything buffer was updated according the new input pattern.
This is very similar to `anything-update-hook' but selection is not moved.
It is useful to select a particular object instead of the first one. ")

(defvar anything-cleanup-hook nil
  "Run after anything minibuffer is closed, IOW this hook is executed BEFORE performing action. ")

(defvar anything-after-action-hook nil
  "Run after executing action.")

(defvar anything-after-persistent-action-hook nil
  "Run after executing persistent action.")

(defvar anything-restored-variables
  '( anything-candidate-number-limit
     anything-source-filter
     anything-source-in-each-line-flag
     anything-map
     anything-sources
     deferred-action-list)
  "Variables which are restored after `anything' invocation.")
;; `anything-saved-sources' is removed

(defvar anything-saved-selection nil
  "Saved value of the currently selected object when the action
  list is shown.")

;; `anything-original-source-filter' is removed

(defvar anything-candidate-separator
  "--------------------"
  "Candidates separator of `multiline' source.")

(defvar anything-current-buffer nil
  "Current buffer when `anything' is invoked.")

(defvar anything-buffer-file-name nil
  "`buffer-file-name' when `anything' is invoked.")

(defvar anything-saved-action nil
  "Saved value of the currently selected action by key.")

(defvar anything-last-sources nil
  "OBSOLETE!! Sources of previously invoked `anything'.")

(defvar anything-saved-current-source nil
  "Saved value of the original (anything-get-current-source) when the action
  list is shown.")

(defvar anything-compiled-sources nil
  "Compiled version of `anything-sources'. ")

(defvar anything-in-persistent-action nil
  "Flag whether in persistent-action or not.")

(defvar anything-quick-update nil
  "If non-nil, suppress displaying sources which are out of screen at first.
They are treated as delayed sources at this input.
This flag makes `anything' a bit faster with many sources.")

(defvar anything-last-sources-local nil
  "Buffer local value of `anything-sources'.")
(defvar anything-last-buffer nil
  "`anything-buffer' of previously `anything' session.")

(defvar anything-save-configuration-functions
  '(set-window-configuration . current-window-configuration)
  "If you want to save and restore frame configuration, set this variable to
 '(set-frame-configuration . current-frame-configuration)

Older version saves/restores frame configuration, but the default is changed now,
because flickering is occurred in some environment.
")

(defvar anything-persistent-action-use-special-display nil
  "If non-nil, use `special-display-function' in persistent action.")

(defvar anything-execute-action-at-once-if-one nil
  "If non-nil and there is one candidate, execute the first action without selection.
It is useful for `anything' applications.")

(defvar anything-quit-if-no-candidate nil
  "if non-nil and there is no candidate, do not display *anything* buffer and quit.
This variable accepts a function, which is executed if no candidate.

It is useful for `anything' applications.")

(defvar anything-scroll-amount nil
  "Scroll amount used by `anything-scroll-other-window' and `anything-scroll-other-window-down'.
If you prefer scrolling line by line, set this value to 1.")

(defvar anything-display-function 'anything-default-display-buffer
  "Function to display *anything* buffer.
It is `anything-default-display-buffer' by default, which affects `anything-samewindow'.")

(defvar anything-delayed-init-executed nil)

(defvar anything-mode-line-string "\\<anything-map>\\[anything-help]:help \\[anything-select-action]:Acts \\[anything-exit-minibuffer]/\\[anything-select-2nd-action-or-end-of-line]/\\[anything-select-3rd-action]:NthAct \\[anything-send-bug-report-from-anything]:BugReport"
  "Help string displayed in mode-line in `anything'.
If nil, use default `mode-line-format'.")

(defvar anything-help-message
  "\\<anything-map>The keys that are defined for `anything' are:
       \\{anything-map}"
  "Detailed help message string for `anything'.
It also accepts function or variable symbol.")

(put 'anything 'timid-completion 'disabled)

(defvar anything-inhibit-fit-frame-flag t
  "If non-nil, inhibit fitting anything frame to its buffer.
It is nil by default because some flickering occurred in some environment.

To enable fitting, set both `anything-inhibit-fit-frame-flag' and
`fit-frame-inhibit-fitting' to nil.")

(defvar anything-source-in-each-line-flag nil
  "If non-nil, add anything-source text-property in each candidate.
experimental feature.")

(defvaralias 'anything-debug-variables 'anything-debug-forms)
(defvar anything-debug-forms nil
  "Forms to show in `anything-debug-output'.
Otherwise all variables started with `anything-' are shown.
It is useful for debug.")

(defvar anything-debug nil
  "If non-nil, write log message into *Anything Log* buffer.
If `debug-on-error' is non-nil, write log message regardless of this variable.
It is disabled by default because *Anything Log* grows quickly.")

;; (@* "Internal Variables")
(defvar anything-test-candidate-list nil)
(defvar anything-test-mode nil)
(defvar anything-source-name nil)
(defvar anything-candidate-buffer-alist nil)
(defvar anything-check-minibuffer-input-timer nil)
(defvar anything-match-hash (make-hash-table :test 'equal))
(defvar anything-cib-hash (make-hash-table :test 'equal))
(defvar anything-tick-hash (make-hash-table :test 'equal))
(defvar anything-issued-errors nil)
(defvar anything-shortcut-keys nil)
(defvar anything-once-called-functions nil)
(defvar anything-follow-mode nil)
(defvar anything-let-variables nil)

;; (@* "Utility: logging")
(defun anything-log (format-string &rest args)
  "Log message if `debug-on-error' or `anything-debug' is non-nil.
Messages are written to the *Anythingn Log* buffer.
Arguments are same as `format'."
  (when (or debug-on-error anything-debug)
    (with-current-buffer (get-buffer-create "*Anything Log*")
      (buffer-disable-undo)
      (set (make-local-variable 'inhibit-read-only) t)
      (goto-char (point-max))
      (insert (let ((tm (current-time)))
                (format "%s.%06d (%s) %s\n"
                        (format-time-string "%H:%M:%S" tm)
                        (nth 2 tm)
                        (anything-log-get-current-function)
                        (apply #'format (cons format-string args))))))))
(defmacro anything-log-eval (&rest exprs)
  "Write each EXPR evaluation result to the *Anything Log* buffer."
  `(anything-log-eval-internal ',exprs))
(defun anything-log-run-hook (hook)
  (anything-log "executing %s" hook)
  (when (boundp hook)
    (anything-log-eval (symbol-value hook))
    (anything-log-eval (default-value hook)))
  (run-hooks hook)
  (anything-log "executed %s" hook))
(defun anything-log-eval-internal (exprs)
  (dolist (expr exprs)
    (condition-case err
        (anything-log "%S = %S" expr (eval expr))
      (error (anything-log "%S = ERROR!" expr)))))
(defun anything-log-get-current-function ()
  "Get function name calling `anything-log'.
The original idea is from `tramp-debug-message'."
  (loop with exclude-func-re = "^anything-\\(?:interpret\\|log\\|.*funcall\\)"
        for btn from 1 to 40            ;avoid inf-loop
        for btf = (second (backtrace-frame btn))
        for fn  = (if (symbolp btf) (symbol-name btf) "")
        if (and (string-match "^anything" fn)
                (not (string-match exclude-func-re fn)))
        return fn))

(defun anything-log-error (&rest args)
  "Accumulate error messages into `anything-issued-errors'."
  (apply 'anything-log (concat "ERROR: " (car args)) (cdr args))
  (let ((msg (apply 'format args)))
    (unless (member msg anything-issued-errors)
      (add-to-list 'anything-issued-errors msg))))

(defvar anything-last-log-file nil)
(defun anything-log-save-maybe ()
  (when (stringp anything-debug)
    (let ((logdir (expand-file-name (format-time-string "%Y%m%d")
                                    anything-debug)))
      (make-directory logdir t)
      (with-current-buffer (get-buffer-create "*Anything Log*")
        (write-region (point-min) (point-max)
                      (setq anything-last-log-file
                            (expand-file-name (format-time-string "%Y%m%d-%H%M%S")
                                              logdir))
                      nil 'silent)
        (erase-buffer)))))

(defun anything-open-last-log ()
  "Open anything log file of last anything session."
  (interactive)
  (if anything-last-log-file
      (view-file anything-last-log-file)
    (switch-to-buffer "*Anything Log*")))

(defun anything-print-error-messages ()
  "Print error messages in `anything-issued-errors'."
  (message "%s" (mapconcat 'identity (reverse anything-issued-errors) "\n")))




;; (anything-log "test")
;; (switch-to-buffer-other-window "*Anything Log*")

;; (@* "Programming Tools")
(defmacro anything-aif (test-form then-form &rest else-forms)
  "Anaphoric if. Temporary variable `it' is the result of test-form."
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))  
(put 'anything-aif 'lisp-indent-function 2)

(defun anything-mklist (obj)
  "If OBJ is a list (but not lambda), return itself, otherwise make a list with one element."
  (if (and (listp obj) (not (functionp obj)))
      obj
    (list obj)))

;; (@* "Anything API")
(defmacro anything-let (varlist &rest body)
  "[OBSOLETE] Like `let'. Bind anything buffer local variables according to VARLIST then eval BODY."
  `(anything-let-internal (anything-let-eval-varlist ',varlist)
                          (lambda () ,@body)))
(put 'anything-let 'lisp-indent-function 1)

(defmacro anything-let* (varlist &rest body)
  "[OBSOLETE] Like `let*'. Bind anything buffer local variables according to VARLIST then eval BODY."
  `(anything-let-internal (anything-let*-eval-varlist ',varlist)
                          (lambda () ,@body)))
(put 'anything-let* 'lisp-indent-function 1)

(defun anything-buffer-get ()
  "If *anything action* buffer is shown, return `anything-action-buffer', otherwise `anything-buffer'."
  (if (anything-action-window)
      anything-action-buffer
    anything-buffer))

(defun anything-window ()
  "Window of `anything-buffer'."
  (get-buffer-window (anything-buffer-get) 'visible))

(defun anything-action-window ()
  "Window of `anything-action-buffer'."
  (get-buffer-window anything-action-buffer 'visible))

(defmacro with-anything-window (&rest body)
  `(let ((--tmpfunc-- (lambda () ,@body)))
     (if anything-test-mode
         (with-current-buffer (anything-buffer-get)
           (funcall --tmpfunc--))
       (with-selected-window (anything-window)
         (funcall --tmpfunc--)))))
(put 'with-anything-window 'lisp-indent-function 0)

(defun anything-deferred-action-function ()
  (dolist (f deferred-action-list) (funcall f)))
(defmacro with-anything-restore-variables(&rest body)
  "Restore variables specified by `anything-restored-variables' after executing BODY . "
  `(let ((--orig-vars (mapcar (lambda (v) (cons v (symbol-value v)))
                              anything-restored-variables))
         (deferred-action-function 'anything-deferred-action-function))
     (anything-log "save variables: %S" --orig-vars)
     (unwind-protect (progn ,@body)
       (loop for (var . value) in --orig-vars
             do (set var value))
       (anything-log "restore variables"))))
(put 'with-anything-restore-variables 'lisp-indent-function 0)

(defun* anything-attr (attribute-name &optional (src (anything-get-current-source)))
  "Get the value of ATTRIBUTE-NAME of SRC (source).
if SRC is omitted, use current source.
It is useful to write your sources."
  (anything-aif (assq attribute-name src)
      (cdr it)))

(defun* anything-attr* (attribute-name &optional (src (anything-get-current-source)))
  "Get the value of ATTRIBUTE-NAME of SRC (source) and pass to `anything-interpret-value'.
if SRC is omitted, use current source.
It is useful to write your sources."
  (anything-interpret-value (anything-attr attribute-name src)))

(defun* anything-attr-defined (attribute-name &optional (src (anything-get-current-source)))
  "Return non-nil if ATTRIBUTE-NAME of SRC (source)  is defined.
if SRC is omitted, use current source.
It is useful to write your sources."
  (and (assq attribute-name src) t))

(defun* anything-attrset (attribute-name value &optional (src (anything-get-current-source)))
  "Set the value of ATTRIBUTE-NAME of SRC (source) to VALUE.
if SRC is omitted, use current source.
It is useful to write your sources."
  (anything-aif (assq attribute-name src)
      (setcdr it value)
    (setcdr src (cons (cons attribute-name value) (cdr src))))
  value)

;; anything-set-source-filter
;;
;;   This function sets a filter for anything sources and it may be
;;   called while anything is running. It can be used to toggle
;;   displaying of sources dinamically. For example, additional keys
;;   can be bound into `anything-map' to display only the file-related
;;   results if there are too many matches from other sources and
;;   you're after files only:
;;
;;   Shift+F shows only file results from some sources:
;;
;;     (define-key anything-map "F" 'anything-my-show-files-only)
;;     
;;     (defun anything-my-show-files-only ()
;;       (interactive)
;;       (anything-set-source-filter '("File Name History"
;;                                     "Files from Current Directory")))
;;
;;   Shift+A shows all results:
;;
;;     (define-key anything-map "A" 'anything-my-show-all)
;;     
;;     (defun anything-my-show-all ()
;;       (interactive)
;;       (anything-set-source-filter nil))
;;  
;;  
;;   Note that you have to prefix the functions with anything- prefix,
;;   otherwise they won't be bound when Anything is used under
;;   Iswitchb. The -my- part is added to avoid collisions with
;;   existing Anything function names.
;;  
(defun anything-set-source-filter (sources)
  "Sets the value of `anything-source-filter' and updates the list of results."
  (unless (and (listp sources)
               (loop for name in sources always (stringp name)))
    (error "invalid data in `anything-set-source-filter': %S" sources))
  (setq anything-source-filter sources)
  (anything-log-eval anything-source-filter)
  (anything-update))

(defun anything-set-sources (sources &optional no-init no-update)
  "Set `anything-sources' during `anything' invocation.
If NO-INIT is non-nil, skip executing init functions of SOURCES.
If NO-UPDATE is non-nil, skip executing `anything-update'."
  (with-current-buffer anything-buffer
    (setq anything-compiled-sources nil
          anything-sources sources
          anything-last-sources-local sources)
    (anything-log-eval anything-compiled-sources anything-sources))
  (unless no-init (anything-funcall-foreach 'init))
  (unless no-update (anything-update)))

(defvar anything-compile-source-functions
  '(anything-compile-source--type
    anything-compile-source--dummy
    anything-compile-source--disable-shortcuts
    anything-compile-source--candidates-in-buffer)
  "Functions to compile elements of `anything-sources' (plug-in).")

(defun anything-get-sources ()
  "Return compiled `anything-sources', which is memoized.

Attributes:

- type
  `anything-type-attributes' are merged in.
- candidates-buffer
  candidates, volatile and match attrubute are created.
"
  (cond
   ;; action
   ((anything-action-window)
    anything-sources)
   ;; memoized
   (anything-compiled-sources)
   ;; first time
   (t
    (prog1
        (setq anything-compiled-sources
              (anything-compile-sources
               anything-sources anything-compile-source-functions))
      (anything-log-eval anything-compiled-sources)))))

(defun* anything-get-selection (&optional (buffer nil buffer-s) (force-display-part))
  "Return the currently selected item or nil.
if BUFFER is nil or unspecified, use anything-buffer as default value.
If FORCE-DISPLAY-PART is non-nil, return the display string."
  (setq buffer (if (and buffer buffer-s) buffer anything-buffer))
  (unless (anything-empty-buffer-p buffer)
    (with-current-buffer buffer
      (let ((selection
             (or (and (not force-display-part)
                      (get-text-property (overlay-start
                                          anything-selection-overlay)
                                         'anything-realvalue))
                 (let ((disp (buffer-substring-no-properties
                              (overlay-start anything-selection-overlay)
                              (1- (overlay-end anything-selection-overlay))))
                       (source (anything-get-current-source)))
                   (anything-aif (and (not force-display-part)
                                      (assoc-default 'display-to-real source))
                       (anything-funcall-with-source source it disp)
                     disp)))))
        (unless (equal selection "")
          (anything-log-eval selection)
          selection)))))

(defun anything-get-action ()
  "Return the associated action for the selected candidate.
It is a function symbol (sole action) or list of (action-display . function)."
  (unless (anything-empty-buffer-p (anything-buffer-get))
    (anything-aif (anything-attr 'action-transformer)
        (anything-composed-funcall-with-source
         (anything-get-current-source) it
         (anything-attr 'action) (anything-get-selection))
      (anything-attr 'action))))

(defun anything-get-current-source ()
  "Return the source for the current selection / in init/candidates/action/candidate-transformer/filtered-candidate-transformer function."
  (declare (special source))
  ;; The name `anything-get-current-source' should be used in init function etc.
  (if (and (boundp 'anything-source-name) (stringp anything-source-name))
      source
    (with-current-buffer (anything-buffer-get)
      (or (get-text-property (point) 'anything-source)
          (block exit
            ;; This goto-char shouldn't be necessary, but point is moved to
            ;; point-min somewhere else which shouldn't happen.
            (goto-char (overlay-start anything-selection-overlay))
            (let* ((header-pos (or (anything-get-previous-header-pos)
                                   (anything-get-next-header-pos)))
                   (source-name
                    (save-excursion
                      (unless header-pos
                        ;(message "No candidates")
                        (return-from exit nil))
                      (goto-char header-pos)
                      (anything-current-line-contents))))
              (some (lambda (source)
                      (if (equal (assoc-default 'name source) source-name)
                          source))
                    (anything-get-sources))))))))

(defun anything-buffer-is-modified (buffer)
  "Return non-nil when BUFFER is modified since `anything' was invoked."
  (let* ((b (get-buffer buffer))
         (key (concat (buffer-name b) "/" (anything-attr 'name)))
         (source-tick (or (gethash key anything-tick-hash) 0))
         (buffer-tick (buffer-chars-modified-tick b))
         (modifiedp (/= source-tick buffer-tick)))
    (puthash key buffer-tick anything-tick-hash)
    (anything-log-eval buffer modifiedp)
    modifiedp))
(defun anything-current-buffer-is-modified ()
  "Return non-nil when `anything-current-buffer' is modified since `anything' was invoked."
  (anything-buffer-is-modified anything-current-buffer))

(defvar anything-quit nil)
(defun anything-run-after-quit (function &rest args)
  "Perform an action after quitting `anything'.
The action is to call FUNCTION with arguments ARGS."
  (setq anything-quit t)
  (anything-log-eval function args)
  (apply 'run-with-idle-timer 0 nil function args)
  (anything-exit-minibuffer))

(defun define-anything-type-attribute (type definition &optional doc)
  "Register type attribute of TYPE as DEFINITION with DOC.
DOC is displayed in `anything-type-attributes' docstring.

Use this function is better than setting `anything-type-attributes' directly."
  (anything-add-type-attribute type definition)
  (and doc (anything-document-type-attribute type doc))
  nil)

(defvaralias 'anything-attributes 'anything-additional-attributes)
(defvar anything-additional-attributes nil
  "List of all `anything' attributes.")
(defun anything-document-attribute (attribute short-doc &optional long-doc)
  "Register ATTRIBUTE documentation introduced by plug-in.
SHORT-DOC is displayed beside attribute name.
LONG-DOC is displayed below attribute name and short documentation."
  (if long-doc
      (setq short-doc (concat "(" short-doc ")"))
    (setq long-doc short-doc
          short-doc ""))
  (add-to-list 'anything-additional-attributes attribute t)
  (put attribute 'anything-attrdoc
       (concat "- " (symbol-name attribute) " " short-doc "\n\n" long-doc "\n")))
(put 'anything-document-attribute 'lisp-indent-function 2)

(defun anything-require-at-least-version (version)
  "Output error message unless anything.el is older than VERSION.
This is suitable for anything applications."
  (when (and (string= "1." (substring version 0 2))
             (string-match "1\.\\([0-9]+\\)" anything-version)
             (< (string-to-number (match-string 1 anything-version))
                (string-to-number (substring version 2))))
    (error "Please update anything.el!!

M-x auto-install-batch anything

You must have auto-install.el too.
http://www.emacswiki.org/cgi-bin/wiki/download/auto-install.el
")))

(defun anything-interpret-value (value &optional source)
  "interpret VALUE as variable, function or literal.
If VALUE is a function, call it with no arguments and return the value.
If SOURCE is `anything' source, `anything-source-name' is source name.

If VALUE is a variable, return the value.

If VALUE is a symbol, but it is not a function or a variable, cause an error.

Otherwise, return VALUE itself."
  (cond ((and source (functionp value))
         (anything-funcall-with-source source value))
        ((functionp value)
         (funcall value))
        ((and (symbolp value) (boundp value))
         (symbol-value value))
        ((symbolp value)
         (error "anything-interpret-value: Symbol must be a function or a variable"))
        (t
         value)))

(defun anything-once (function &rest args)
  "Ensure FUNCTION with ARGS to be called once in `anything' session."
  (let ((spec (cons function args)))
    (unless (member spec anything-once-called-functions)
      (apply function args)
      (push spec anything-once-called-functions))))

;; (@* "Core: API helper")
(defun anything-empty-buffer-p (&optional buffer)
  (zerop (buffer-size (and buffer (get-buffer buffer)))))

(defun anything-let-eval-varlist (varlist)
  (mapcar (lambda (pair)
            (if (listp pair)
                (cons (car pair) (eval (cadr pair)))
              (cons pair nil)))
          varlist))
(defun anything-let*-eval-varlist (varlist)
  (let ((vars (mapcar (lambda (pair) (or (car-safe pair) pair)) varlist)))
    (eval `(let ,vars
             ,@(mapcar (lambda (pair)
                         (if (listp pair)
                             `(setq ,(car pair) ,(cadr pair))
                           `(setq ,pair nil)))
                       varlist)
             (mapcar (lambda (v) (cons v (symbol-value v))) ',vars)))))
(defun anything-let-internal (binding bodyfunc)
  "Evaluate BODYFUNC and Set BINDING to anything buffer-local variables.
BINDING is a list of (VARNAME . VALUE) pair."
  (setq anything-let-variables binding)
  (unwind-protect
      (funcall bodyfunc)
    (setq anything-let-variables nil)))


;; (@* "Core: tools")
(defun anything-current-line-contents ()
  "Current line strig without properties."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun anything-funcall-with-source (source func &rest args)
  "Call FUNC with ARGS with variable `anything-source-name' and `source' is bound.
FUNC can be function list. Return the result of last function call."
  (let ((anything-source-name (assoc-default 'name source))
        result)
    (anything-log-eval anything-source-name func args)
    (dolist (func (if (functionp func) (list func) func) result)
      (setq result (apply func args)))))

(defun anything-funcall-foreach (sym)
  "Call the sym function(s) for each source if any."
  (dolist (source (anything-get-sources))
    (anything-aif (assoc-default sym source)
        (anything-funcall-with-source source it))))

(defun anything-normalize-sources (sources)
  "If SOURCES is only one source, make a list."
  (cond ((or (and sources               ; avoid nil
                  (symbolp sources))
             (and (listp sources) (assq 'name sources)))
         (list sources))
        (sources)
        (t anything-sources)))  

(defun anything-approximate-candidate-number ()
  "Approximate Number of candidates.
It is used to check if candidate number is 0, 1, or 2+."
  (with-current-buffer anything-buffer
    (let ((lines (1- (line-number-at-pos (1- (point-max))))))
      (if (zerop lines)
          0
        (save-excursion
          (goto-char (point-min))
          (forward-line 1)
          (if (anything-pos-multiline-p)
              (if (search-forward anything-candidate-separator nil t) 2 1)
            lines))))))

(defmacro with-anything-quittable (&rest body)
  `(let (inhibit-quit)
     (condition-case v
         (progn ,@body)
       (quit (setq anything-quit t)
             (exit-minibuffer)
             (keyboard-quit)))))
(put 'with-anything-quittable 'lisp-indent-function 0)

(defun anything-compose (arg-lst func-lst)
  "Call each function in FUNC-LST with the arguments specified in ARG-LST.
The result of each function will be the new `car' of ARG-LST.

This function allows easy sequencing of transformer functions."
  (dolist (func func-lst)
    (setcar arg-lst (apply func arg-lst)))
  (car arg-lst))

(defun anything-composed-funcall-with-source (source funcs &rest args)
  (if (functionp funcs)
      (apply 'anything-funcall-with-source source funcs args)
    (apply 'anything-funcall-with-source
           source (lambda (&rest args) (anything-compose args funcs)) args)))

(defun anything-new-timer (variable timer)
  "Set new TIMER to VARIABLE. Old timer is cancelled."
  (anything-aif (symbol-value variable)
      (cancel-timer it))
  (set variable timer))

;; (@* "Core: entry point")
(defconst anything-argument-keys
  '(:sources :input :prompt :resume :preselect :buffer :keymap))
;;;###autoload
(defun anything (&rest plist)
  "Select anything. In Lisp program, some optional arguments can be used.

PLIST is a list like (:key1 val1 :key2 val2 ...) or
 (&optional sources input prompt resume preselect buffer keymap).

Basic keywords are the following:

- :sources

  Temporary value of `anything-sources'.  It also accepts a
  symbol, interpreted as a variable of an anything source.  It
  also accepts an alist representing an anything source, which is
  detected by (assq 'name ANY-SOURCES)

- :input

  Temporary value of `anything-pattern', ie. initial input of minibuffer.

- :prompt

  Prompt other than \"pattern: \".

- :resume

  If t, Resurrect previously instance of `anything'. Skip the initialization.
  If 'noresume, this instance of `anything' cannot be resumed.

- :preselect

  Initially selected candidate. Specified by exact candidate or a regexp.
  Note that it is not working with delayed sources.

- :buffer

  `anything-buffer' instead of *anything*.

- :keymap

  `anything-map' for current `anything' session.


Of course, conventional arguments are supported, the two are same.

 (anything :sources sources :input input :prompt prompt :resume resume
           :preselect preselect :buffer buffer :keymap keymap)
 (anything sources input prompt resume preselect buffer keymap)
           

Other keywords are interpreted as local variables of this anything session.
The `anything-' prefix can be omitted. For example,

 (anything :sources 'anything-c-source-buffers
           :buffer \"*buffers*\" :candidate-number-limit 10)

means starting anything session with `anything-c-source-buffers'
source in *buffers* buffer and set
`anything-candidate-number-limit' to 10 as session local variable. "
  (interactive)
  (if (keywordp (car plist))
      (anything-let-internal
       (anything-parse-keys plist)
       (lambda ()
         (apply 'anything
                (mapcar (lambda (key) (plist-get plist key))
                        anything-argument-keys))))
    (apply 'anything-internal plist)))

(defun* anything-resume (&optional (any-buffer anything-last-buffer) buffer-pattern (any-resume t))
  "Resurrect previously invoked `anything'."
  (interactive)
  (when (or current-prefix-arg buffer-pattern)
    (setq any-buffer (anything-resume-select-buffer buffer-pattern)))
  (setq anything-compiled-sources nil)
  (anything
   (or (buffer-local-value 'anything-last-sources-local (get-buffer any-buffer))
       anything-last-sources anything-sources)
   (buffer-local-value 'anything-input-local (get-buffer any-buffer))
   nil any-resume nil any-buffer))

;;; rubikitch: experimental
;;; I use this and check it whether I am convenient.
;;; I may introduce an option to control the behavior.
(defun* anything-resume-window-only (&optional (any-buffer anything-last-buffer) buffer-pattern)
  (interactive)
  (anything-resume any-buffer buffer-pattern 'window-only))

;;;###autoload
(defun anything-at-point (&optional any-sources any-input any-prompt any-resume any-preselect any-buffer)
  "Same as `anything' except when C-u is pressed, the initial input is the symbol at point."
  (interactive)
  (anything any-sources
            (if current-prefix-arg
                (concat "\\b" (thing-at-point 'symbol) "\\b"
                        (if (featurep 'anything-match-plugin) " " ""))
              any-input)
            any-prompt any-resume any-preselect any-buffer))

;;;###autoload
(defun anything-other-buffer (any-sources any-buffer)
  "Simplified interface of `anything' with other `anything-buffer'"
  (anything any-sources nil nil nil nil any-buffer))

;;; (@* "Core: entry point helper")
(defun anything-internal (&optional any-sources any-input any-prompt any-resume any-preselect any-buffer any-keymap)
  "Older interface of `anything'. It is called by `anything'."
  (anything-log "++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
  (anything-log-eval any-prompt any-preselect any-buffer any-keymap)
  (unwind-protect
      (condition-case v
          (let ( ;; It is needed because `anything-source-name' is non-nil
                ;; when `anything' is invoked by action. Awful global scope.
                anything-source-name
                anything-in-persistent-action
                anything-quit
                (case-fold-search t)
                (anything-buffer (or any-buffer anything-buffer))
                ;; cua-mode ; avoid error when region is selected
                )
            (with-anything-restore-variables
              (anything-initialize-1 any-resume any-input any-sources)
              (anything-display-buffer anything-buffer)
              (anything-log "show prompt")
              (unwind-protect
                  (anything-read-pattern-maybe
                   any-prompt any-input any-preselect any-resume any-keymap)
                (anything-cleanup)))
            (prog1 (unless anything-quit (anything-execute-selection-action-1))
              (anything-log "end session --------------------------------------------")))
        (quit
         (anything-on-quit)
         (anything-log "end session (quit) -------------------------------------")
         nil))
    (anything-log-save-maybe)))



(defun anything-parse-keys (keys)
  (loop for (key value &rest _) on keys by #'cddr
        for symname = (substring (symbol-name key) 1)
        for sym = (intern (if (string-match "^anything-" symname)
                              symname
                            (concat "anything-" symname)))
        unless (memq key anything-argument-keys)
        collect (cons sym value)))

(defun anything-resume-p (any-resume)
  "Whethre current anything session is resumed or not."
  (memq any-resume '(t window-only)))

(defvar anything-buffers nil
  "All of `anything-buffer' in most recently used order.")
(defun anything-initialize-1 (any-resume any-input any-sources)
  "The real initialization of `anything'.

This function name should be `anything-initialize', but anything
extensions may advice `anything-initalize'. I cannot rename, sigh."
  (anything-log "start initialization: any-resume=%S any-input=%S" any-resume any-input)
  (anything-frame/window-configuration 'save)
  (setq anything-sources (anything-normalize-sources any-sources))
  (anything-log "sources = %S" anything-sources)
  (anything-hooks 'setup)
  (anything-current-position 'save)
  (if (anything-resume-p any-resume)
      (anything-initialize-overlays (anything-buffer-get))
    (anything-initialize))
  (unless (eq any-resume 'noresume)
    (anything-recent-push anything-buffer 'anything-buffers)
    (setq anything-last-buffer anything-buffer))
  (when any-input (setq anything-input any-input anything-pattern any-input))
  (and (anything-resume-p any-resume) (anything-funcall-foreach 'resume))
  (anything-log "end initialization"))

(defun anything-execute-selection-action-1 ()
  (unwind-protect
      (anything-execute-selection-action)
    (anything-aif (get-buffer anything-action-buffer)
        (kill-buffer it))
    (anything-log-run-hook 'anything-after-action-hook)))

(defun anything-on-quit ()
  (setq minibuffer-history (cons anything-input minibuffer-history))
  (anything-current-position 'restore))

(defun anything-resume-select-buffer (input)
  (anything '(((name . "Resume anything buffer")
               (candidates . anything-buffers)
               (action . identity)))
            input nil 'noresume nil "*anything resume*"))

(defun anything-recent-push (elt list-var)
  "Add ELT to the value of LIST-VAR as most recently used value."
  (let ((m (member elt (symbol-value list-var))))
    (and m (set list-var (delq (car m) (symbol-value list-var))))
    (push elt (symbol-value list-var))))

;;; (@* "Core: Accessors")
;;; rubikitch: I love to create functions to control variables.
(defvar anything-current-position nil
  "Cons of (point) and (window-start) when `anything' is invoked.
It is needed because restoring position when `anything' is keyboard-quitted.")
(defun anything-current-position (save-or-restore)
  (case save-or-restore
    (save
     (setq anything-current-position (cons (point) (window-start))))
    (restore
     (goto-char (car anything-current-position))
     (set-window-start (selected-window) (cdr anything-current-position)))))

;;; FIXME I want to remove them. But anything-iswitchb uses them.
(defun anything-current-frame/window-configuration ()
  (funcall (cdr anything-save-configuration-functions)))
(defun anything-set-frame/window-configuration (conf)
  (funcall (car anything-save-configuration-functions) conf))

(lexical-let (conf)
  (defun anything-frame/window-configuration (save-or-restore)
    (anything-log-eval anything-save-configuration-functions)
    (case save-or-restore
      (save    (setq conf (funcall (cdr anything-save-configuration-functions))))
      (restore (funcall (car anything-save-configuration-functions) conf)))))

;; (@* "Core: Display *anything* buffer")
(defun anything-display-buffer (buf)
  "Display *anything* buffer."
  (funcall (with-current-buffer buf anything-display-function) buf))

(defun anything-default-display-buffer (buf)
  (funcall (if anything-samewindow 'switch-to-buffer 'pop-to-buffer) buf))

;; (@* "Core: initialize")
(defun anything-initialize ()
  "Initialize anything settings and set up the anything buffer."
  (anything-log-run-hook 'anything-before-initialize-hook)
  (setq anything-once-called-functions nil)
  (setq anything-delayed-init-executed nil)
  (setq anything-current-buffer (current-buffer))
  (setq anything-buffer-file-name buffer-file-name)
  (setq anything-issued-errors nil)
  (setq anything-compiled-sources nil)
  (setq anything-saved-current-source nil)
  ;; Call the init function for sources where appropriate
  (anything-funcall-foreach 'init)

  (setq anything-pattern "")
  (setq anything-input "")
  (setq anything-candidate-cache nil)
  (setq anything-last-sources anything-sources)

  (anything-create-anything-buffer)
  (anything-log-run-hook 'anything-after-initialize-hook))

(defun anything-read-pattern-maybe (any-prompt any-input any-preselect any-resume any-keymap)
  (if (anything-resume-p any-resume)
      (anything-mark-current-line)
    (anything-update))
  (select-frame-set-input-focus (window-frame (minibuffer-window)))
  (anything-preselect any-preselect)
  (let ((minibuffer-local-map
         (with-current-buffer (anything-buffer-get)
           (and any-keymap (set (make-local-variable 'anything-map) any-keymap))
           anything-map)))
    (anything-log-eval (anything-approximate-candidate-number)
                       anything-execute-action-at-once-if-one
                       anything-quit-if-no-candidate)
    (cond ((and anything-execute-action-at-once-if-one
                (= (anything-approximate-candidate-number) 1))
           (ignore))
          ((and anything-quit-if-no-candidate
                (= (anything-approximate-candidate-number) 0))
           (setq anything-quit t)
           (and (functionp anything-quit-if-no-candidate)
                (funcall anything-quit-if-no-candidate)))
          (t
           (read-string (or any-prompt "pattern: ") any-input)))))

(defun anything-create-anything-buffer (&optional test-mode)
  "Create newly created `anything-buffer'.
If TEST-MODE is non-nil, clear `anything-candidate-cache'."
  (when test-mode
    (setq anything-candidate-cache nil))
  (with-current-buffer (get-buffer-create anything-buffer)
    (anything-log "kill local variables: %S" (buffer-local-variables))
    (kill-all-local-variables)
    (buffer-disable-undo)
    (erase-buffer)
    (set (make-local-variable 'inhibit-read-only) t)
    (set (make-local-variable 'anything-last-sources-local) anything-sources)
    (set (make-local-variable 'anything-follow-mode) nil)
    (set (make-local-variable 'anything-display-function) anything-display-function)
    (anything-log-eval anything-display-function anything-let-variables)
    (loop for (var . val) in anything-let-variables
          do (set (make-local-variable var) val))
    
    (setq cursor-type nil)
    (setq mode-name "Anything"))
  (anything-initialize-overlays anything-buffer)
  (get-buffer anything-buffer))

(defun anything-initialize-overlays (buffer)
  (anything-log "overlay setup")
  (if anything-selection-overlay
      ;; make sure the overlay belongs to the anything buffer if
      ;; it's newly created
      (move-overlay anything-selection-overlay (point-min) (point-min)
                    (get-buffer buffer))

    (setq anything-selection-overlay 
          (make-overlay (point-min) (point-min) (get-buffer buffer)))
    (overlay-put anything-selection-overlay 'face anything-selection-face))

  (cond (anything-enable-shortcuts
         (setq anything-shortcut-keys
               (assoc-default anything-enable-shortcuts anything-shortcut-keys-alist))
         (unless anything-digit-overlays
           (setq anything-digit-overlays
                 (loop for key across anything-shortcut-keys
                       for overlay = (make-overlay (point-min) (point-min)
                                                   (get-buffer buffer))
                       do (overlay-put overlay 'before-string
                                       (format "%s - " (upcase (make-string 1 key))))
                       collect overlay))))
        (anything-digit-overlays
         (mapc 'delete-overlay anything-digit-overlays)
         (setq anything-digit-overlays nil))))

(defun anything-hooks (setup-or-cleanup)
  (let ((hooks '((deferred-action-list anything-check-minibuffer-input)
                 (minibuffer-setup-hook anything-print-error-messages))))
    (if (eq setup-or-cleanup 'setup)
        (dolist (args hooks) (apply 'add-hook args))
      (dolist (args (reverse hooks)) (apply 'remove-hook args)))))

;; (@* "Core: clean up")
;;; TODO move
(defun anything-cleanup ()
  "Clean up the mess."
  (anything-log "start cleanup")
  (with-current-buffer anything-buffer
    (setq cursor-type t))
  (bury-buffer anything-buffer)
  (anything-funcall-foreach 'cleanup)
  (anything-new-timer 'anything-check-minibuffer-input-timer nil)
  (anything-kill-async-processes)
  (anything-log-run-hook 'anything-cleanup-hook)
  (anything-hooks 'cleanup)
  (anything-frame/window-configuration 'restore))

;; (@* "Core: input handling")
(defun anything-check-minibuffer-input ()
  "Extract input string from the minibuffer and check if it needs
to be handled."
  (let ((delay (with-current-buffer anything-buffer anything-input-idle-delay)))
    (if (or (not delay) (anything-action-window))
       (anything-check-minibuffer-input-1)
     (anything-new-timer
      'anything-check-minibuffer-input-timer
      (run-with-idle-timer delay nil 'anything-check-minibuffer-input-1)))))

(defun anything-check-minibuffer-input-1 ()
  (with-anything-quittable
    (with-selected-window (minibuffer-window)
      (anything-check-new-input (minibuffer-contents)))))

(defun anything-check-new-input (input)
  "Check input string and update the anything buffer if
necessary."
  (unless (equal input anything-pattern)
    (setq anything-pattern input)
    (unless (anything-action-window)
      (setq anything-input anything-pattern))
    (anything-log-eval anything-pattern anything-input)
    (anything-update)))

;; (@* "Core: source compiler")
(defvar anything-compile-source-functions-default anything-compile-source-functions
  "Plug-ins this file provides.")
(defun anything-compile-sources (sources funcs)
  "Compile sources (`anything-sources') with funcs (`anything-compile-source-functions').
Anything plug-ins are realized by this function."
  (mapcar
   (lambda (source)
     (loop with source = (if (listp source) source (symbol-value source))
           for f in funcs
           do (setq source (funcall f source))
           finally (return source)))
   sources))  

;; (@* "Core: plug-in attribute documentation hack")

;; `anything-document-attribute' is public API.
(defadvice documentation-property (after anything-document-attribute activate)
  "Hack to display plug-in attributes' documentation as `anything-sources' docstring."
  (when (eq symbol 'anything-sources)
    (setq ad-return-value
          (concat ad-return-value "\n"
                  (mapconcat (lambda (sym) (get sym 'anything-attrdoc))
                             anything-additional-attributes
                             "\n")))))
;; (describe-variable 'anything-sources)
;; (documentation-property 'anything-sources 'variable-documentation)
;; (progn (ad-disable-advice 'documentation-property 'after 'anything-document-attribute) (ad-update 'documentation-property)) 

;; (@* "Core: all candidates")
(defun anything-process-delayed-init (source)
  (let ((name (assoc-default 'name source)))
    (unless (member name anything-delayed-init-executed)
      (anything-aif (assoc-default 'delayed-init source)
          (with-current-buffer anything-current-buffer
            (anything-funcall-with-source source it)
            (dolist (f (if (functionp it) (list it) it))
              (add-to-list 'anything-delayed-init-executed name)))))))

(defun anything-get-candidates (source)
  "Retrieve and return the list of candidates from
SOURCE."
  (anything-process-delayed-init source)
  (let* ((candidate-source (assoc-default 'candidates source))
         (type-error (lambda ()
                       (error (concat "Candidates must either be a function, "
                                      " a variable or a list: %s")
                              candidate-source)))
         (candidates (condition-case err
                         (anything-interpret-value candidate-source source)
                       (error (funcall type-error)))))
    (cond ((processp candidates) candidates)
          ((listp candidates) (anything-transform-candidates candidates source))
          (t (funcall type-error)))))
         

(defun anything-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is not yet a cached value."
  (let* ((name (assoc-default 'name source))
         (candidate-cache (assoc name anything-candidate-cache)))
    (cond (candidate-cache
           (anything-log "use cached candidates")
           (cdr candidate-cache))
          (t
           (anything-log "calculate candidates")
           (let ((candidates (anything-get-candidates source)))
             (cond ((processp candidates)
                    (push (cons candidates
                                (append source 
                                        (list (cons 'item-count 0)
                                              (cons 'incomplete-line ""))))
                          anything-async-processes)
                    (set-process-filter candidates 'anything-output-filter)
                    (setq candidates nil))
                   ((not (assoc 'volatile source))
                    (setq candidate-cache (cons name candidates))
                    (push candidate-cache anything-candidate-cache)))
             candidates)))))

;;; (@* "Core: candidate transformers")
(defun anything-process-candidate-transformer (candidates source)
  (anything-aif (assoc-default 'candidate-transformer source)
      (anything-composed-funcall-with-source source it candidates)
    candidates))
(defun anything-process-filtered-candidate-transformer (candidates source)
  (anything-aif (assoc-default 'filtered-candidate-transformer source)
      (anything-composed-funcall-with-source source it candidates source)
    candidates))
(defun anything-process-filtered-candidate-transformer-maybe (candidates source process-p)
  (if process-p
      (anything-process-filtered-candidate-transformer candidates source)
    candidates))
(defun anything-process-real-to-display (candidates source)
  (anything-aif (assoc-default 'real-to-display source)
      (setq candidates (anything-funcall-with-source
                        source 'mapcar
                        (lambda (cand_)
                          (if (consp cand_)
                              ;; override DISPLAY from candidate-transformer
                              (cons (funcall it (cdr cand_)) (cdr cand_))
                            (cons (funcall it cand_) cand_)))
                        candidates))
    candidates))
(defun anything-transform-candidates (candidates source &optional process-p)
  "Transform CANDIDATES according to candidate transformers."
  (anything-process-real-to-display
   (anything-process-filtered-candidate-transformer-maybe
    (anything-process-candidate-transformer candidates source) source process-p)
   source))


;; (@* "Core: narrowing candidates")
(defun anything-candidate-number-limit (source)
  "`anything-candidate-number-limit' variable may be overridden by SOURCE.
If (candidate-number-limit) is in SOURCE, show all candidates in SOURCE,
ie. cancel the effect of `anything-candidate-number-limit'."
  (anything-aif (assq 'candidate-number-limit source)
      (or (cdr it) 99999999)
    (or anything-candidate-number-limit 99999999)))

(defconst anything-default-match-functions
  (list (lambda (candidate)
          (string-match anything-pattern candidate))))

(defun anything-compute-matches (source)
  "Compute matches from SOURCE according to its settings."
  (if debug-on-error
      (anything-compute-matches-internal source)
    (condition-case v
        (anything-compute-matches-internal source)
      (error (anything-log-error
              "anything-compute-matches: error when processing source: %s"
              (assoc-default 'name source))
             nil))))

(defun anything-candidate-get-display (candidate)
  "Get display part (searched) from CANDIDATE.
CANDIDATE is a string, a symbol, or (DISPLAY . REAL) cons cell."
  (format "%s" (or (car-safe candidate) candidate)))

(defun anything-process-pattern-transformer (pattern source)
  (anything-aif (assoc-default 'pattern-transformer source)
      (anything-composed-funcall-with-source source it pattern)
    pattern))

(defun anything-match-functions (source)
  (or (assoc-default 'match source)
      anything-default-match-functions))

(defmacro anything-accumulate-candidates-internal (cand newmatches hash item-count limit)
  "INTERNAL: add CAND (ITEM-COUNT th match) into NEWMATCHES.
Use HASH to uniq NEWMATCHES.
if ITEM-COUNT reaches LIMIT, exit from inner loop."
  `(unless (gethash ,cand ,hash)
     (puthash ,cand t ,hash)
     (push ,cand ,newmatches)
     (incf ,item-count)
     (when (= ,item-count ,limit)
       (setq exit t)
       (return))))

(defun anything-take-first-elements (seq n)
  (if (> (length seq) n)
      (setq seq (subseq seq 0 n))
    seq))

(defun anything-match-from-candidates (cands matchfns limit)
  (let (matches)
    (condition-case nil
        (let ((item-count 0) exit)
          (clrhash anything-match-hash)
          (dolist (match matchfns)
            (let (newmatches)
              (dolist (candidate cands)
                (when (funcall match (anything-candidate-get-display candidate))
                  (anything-accumulate-candidates-internal
                   candidate newmatches anything-match-hash item-count limit)))
              (setq matches (append matches (reverse newmatches)))
              (if exit (return)))))
      (invalid-regexp (setq matches nil)))
    matches))

(defun anything-compute-matches-internal (source)
  (let ((matchfns (anything-match-functions source))
        (anything-source-name (assoc-default 'name source))
        (limit (anything-candidate-number-limit source))
        (anything-pattern (anything-process-pattern-transformer
                           anything-pattern source)))
    (anything-process-filtered-candidate-transformer
     (if (or (equal anything-pattern "") (equal matchfns '(identity)))
         (anything-take-first-elements
          (anything-get-cached-candidates source) limit)
       (anything-match-from-candidates
        (anything-get-cached-candidates source) matchfns limit))
     source)))

;; (anything '(((name . "error")(candidates . (lambda () (hage))) (action . identity))))

(defun anything-process-source (source)
  "Display matches from SOURCE according to its settings."
  (anything-log-eval (assoc-default 'name source))
  (if (assq 'direct-insert-match source) ;experimental
      (anything-process-source--direct-insert-match source)
    (let ((matches (anything-compute-matches source)))
      (when matches
        (when anything-test-mode
          (setq anything-test-candidate-list
                `(,@anything-test-candidate-list
                  (,(assoc-default 'name source)
                   ,matches))))
        (anything-insert-header-from-source source)
        (if (not (assq 'multiline source))
            (mapc 'anything-insert-match-with-digit-overlay matches)
          (let ((start (point)) separate)
            (dolist (match matches)
              (if separate
                  (anything-insert-candidate-separator)
                (setq separate t))
              (anything-insert-match-with-digit-overlay match))
            (put-text-property start (point) 'anything-multiline t)))))))

(defun anything-insert-match-with-digit-overlay (match)
  (declare (special source))
  (anything-put-digit-overlay-maybe)
  (anything-insert-match match 'insert source))

(defun anything-put-digit-overlay-maybe ()
  (when (and anything-enable-shortcuts
             (not (eq anything-digit-shortcut-count
                      (length anything-digit-overlays))))
    (move-overlay (nth anything-digit-shortcut-count
                       anything-digit-overlays)
                  (point-at-bol)
                  (point-at-bol))
    (incf anything-digit-shortcut-count)))

(defun anything-process-source--direct-insert-match (source)
  "[EXPERIMENTAL] Insert candidates from `anything-candidate-buffer'"
  (anything-log-eval (assoc-default 'name source))
  (let ((anything-source-name (assoc-default 'name source))
        content-buf)
    (funcall (assoc-default 'candidates source))
    (setq content-buf (anything-candidate-buffer))
    (unless (anything-empty-buffer-p content-buf)
      (anything-insert-header-from-source source)
      (insert-buffer-substring content-buf)
      ;; TODO call anything-put-digit-overlay-maybe with loop
      )))

(defun anything-process-delayed-sources (delayed-sources)
  "Process delayed sources if the user is idle for
`anything-idle-delay' seconds."
  (with-anything-quittable
    (anything-log-eval (mapcar (lambda (s) (assoc-default 'name s)) delayed-sources))
    (with-current-buffer anything-buffer        
      (save-excursion
        (goto-char (point-max))
        (mapc 'anything-process-source delayed-sources)
        (when (and (not (anything-empty-buffer-p))
                   ;; no selection yet
                   (= (overlay-start anything-selection-overlay)
                      (overlay-end anything-selection-overlay)))
          (goto-char (point-min))
          (anything-next-line)))
      (save-excursion
        (goto-char (point-min))
        (anything-log-run-hook 'anything-update-hook))
      (anything-maybe-fit-frame))))

;; (@* "Core: *anything* buffer contents")
(defvar anything-input-local nil)
(defvar anything-process-delayed-sources-timer nil)
(defun anything-update ()
  "Update the list of matches in the anything buffer according to
the current pattern."
  (anything-log "start update")
  (setq anything-digit-shortcut-count 0)
  (anything-kill-async-processes)
  (with-current-buffer (anything-buffer-get)
    (set (make-local-variable 'anything-input-local) anything-pattern)
    (erase-buffer)
    (when anything-enable-shortcuts
      (mapc 'delete-overlay anything-digit-overlays))
    (let (delayed-sources)
      (unwind-protect
          (setq delayed-sources
                (loop for source in (remove-if-not 'anything-update-source-p
                                                   (anything-get-sources))
                      if (anything-delayed-source-p source)
                      collect source
                      else do (anything-process-source source)))
        (anything-log-eval
         (mapcar (lambda (s) (assoc-default 'name s)) delayed-sources))
        (anything-update-move-first-line)
        (if anything-test-mode
            (mapc 'anything-process-source delayed-sources)
          (anything-maybe-fit-frame)
          (when delayed-sources
            (anything-new-timer
             'anything-process-delayed-sources-timer
             (run-with-idle-timer
              anything-idle-delay nil
              'anything-process-delayed-sources delayed-sources)))
          ;; FIXME I want to execute anything-after-update-hook
          ;; AFTER processing delayed sources
          (anything-log-run-hook 'anything-after-update-hook))
        (anything-log "end update")))))

(defun anything-update-source-p (source)
  (and (or (not anything-source-filter)
           (member (assoc-default 'name source) anything-source-filter))
       (>= (length anything-pattern)
           (anything-aif (assoc 'requires-pattern source)
               (or (cdr it) 1)
             0))))
(defun anything-delayed-source-p (source)
  (or (assoc 'delayed source)
      (and anything-quick-update
           (< (window-height (get-buffer-window (current-buffer)))
              (line-number-at-pos (point-max))))))

(defun anything-update-move-first-line ()
  (goto-char (point-min))
  (save-excursion (anything-log-run-hook 'anything-update-hook))
  (anything-next-line))

(defun anything-force-update ()
  "Recalculate and update candidates.
If current source has `update' attribute, a function without argument, call it before update."
  (interactive)
  (let ((source (anything-get-current-source)))
    (if source
        (anything-force-update--reinit source)
      (anything-erase-message)
      (mapc 'anything-force-update--reinit (anything-get-sources)))
    (let ((selection (anything-get-selection nil t)))
      (anything-update)
      (anything-keep-selection source selection))))

(defun anything-force-update--reinit (source)
  (anything-aif (anything-funcall-with-source source 'anything-candidate-buffer)
      (kill-buffer it))
  (dolist (attr '(update init))
    (anything-aif (assoc-default attr source)
        (anything-funcall-with-source source it)))
  (anything-remove-candidate-cache source))

(defun anything-erase-message ()
  (message ""))

(defun anything-keep-selection (source selection)
  (when (and source selection)
    (with-anything-window
      (anything-goto-source source)
      (forward-char -1)                  ;back to \n
      (if (search-forward (concat "\n" selection "\n") nil t)
          (forward-line -1)
        (goto-char (point-min))
        (forward-line 1))
      (anything-mark-current-line))))

(defun anything-remove-candidate-cache (source)
  (setq anything-candidate-cache
        (delete (assoc (assoc-default 'name source) anything-candidate-cache)
                anything-candidate-cache)))

(defun anything-insert-match (match insert-function source)
  "Insert MATCH into the anything buffer. If MATCH is a list then
insert the string inteneded to appear on the display and store
the real value in a text property."
  (let ((start (point-at-bol (point)))
        (string (or (car-safe match) match))
        (realvalue (cdr-safe match)))
    (when (symbolp string) (setq string (symbol-name string)))
    (when (stringp string)
      (funcall insert-function string)
      ;; Some sources with candidates-in-buffer have already added
      ;; 'anything-realvalue property when creating candidate buffer.
      (unless (get-text-property start 'anything-realvalue)
        (and realvalue
             (put-text-property start (point-at-eol)
                                'anything-realvalue realvalue)))
      (when anything-source-in-each-line-flag
        (put-text-property start (point-at-eol) 'anything-source source))
      (funcall insert-function "\n"))))

(defun anything-insert-header-from-source (source)
  (let ((name (assoc-default 'name source)))
    (anything-insert-header
     name
     (anything-aif (assoc-default 'header-name source)
         (anything-funcall-with-source source it name)))))

(defun anything-insert-header (name &optional display-string)
  "Insert header of source NAME into the anything buffer."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (put-text-property start (point) 'anything-header-separator t)))

  (let ((start (point)))
    (insert name)
    (put-text-property (point-at-bol)
                       (point-at-eol) 'anything-header t)
    (when display-string
      (overlay-put (make-overlay (point-at-bol) (point-at-eol))
                   'display display-string))
    (insert "\n")
    (put-text-property start (point) 'face anything-header-face)))


(defun anything-insert-candidate-separator ()
  "Insert separator of candidates into the anything buffer."
  (insert anything-candidate-separator)
  (put-text-property (point-at-bol)
                     (point-at-eol) 'anything-candidate-separator t)
  (insert "\n"))




;; (@* "Core: async process")
(defun anything-output-filter (process string)
  "Process output from PROCESS."
  (anything-output-filter-1 (assoc process anything-async-processes) string))

(defun anything-output-filter-1 (process-assoc string)
  (anything-log-eval string)
  (with-current-buffer anything-buffer
    (let ((source (cdr process-assoc)))
      (save-excursion
       (anything-aif (assoc-default 'insertion-marker source)
           (goto-char it)
         (goto-char (point-max))
         (anything-insert-header-from-source source)
         (setcdr process-assoc
                 (append source `((insertion-marker . ,(point-marker))))))
       (anything-output-filter--process-source
        (car process-assoc) string source
        (anything-candidate-number-limit source))))
    (anything-output-filter--post-process)))

(defun anything-output-filter--process-source (process string source limit)
  (dolist (candidate (anything-transform-candidates
                      (anything-output-filter--collect-candidates
                       (split-string string "\n")
                       (assoc 'incomplete-line source))
                      source t))
    (if (not (assq 'multiline source))
        (anything-insert-match candidate 'insert-before-markers source)
      (let ((start (point)))
        (anything-insert-candidate-separator)
        (anything-insert-match candidate 'insert-before-markers source)
        (put-text-property start (point) 'anything-multiline t)))
    (incf (cdr (assoc 'item-count source)))
    (when (>= (assoc-default 'item-count source) limit)
      (anything-kill-async-process process)
      (return))))

(defun anything-output-filter--collect-candidates (lines incomplete-line-info)
  (anything-log-eval (cdr incomplete-line-info))
  (butlast
   (loop for line in lines collect
         (if (cdr incomplete-line-info)
             (prog1
                 (concat (cdr incomplete-line-info) line)
               (setcdr incomplete-line-info nil))
           line)
         finally (setcdr incomplete-line-info line))))

(defun anything-output-filter--post-process ()
  (anything-maybe-fit-frame)
  (anything-log-run-hook 'anything-update-hook)
  (save-selected-window
    (select-window (get-buffer-window anything-buffer 'visible))
    (anything-skip-noncandidate-line 'next)
    (anything-mark-current-line)))


(defun anything-kill-async-processes ()
  "Kill all known asynchronous processes according to
`anything-async-processes'."
    "Kill locate process."
    (mapc 'anything-kill-async-process (mapcar 'car anything-async-processes))
    (setq anything-async-processes nil))


(defun anything-kill-async-process (process)
  "Kill PROCESS and detach the associated functions."
  (set-process-filter process nil)
  (delete-process process))
  

;; (@* "Core: action")
(defun anything-execute-selection-action (&optional selection action preserve-saved-action)
  "If a candidate was selected then perform the associated
action."
  (anything-log "executing action")
  (setq action (anything-get-default-action
                (or action
                    anything-saved-action
                    (if (get-buffer anything-action-buffer)
                        (anything-get-selection anything-action-buffer)
                      (anything-get-action)))))
  (let ((source (or anything-saved-current-source (anything-get-current-source))))
    (setq selection (or selection
                        (anything-get-selection)
                        (and (assoc 'accept-empty source) "")))
    (unless preserve-saved-action (setq anything-saved-action nil))
    (if (and selection action)
        (anything-funcall-with-source
         source action
         (anything-coerce-selection selection source)))))

(defun anything-coerce-selection (selection source)
  "Coerce source with coerce function."
  (anything-aif (assoc-default 'coerce source)
             (anything-funcall-with-source source it selection)
           selection))

(defun anything-get-default-action (action)
  (if (and (listp action) (not (functionp action)))
      (cdar action)
    action))

(defun anything-select-action ()
  "Select an action for the currently selected candidate.
If action buffer is selected, back to the anything buffer."
  (interactive)
  (cond ((get-buffer-window anything-action-buffer 'visible)
         (set-window-buffer (get-buffer-window anything-action-buffer)
                            anything-buffer)
         (kill-buffer anything-action-buffer)
         (anything-set-pattern anything-input 'noupdate))
        (t
         (setq anything-saved-selection (anything-get-selection))
         (unless anything-saved-selection
           (error "Nothing is selected."))
         (setq anything-saved-current-source (anything-get-current-source))
         (let ((actions (anything-get-action)))
           (if (functionp actions)
               (message "Sole action: %s" actions)
             (anything-show-action-buffer actions)
             (anything-delete-minibuffer-contents)
             (setq anything-pattern 'dummy) ; so that it differs from the previous one
             (anything-check-minibuffer-input))))))

(defun anything-show-action-buffer (actions)
  (with-current-buffer (get-buffer-create anything-action-buffer)
    (erase-buffer)
    (buffer-disable-undo)
    (set-window-buffer (get-buffer-window anything-buffer) anything-action-buffer)
    (set (make-local-variable 'anything-sources)
         `(((name . "Actions")
            (volatile)
            (candidates . ,actions)
            (candidate-number-limit))))
    (set (make-local-variable 'anything-source-filter) nil)
    (set (make-local-variable 'anything-selection-overlay) nil)
    (set (make-local-variable 'anything-digit-overlays) nil)
    (anything-initialize-overlays anything-action-buffer)))

;; (@* "Core: selection")
(defun anything-move-selection-common (move-func unit direction)
  "Move the selection marker to a new position determined by
UNIT and DIRECTION."
  (unless (or (anything-empty-buffer-p (anything-buffer-get))
              (not (anything-window)))
    (with-anything-window
      (funcall move-func)
      (anything-skip-noncandidate-line direction)
      (anything-display-source-at-screen-top-maybe unit)
      (when (anything-get-previous-header-pos)
        (anything-mark-current-line))
      (anything-display-mode-line (anything-get-current-source)))))

(defun anything-display-source-at-screen-top-maybe (unit)
  (when (and anything-display-source-at-screen-top (eq unit 'source))
    (set-window-start (selected-window)
                      (save-excursion (forward-line -1) (point)))))

(defun anything-skip-noncandidate-line (direction)
  (anything-skip-header-and-separator-line direction)
  (and (bobp) (forward-line 1))     ;skip first header
  (and (eobp) (forward-line -1))    ;avoid last empty line
  )

(defun anything-skip-header-and-separator-line (direction)
  (while (and (not (bobp))
              (or (anything-pos-header-line-p)
                  (anything-pos-candidate-separator-p)))
    (forward-line (if (and (eq direction 'previous)
                           (not (eq (point-at-bol) (point-min))))
                      -1
                    1))))

(defvar anything-mode-line-string-real nil)
(defun anything-display-mode-line (source)
  (set (make-local-variable 'anything-mode-line-string)
       (anything-interpret-value (or (assoc-default 'mode-line source)
                                     (default-value 'anything-mode-line-string))
                                 source))
  (if anything-mode-line-string
      (setq mode-line-format
            '(" " mode-line-buffer-identification " "
              (line-number-mode "%l") " " (anything-follow-mode "(F)")
              " " anything-mode-line-string-real "-%-")
            anything-mode-line-string-real
            (substitute-command-keys anything-mode-line-string))
    (setq mode-line-format
          (default-value 'mode-line-format)))
  (setq header-line-format
        (anything-interpret-value (assoc-default 'header-line source) source)))

(defun anything-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (if (not (anything-pos-multiline-p))
         (forward-line -1)      ;double forward-line is meaningful
       (forward-line -1)        ;because evaluation order is important
       (anything-skip-header-and-separator-line 'previous)
       (let ((header-pos (anything-get-previous-header-pos))
             (separator-pos (anything-get-previous-candidate-separator-pos)))
         (when header-pos
           (goto-char (if (or (null separator-pos) (< separator-pos header-pos))
                          header-pos ; first candidate
                        separator-pos))
           (forward-line 1)))))
   'line 'previous))

(defun anything-next-line ()
  "Move selection to the next line."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (if (not (anything-pos-multiline-p))
         (forward-line 1)
       (let ((header-pos (anything-get-next-header-pos))
             (separator-pos (anything-get-next-candidate-separator-pos)))
         (cond ((and separator-pos
                     (or (null header-pos) (< separator-pos header-pos)))
                (goto-char separator-pos))
               (header-pos
                (goto-char header-pos))))))
   'line 'next))

(defun anything-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (condition-case nil
         (scroll-down)
       (beginning-of-buffer (goto-char (point-min)))))
   'page 'previous))

(defun anything-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (condition-case nil
         (scroll-up)
       (end-of-buffer (goto-char (point-max)))))
   'page 'next))

(defun anything-beginning-of-buffer ()
  "Move selection at the top."
  (interactive)
  (anything-move-selection-common (lambda () (goto-char (point-min)))
                                  'edge 'previous))

(defun anything-end-of-buffer ()
  "Move selection at the bottom."
  (interactive)
  (anything-move-selection-common (lambda () (goto-char (point-max)))
                                  'edge 'next))

(defun anything-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (anything-move-selection-common
   (lambda ()
     (forward-line -1)
     (if (bobp)
         (goto-char (point-max))
       (anything-skip-header-and-separator-line 'previous))
     (goto-char (anything-get-previous-header-pos))
     (forward-line 1))
   'source 'previous))

(defun anything-next-source ()
  "Move selection to the next source."
  (interactive)
  (anything-move-selection-common
   (lambda () (goto-char (or (anything-get-next-header-pos) (point-min))))
   'source 'next))

(defun anything-goto-source (source-or-name)
  "Move the selection to the source (SOURCE-OR-NAME)."
  (anything-move-selection-common
   (lambda ()
     (goto-char (point-min))
     (let ((name (if (stringp source-or-name) source-or-name
                   (assoc-default 'name source-or-name))))
       (condition-case err
           (while (not (string= name (anything-current-line-contents)))
             (goto-char (anything-get-next-header-pos)))
         (error (message "")))))
   'source 'next))

(defun anything-mark-current-line ()
  "Move selection overlay to current line."
  (move-overlay
   anything-selection-overlay (point-at-bol)
   (if (anything-pos-multiline-p)
       (let ((header-pos (anything-get-next-header-pos))
             (separator-pos (anything-get-next-candidate-separator-pos)))
         (or (and (null header-pos) separator-pos)
             (and header-pos separator-pos (< separator-pos header-pos)
                  separator-pos)
             header-pos
             (point-max)))
     (1+ (point-at-eol))))
  (anything-follow-execute-persistent-action-maybe))

(defun anything-this-command-key ()
  (event-basic-type (elt (this-command-keys-vector) 0)))
;; (progn (read-key-sequence "Key: ") (p (anything-this-command-key)))

(defun anything-select-with-shortcut-internal (types get-key-func)
  (if (memq anything-enable-shortcuts types)
      (save-selected-window
        (select-window (anything-window))          
        (let* ((key (funcall get-key-func))
               (overlay (ignore-errors (nth (position key anything-shortcut-keys)
                                            anything-digit-overlays))))
          (if (not (and overlay (overlay-buffer overlay)))
              (when (numberp key)
                (select-window (minibuffer-window))
                (self-insert-command 1))
            (goto-char (overlay-start overlay))
            (anything-mark-current-line)
            (anything-exit-minibuffer))))
    (self-insert-command 1)))

(defun anything-select-with-prefix-shortcut ()
  "Invoke default action with prefix shortcut."
  (interactive)
  (anything-select-with-shortcut-internal
   '(prefix)
   (lambda () (read-event "Select shortcut key: "))))

(defun anything-select-with-digit-shortcut ()
  "Invoke default action with digit/alphabet shortcut."
  (interactive)
  (anything-select-with-shortcut-internal
   '(alphabet t) 'anything-this-command-key))

;; (setq anything-enable-shortcuts 'prefix)
;; (define-key anything-map "@" 'anything-select-with-prefix-shortcut)
;; (define-key anything-map (kbd "<f18>") 'anything-select-with-prefix-shortcut)

(defun anything-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (interactive)
  (declare (special anything-iswitchb-candidate-selected))
  (setq anything-iswitchb-candidate-selected (anything-get-selection))
  (exit-minibuffer))


(defun anything-get-next-header-pos ()
  "Return the position of the next header from point."
  (next-single-property-change (point) 'anything-header))


(defun anything-get-previous-header-pos ()
  "Return the position of the previous header from point"
  (previous-single-property-change (point) 'anything-header))


(defun anything-pos-multiline-p ()
  "Return non-nil if the current position is in the multiline source region."
  (get-text-property (point) 'anything-multiline))


(defun anything-get-next-candidate-separator-pos ()
  "Return the position of the next candidate separator from point."
  (next-single-property-change (point) 'anything-candidate-separator))


(defun anything-get-previous-candidate-separator-pos ()
  "Return the position of the previous candidate separator from point."
  (previous-single-property-change (point) 'anything-candidate-separator))


(defun anything-pos-header-line-p ()
  "Return t if the current line is a header line."
  (or (get-text-property (point-at-bol) 'anything-header)
      (get-text-property (point-at-bol) 'anything-header-separator)))

(defun anything-pos-candidate-separator-p ()
  "Return t if the current line is a candidate separator."
  (get-text-property (point-at-bol) 'anything-candidate-separator))

;; (@* "Core: help")
(defun anything-help-internal (bufname insert-content-fn)
  "Show long message during `anything' session."
  (save-window-excursion
    (select-window (anything-window))
    (delete-other-windows)
    (switch-to-buffer (get-buffer-create bufname))
    (erase-buffer)
    (funcall insert-content-fn)
    (setq mode-line-format "%b (SPC,C-v:NextPage  b,M-v:PrevPage  other:Exit)")
    (setq cursor-type nil)
    (goto-char 1)
    (anything-help-event-loop)))

(defun anything-help-event-loop ()
  (ignore-errors
    (loop for event = (read-event) do
          (case event
            ((?\C-v ? )  (scroll-up))
            ((?\M-v ?b) (scroll-down))
            (t (return))))))

(defun anything-help ()
  "Help of `anything'."
  (interactive)
  (anything-help-internal
   " *Anything Help*"
   (lambda ()
     (insert (substitute-command-keys
              (anything-interpret-value anything-help-message)))
     (org-mode))))

(defun anything-debug-output ()
  "Show all anything-related variables at this time."
  (interactive)
  (anything-help-internal " *Anything Debug*" 'anything-debug-output-function))

(defun anything-debug-output-function (&optional vars)
  (message "Calculating all anything-related values...")
  (insert "If you debug some variables or forms, set `anything-debug-forms'
to a list of forms.\n\n")
  (dolist (v (or vars
                 anything-debug-forms
                 (apropos-internal "^anything-" 'boundp)))
    (insert "** "
            (pp-to-string v) "\n"
            (pp-to-string (eval v)) "\n"))
  (message "Calculating all anything-related values...Done"))

;; (@* "Core: misc")
(defun anything-kill-buffer-hook ()
  "Remove tick entry from `anything-tick-hash' when killing a buffer."
  (loop for key being the hash-keys in anything-tick-hash
        if (string-match (format "^%s/" (regexp-quote (buffer-name))) key)
        do (remhash key anything-tick-hash)))
(add-hook 'kill-buffer-hook 'anything-kill-buffer-hook)

(defun anything-maybe-fit-frame ()
  "Fit anything frame to its buffer, and put it at top right of display.

It is disabled by default because some flickering occurred in some environment.
To enable fitting, set both `anything-inhibit-fit-frame-flag' and
 `fit-frame-inhibit-fitting' to nil.
You can set user options `fit-frame-max-width-percent' and
`fit-frame-max-height-percent' to control max frame size."
  (declare (warn (unresolved 0)))
  (when (and (not anything-inhibit-fit-frame-flag)
             (anything-window)
             (require 'fit-frame nil t)
             (boundp 'fit-frame-inhibit-fitting-flag)
             (not fit-frame-inhibit-fitting-flag))
    (ignore-errors
      (with-anything-window
        (fit-frame nil nil nil t)
        (modify-frame-parameters
         (selected-frame)
         `((left . ,(- (x-display-pixel-width) (+ (frame-pixel-width) 7)))
           (top . 0))))))) ; The (top . 0) shouldn't be necessary (Emacs bug).

(defun anything-preselect (candidate-or-regexp)
  (with-anything-window
    (when candidate-or-regexp
      (goto-char (point-min))
      ;; go to first candidate of first source
      (forward-line 1)
      (let ((start (point)))
        (unless (or (re-search-forward
                     (concat "^" (regexp-quote candidate-or-regexp) "$")
                     nil t)
                    (progn (goto-char start)
                           (re-search-forward candidate-or-regexp nil t)))
          (goto-char start))))
    (anything-mark-current-line)))

(defun anything-delete-current-selection ()
  "Delete the currently selected item."
  (interactive)
  (with-anything-window
    (cond ((anything-pos-multiline-p)
           (anything-aif (anything-get-next-candidate-separator-pos)
               (delete-region (point-at-bol)
                              (1+ (progn (goto-char it) (point-at-eol))))
             ;; last candidate
             (goto-char (anything-get-previous-candidate-separator-pos))
             (delete-region (point-at-bol) (point-max)))
           (when (eobp)
             (goto-char (or (anything-get-previous-candidate-separator-pos)
                            (point-min)))
             (forward-line 1)))
          (t
           (delete-region (point-at-bol) (1+ (point-at-eol)))
           (when (eobp) (forward-line -1))))
    (anything-mark-current-line)))

(defun anything-edit-current-selection-internal (func)
  (with-anything-window
    (beginning-of-line)
    (let ((realvalue (get-text-property (point) 'anything-realvalue)))
      (funcall func)
      (beginning-of-line)
      (and realvalue
           (put-text-property (point) (point-at-eol)
                              'anything-realvalue realvalue))
      (anything-mark-current-line))))

(defmacro anything-edit-current-selection (&rest forms)
  "Evaluate FORMS at current selection in the anything buffer.
You can edit the line."
  `(anything-edit-current-selection-internal
    (lambda () ,@forms)))
(put 'anything-edit-current-selection 'lisp-indent-function 0)

(defun anything-set-pattern (pattern &optional noupdate)
  "Set minibuffer contents to PATTERN.
if optional NOUPDATE is non-nil, anything buffer is not changed."
  (with-selected-window (minibuffer-window)
    (delete-minibuffer-contents)
    (insert pattern))
  (when noupdate
    (setq anything-pattern pattern)
    (anything-hooks 'cleanup)
    (run-with-idle-timer 0 nil 'anything-hooks 'setup)))

(defun anything-delete-minibuffer-contents ()
  "Same as `delete-minibuffer-contents' but this is a command."
  (interactive)
  (anything-set-pattern ""))
(defalias 'anything-delete-minibuffer-content 'anything-delete-minibuffer-contents)

;; (@* "Built-in plug-in: type")
(defun anything-compile-source--type (source)
  (anything-aif (assoc-default 'type source)
      (append source (assoc-default it anything-type-attributes) nil)
    source))

;; `define-anything-type-attribute' is public API.

(defun anything-add-type-attribute (type definition)
  (anything-aif (assq type anything-type-attributes)
      (setq anything-type-attributes (delete it anything-type-attributes)))
  (push (cons type definition) anything-type-attributes))

(defvar anything-types nil)
(defun anything-document-type-attribute (type doc)
  (add-to-list 'anything-types type t)
  (put type 'anything-typeattrdoc
       (concat "- " (symbol-name type) "\n\n" doc "\n")))

(defadvice documentation-property (after anything-document-type-attribute activate)
  "Hack to display type attributes' documentation as `anything-type-attributes' docstring."
  (when (eq symbol 'anything-type-attributes)
    (setq ad-return-value
          (concat ad-return-value "\n\n++++ Types currently defined ++++\n"
                  (mapconcat (lambda (sym) (get sym 'anything-typeattrdoc))
                             anything-types "\n")))))

;; (@* "Built-in plug-in: dummy")
(defun anything-dummy-candidate (candidate source)
  ;; `source' is defined in filtered-candidate-transformer
  (list anything-pattern))  

(defun anything-compile-source--dummy (source)
  (if (assoc 'dummy source)
      (append source
              '((candidates "dummy")
                (accept-empty)
                (match identity)
                (filtered-candidate-transformer . anything-dummy-candidate)
                (disable-shortcuts)
                (volatile)))
    source))

;; (@* "Built-in plug-in: disable-shortcuts")
(defvar anything-orig-enable-shortcuts nil)
(defun anything-save-enable-shortcuts ()
  (anything-once
   (lambda () (setq anything-orig-enable-shortcuts anything-enable-shortcuts
                    anything-enable-shortcuts nil))))
(defun anything-compile-source--disable-shortcuts (source)
  (if (assoc 'disable-shortcuts source)
      (append `((init ,@(anything-mklist (assoc-default 'init source))
                      anything-save-enable-shortcuts)
                (resume ,@(anything-mklist (assoc-default 'resume source))
                        anything-save-enable-shortcuts)
                (cleanup ,@(anything-mklist (assoc-default 'cleanup source))
                         (lambda () (setq anything-enable-shortcuts
                                          anything-orig-enable-shortcuts))))
              source)
    source))

;; (@* "Built-in plug-in: candidates-in-buffer")
(defun anything-candidates-in-buffer ()
  "Get candidates from the candidates buffer according to `anything-pattern'.

BUFFER is `anything-candidate-buffer' by default.  Each
candidate must be placed in one line.  This function is meant to
be used in candidates-in-buffer or candidates attribute of an
anything source.  Especially fast for many (1000+) candidates.

eg.
 '((name . \"many files\")
   (init . (lambda () (with-current-buffer (anything-candidate-buffer 'local)
                        (insert-many-filenames))))
   (search re-search-forward)  ; optional
   (candidates-in-buffer)
   (type . file))

+===============================================================+
| The new way of making and narrowing candidates: Using buffers |
+===============================================================+

By default, `anything' makes candidates by evaluating the
candidates function, then narrows them by `string-match' for each
candidate.

But this way is very slow for many candidates. The new way is
storing all candidates in a buffer and narrowing them by
`re-search-forward'. Search function is customizable by search
attribute. The important point is that buffer processing is MUCH
FASTER than string list processing and is the Emacs way.

The init function writes all candidates to a newly-created
candidate buffer.  The candidates buffer is created or specified
by `anything-candidate-buffer'.  Candidates are stored in a line.

The candidates function narrows all candidates, IOW creates a
subset of candidates dynamically. It is the task of
`anything-candidates-in-buffer'.  As long as
`anything-candidate-buffer' is used,`(candidates-in-buffer)' is
sufficient in most cases.

Note that `(candidates-in-buffer)' is shortcut of three attributes:
  (candidates . anything-candidates-in-buffer)
  (volatile)
  (match identity)
And `(candidates-in-buffer . func)' is shortcut of three attributes:
  (candidates . func)
  (volatile)
  (match identity)
The expansion is performed in `anything-get-sources'.

The candidates-in-buffer attribute implies the volatile attribute.
The volatile attribute is needed because `anything-candidates-in-buffer'
creates candidates dynamically and need to be called everytime
`anything-pattern' changes.

Because `anything-candidates-in-buffer' plays the role of `match' attribute
function, specifying `(match identity)' makes the source slightly faster.

To customize `anything-candidates-in-buffer' behavior, use search,
get-line and search-from-end attributes. See also `anything-sources' docstring.
"
  (declare (special source))
  (anything-candidates-in-buffer-1 (anything-candidate-buffer)
                                   anything-pattern
                                   (or (assoc-default 'get-line source)
                                       #'buffer-substring-no-properties)
                                   ;; use external variable `source'.
                                   (or (assoc-default 'search source)
                                       (if (assoc 'search-from-end source)
                                           '(re-search-backward)
                                         '(re-search-forward)))
                                   (anything-candidate-number-limit source)
                                   (assoc 'search-from-end source)))

(defun anything-candidates-in-buffer-1 (buffer pattern get-line-fn search-fns limit search-from-end)
  ;; buffer == nil when candidates buffer does not exist.
  (when buffer
    (with-current-buffer buffer
      (let ((start-point (if search-from-end (point-max) (point-min)))
            (endp (if search-from-end #'bobp #'eobp)))
        (goto-char (1- start-point))
        (if (string= pattern "")
            (anything-initial-candidates-from-candidate-buffer
             endp get-line-fn limit search-from-end)
          (anything-search-from-candidate-buffer
           pattern get-line-fn search-fns limit search-from-end
           start-point endp))))))

(defun anything-point-is-moved (proc)
  "If point is moved after executing PROC, return t, otherwise nil."
  (/= (point) (progn (funcall proc) (point))))

(defun anything-search-from-candidate-buffer (pattern get-line-fn search-fns
                                                      limit search-from-end
                                                      start-point endp)
  (let (buffer-read-only
        matches exit newmatches)
    (anything-search-from-candidate-buffer-internal
     (lambda ()
       (clrhash anything-cib-hash)
       (dolist (searcher search-fns)
         (goto-char start-point)
         (setq newmatches nil)
         (loop with item-count = 0
               while (funcall searcher pattern nil t)
               for cand = (funcall get-line-fn (point-at-bol) (point-at-eol))
               do (anything-accumulate-candidates-internal
                   cand newmatches anything-cib-hash item-count limit)
               unless (anything-point-is-moved
                       (lambda ()
                         (if search-from-end
                             (goto-char (1- (point-at-bol)))
                           (forward-line 1))))
               return nil)
         (setq matches (append matches (nreverse newmatches)))
         (if exit (return)))
       (delq nil matches)))))

(defun anything-initial-candidates-from-candidate-buffer (endp get-line-fn limit search-from-end)
  (delq nil (loop with next-line-fn =
                  (if search-from-end
                      (lambda (x) (goto-char (max (1- (point-at-bol)) 1)))
                    #'forward-line)
                  until (funcall endp)
                  for i from 1 to limit
                  collect (funcall get-line-fn (point-at-bol) (point-at-eol))
                  do (funcall next-line-fn 1))))

(defun anything-search-from-candidate-buffer-internal (search-fn)
  (goto-char (point-min))
  (insert "\n")
  (goto-char (point-max))
  (insert "\n")
  (unwind-protect
      (funcall search-fn)
    (goto-char (point-min))
    (delete-char 1)
    (goto-char (1- (point-max)))
    (delete-char 1)
                
    (set-buffer-modified-p nil)))

(defun anything-candidate-buffer (&optional create-or-buffer)
  "Register and return a buffer containing candidates of current source.
`anything-candidate-buffer' searches buffer-local candidates buffer first,
then global candidates buffer.

Acceptable values of CREATE-OR-BUFFER:

- nil (omit)
  Only return the candidates buffer.
- a buffer
  Register a buffer as a candidates buffer.
- 'global
  Create a new global candidates buffer,
  named \" *anything candidates:SOURCE*\".
- other non-nil value
  Create a new local candidates buffer,
  named \" *anything candidates:SOURCE*ANYTHING-CURRENT-BUFFER\".
"
  (let* ((global-bname (format " *anything candidates:%s*" anything-source-name))
         (local-bname (format " *anything candidates:%s*%s"
                              anything-source-name
                              (buffer-name anything-current-buffer)))
         (register-func
          (lambda ()
            (setq anything-candidate-buffer-alist
                  (cons (cons anything-source-name create-or-buffer)
                        (delete (assoc anything-source-name
                                       anything-candidate-buffer-alist)
                                anything-candidate-buffer-alist)))))
         (kill-buffers-func
          (lambda ()
            (loop for b in (buffer-list)
                  if (string-match (format "^%s" (regexp-quote global-bname))
                                   (buffer-name b))
                  do (kill-buffer b))))
         (create-func
          (lambda ()
            (with-current-buffer
                (get-buffer-create (if (eq create-or-buffer 'global)
                                       global-bname
                                     local-bname))
              (buffer-disable-undo)
              (erase-buffer)
              (font-lock-mode -1))))
         (return-func
          (lambda ()
            (or (get-buffer local-bname)
                (get-buffer global-bname)
                (anything-aif (assoc-default anything-source-name
                                             anything-candidate-buffer-alist)
                    (and (buffer-live-p it) it))))))
    (when create-or-buffer
      (funcall register-func)
      (unless (bufferp create-or-buffer) 
        (and (eq create-or-buffer 'global) (funcall kill-buffers-func))
        (funcall create-func)))
    (funcall return-func)))

(defun anything-compile-source--candidates-in-buffer (source)
  (anything-aif (assoc 'candidates-in-buffer source)
      (append source
              `((candidates . ,(or (cdr it) 'anything-candidates-in-buffer))
                (volatile) (match identity)))
    source))

;; (@* "Utility: resplit anything window")
(defun anything-toggle-resplit-window ()
  "Toggle resplit anything window, vertically or horizontally."
  (interactive)
  (with-anything-window
    (let ((before-height (window-height)))
      (delete-window)
      (set-window-buffer
       (select-window (if (= (window-height) before-height)
                          (split-window-vertically)
                        (split-window-horizontally)))
       anything-buffer))))

;; (@* "Utility: select another action by key")
(defun anything-select-nth-action (n)
  "Select the nth action for the currently selected candidate."
  (setq anything-saved-selection (anything-get-selection))
  (unless anything-saved-selection
    (error "Nothing is selected."))
  (setq anything-saved-action (anything-get-nth-action n (anything-get-action)))
  (anything-exit-minibuffer))

(defun anything-get-nth-action (n action)
  (cond ((and (zerop n) (functionp action))
         action)
        ((listp action)
         (or (cdr (elt action n))
             (error "No such action")))
        ((and (functionp action) (< 0 n))
         (error "Sole action."))
        (t
         (error "Error in `anything-select-nth-action'."))))

(defun anything-select-2nd-action ()
  "Select the 2nd action for the currently selected candidate."
  (interactive)
  (anything-select-nth-action 1))

(defun anything-select-3rd-action ()
  "Select the 3rd action for the currently selected candidate."
  (interactive)
  (anything-select-nth-action 2))

(defun anything-select-4th-action ()
  "Select the 4th action for the currently selected candidate."
  (interactive)
  (anything-select-nth-action 3))

(defun anything-select-2nd-action-or-end-of-line ()
  "Select the 2nd action for the currently selected candidate if the point is at the end of minibuffer.
Otherwise goto the end of minibuffer."
  (interactive)
  (if (eolp)
      (anything-select-nth-action 1)
    (end-of-line)))

;; (@* "Utility: Persistent Action")
(defmacro with-anything-display-same-window (&rest body)
  "Make `pop-to-buffer' and `display-buffer' display in the same window."
  `(let ((display-buffer-function 'anything-persistent-action-display-buffer))
     ,@body))
(put 'with-anything-display-same-window 'lisp-indent-function 0)

(defun* anything-execute-persistent-action (&optional (attr 'persistent-action))
  "If a candidate is selected then perform the associated action without quitting anything."
  (interactive)
  (anything-log "executing persistent-action")
  (save-selected-window
    (select-window (get-buffer-window (anything-buffer-get)))
    (select-window (setq minibuffer-scroll-window
                         (if (one-window-p t) (split-window)
                           (next-window (selected-window) 1))))
    (anything-log-eval (current-buffer))
    (let ((anything-in-persistent-action t))
      (with-anything-display-same-window
        (anything-execute-selection-action
         nil
         (or (assoc-default attr (anything-get-current-source))
             (anything-get-action))
         t)
        (anything-log-run-hook 'anything-after-persistent-action-hook)))))

(defun anything-persistent-action-display-buffer (buf &optional not-this-window)
  "Make `pop-to-buffer' and `display-buffer' display in the same window in persistent action.
If `anything-persistent-action-use-special-display' is non-nil and
BUF is to be displayed by `special-display-function', use it.
Otherwise ignores `special-display-buffer-names' and `special-display-regexps'."
  (let* ((name (buffer-name buf))
         display-buffer-function pop-up-windows
         (same-window-regexps
          (unless (and anything-persistent-action-use-special-display
                       (or (member name
                                   (mapcar (lambda (x) (or (car-safe x) x))
                                           special-display-buffer-names))
                           (remove-if-not
                            (lambda (x) (string-match (or (car-safe x) x) name))
                            special-display-regexps)))
            '("."))))
    (display-buffer buf not-this-window)))

;; scroll-other-window(-down)? for persistent-action
(defun anything-scroll-other-window-base (command)
  (save-selected-window
    (select-window
     (some-window
      (lambda (w) (not (string= anything-buffer (buffer-name (window-buffer w)))))
      'no-minibuffer 'current-frame))
    (funcall command anything-scroll-amount)))

(defun anything-scroll-other-window ()
  "Scroll other window (not *Anything* window) upward."
  (interactive)
  (anything-scroll-other-window-base 'scroll-up))
(defun anything-scroll-other-window-down ()
  "Scroll other window (not *Anything* window) downward."
  (interactive)
  (anything-scroll-other-window-base 'scroll-down))

;; (@* "Utility: Visible Mark")
(defface anything-visible-mark
  '((((min-colors 88) (background dark))
     (:background "green1" :foreground "black"))
    (((background dark)) (:background "green" :foreground "black"))
    (((min-colors 88)) (:background "green1"))
    (t (:background "green")))
  "Face for visible mark."
  :group 'anything)
(defvar anything-visible-mark-face 'anything-visible-mark)
(defvar anything-visible-mark-overlays nil)

(defun anything-clear-visible-mark ()
  (with-current-buffer (anything-buffer-get)
    (mapc 'delete-overlay anything-visible-mark-overlays)
    (set (make-local-variable 'anything-visible-mark-overlays) nil)))
(add-hook 'anything-after-initialize-hook 'anything-clear-visible-mark)

(defvar anything-c-marked-candidate-list nil
  "[OBSOLETE] DO NOT USE!!")
(defvar anything-marked-candidates nil
  "Marked candadates. List of (source . real) pair.")

(defun anything-this-visible-mark ()
  (loop for o in anything-visible-mark-overlays
        when (equal (point-at-bol) (overlay-start o))
        do   (return o)))

(defun anything-delete-visible-mark (overlay)
  (setq anything-c-marked-candidate-list
        (remove (anything-current-line-contents) anything-c-marked-candidate-list))
  (setq anything-marked-candidates
        (remove
         (cons (anything-get-current-source) (anything-get-selection))
         anything-marked-candidates))
  (delete-overlay overlay)
  (setq anything-visible-mark-overlays
        (delq overlay anything-visible-mark-overlays)))

(defun anything-make-visible-mark ()
  (let ((o (make-overlay (point-at-bol) (1+ (point-at-eol)))))
    (overlay-put o 'face anything-visible-mark-face)
    (overlay-put o 'source (assoc-default 'name (anything-get-current-source)))
    (overlay-put o 'string (buffer-substring (overlay-start o) (overlay-end o)))
    (add-to-list 'anything-visible-mark-overlays o))
  (push (anything-current-line-contents) anything-c-marked-candidate-list)
  (push (cons (anything-get-current-source) (anything-get-selection))
        anything-marked-candidates))

(defun anything-toggle-visible-mark ()
  "Toggle anything visible mark at point."
  (interactive)
  (with-anything-window
    (anything-aif (anything-this-visible-mark)
        (anything-delete-visible-mark it)
      (anything-make-visible-mark))
    (anything-next-line)))

(defun anything-display-all-visible-marks ()
  "Show all `anything' visible marks strings."
  (interactive)
  (with-anything-window
    (lexical-let ((overlays (reverse anything-visible-mark-overlays)))
      (anything-run-after-quit
       (lambda ()
         (with-output-to-temp-buffer "*anything visible marks*"
           (dolist (o overlays) (princ (overlay-get o 'string)))))))))

(defun anything-marked-candidates ()
  "Marked candidates (real value) of current source if any,
otherwise 1-element list of current selection.

It is analogous to `dired-get-marked-files'."
  (with-current-buffer (anything-buffer-get)
    (let ((cands
           (if anything-marked-candidates
               (loop with current-src = (anything-get-current-source)
                     for (source . real) in (reverse anything-marked-candidates)
                     when (equal current-src source)
                     collect (anything-coerce-selection real source))
             (list (anything-get-selection)))))
      (anything-log-eval cands)
      cands)))

(defun anything-reset-marked-candidates ()
  (with-current-buffer (anything-buffer-get)
    (set (make-local-variable 'anything-c-marked-candidate-list) nil)
    (set (make-local-variable 'anything-marked-candidates) nil)))

(add-hook 'anything-after-initialize-hook 'anything-reset-marked-candidates)
;; (add-hook 'anything-after-action-hook 'anything-reset-marked-candidates)

(defun anything-current-source-name= (name)
  (save-excursion
    (goto-char (anything-get-previous-header-pos))
    (equal name (anything-current-line-contents))))

(defun anything-revive-visible-mark ()
  (with-current-buffer anything-buffer
    (dolist (o anything-visible-mark-overlays)
      (goto-char (point-min))
      (while (and (search-forward (overlay-get o 'string) nil t)
                  (anything-current-source-name= (overlay-get o 'source)))
        ;; Now the next line of visible mark
        (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0)))))))
(add-hook 'anything-update-hook 'anything-revive-visible-mark)

(defun anything-next-point-in-list (curpos points &optional prev)
  (cond
   ;; rule out special cases
   ((null points)                        curpos)
   ((and prev (< curpos (car points)))   curpos)
   ((< (car (last points)) curpos)
    (if prev (car (last points)) curpos))
   (t
    (nth (if prev
             (loop for pt in points
                   for i from 0
                   if (<= curpos pt)
                   do (return (1- i)))
           (loop for pt in points
                 for i from 0
                 if (< curpos pt)
                 do (return i)))
         points))))

(defun anything-next-visible-mark (&optional prev)
  "Move next anything visible mark."
  (interactive)
  (with-anything-window
    (goto-char (anything-next-point-in-list
                (point)
                (sort (mapcar 'overlay-start anything-visible-mark-overlays) '<)
                prev))
    (anything-mark-current-line)))

(defun anything-prev-visible-mark ()
  "Move previous anything visible mark."
  (interactive)
  (anything-next-visible-mark t))

;; (@* "Utility: `find-file' integration")
(defun anything-quit-and-find-file ()
  "Drop into `find-file' from `anything' like `iswitchb-find-file'.
If current selection is a buffer or a file, `find-file' from its directory."
  (interactive)
  (anything-run-after-quit
   (lambda (f)
     (if (file-exists-p f)
         (let ((default-directory (file-name-directory f)))
           (call-interactively 'find-file))
       (call-interactively 'find-file)))
   (anything-aif (get-buffer (anything-get-selection))
       (buffer-file-name it)
     (expand-file-name (anything-get-selection)))))

;; (@* "Utility: Selection Paste")
(defun anything-yank-selection ()
  "Set minibuffer contents to current selection."
  (interactive)
  (anything-set-pattern (anything-get-selection nil t)))

(defun anything-kill-selection-and-quit ()
  "Store current selection to kill ring.
You can paste it by typing C-y."
  (interactive)
  (anything-run-after-quit
   (lambda (sel)
     (kill-new sel)
     (message "Killed: %s" sel))
   (anything-get-selection nil t)))


;; (@* "Utility: Automatical execution of persistent-action")
(add-to-list 'minor-mode-alist '(anything-follow-mode " AFollow"))
(defun anything-follow-mode ()
  "If this mode is on, persistent action is executed everytime the cursor is moved."
  (interactive)
  (with-current-buffer anything-buffer
    (setq anything-follow-mode (not anything-follow-mode))
    (message "anything-follow-mode is %s"
             (if anything-follow-mode "enabled" "disabled"))))

(defun anything-follow-execute-persistent-action-maybe ()
  "Execute persistent action after `anything-input-idle-delay' secs when `anything-follow-mode' is enabled."
  (and (not (get-buffer-window anything-action-buffer 'visible))
       (buffer-local-value 'anything-follow-mode
                           (get-buffer-create anything-buffer))
       (sit-for anything-input-idle-delay)
       (anything-window)
       (anything-get-selection)
       (save-excursion
         (anything-execute-persistent-action))))

;; (@* "Utility: Migrate `anything-sources' to my-anything command")
(defun anything-migrate-sources ()
  "Help to migrate to new `anything' way."
  (interactive)
  (with-current-buffer (get-buffer-create "*anything migrate*")
    (erase-buffer)
    (insert (format "\
Setting `anything-sources' directly is not good because
`anything' is not for one command.  For now, interactive use of
`anything' (M-x anything) is only for demonstration purpose.
So you should define commands calling `anything'.
I help you to migrate to the new way.

The code below is automatically generated from current
`anything-sources' value. You can use the `my-anything' command
now!

Copy and paste it to your .emacs. Then substitute `my-anything'
for `anything' bindings in all `define-key', `local-set-key' and
`global-set-key' calls.

\(defun my-anything ()
  \"Anything command for you.

It is automatically generated by `anything-migrate-sources'.\"
  (interactive)
  (anything-other-buffer
    '%S
    \"*my-anything*\"))
" anything-sources))
    (eval-last-sexp nil)
    (substitute-key-definition 'anything 'my-anything global-map)
    (pop-to-buffer (current-buffer))))

;; (@* "Utility: Incremental search within results (unmaintained)")

(defvar anything-isearch-original-global-map nil
  "Original global map before Anything isearch is started.")

(defvar anything-isearch-original-message-timeout nil
  "Original message timeout before Anything isearch is started.")

(defvar anything-isearch-pattern nil
  "The current isearch pattern.")

(defvar anything-isearch-message-suffix ""
  "Message suffix indicating the current state of the search.")

(defvar anything-isearch-original-point nil
  "Original position of point before isearch is started.")

(defvar anything-isearch-original-window nil
  "Original selected window before isearch is started.")

(defvar anything-isearch-original-cursor-in-non-selected-windows nil
  "Original value of cursor-in-non-selected-windows before isearch is started.")

(defvar anything-isearch-original-deferred-action-list nil
  "Original value of deferred-action-list before isearch is started.")

(defvar anything-isearch-match-positions nil
  "Stack of positions of matches or non-matches.

It's a list of plists with two properties: `event', the last user
 event, `start', the start position of the current match, and
 `pos', the position of point after that event.

The value of `event' can be the following symbols: `char' if a
character was typed, `error' if a non-matching character was
typed, `search' if a forward search had to be done after a
character, and `search-again' if a search was done for the next
occurrence of the current pattern.")

(defvar anything-isearch-match-start nil
  "Start position of the current match.")


(defun anything-isearch ()
  "Start incremental search within results. (UNMAINTAINED)"
  (interactive)
  (if (anything-empty-buffer-p (anything-buffer-get))
      (message "There are no results.")

    (setq anything-isearch-original-message-timeout minibuffer-message-timeout)
    (setq minibuffer-message-timeout nil)

    (setq anything-isearch-original-global-map global-map)

    (condition-case nil
        (progn
          (setq anything-isearch-original-window (selected-window))
          (select-window (anything-window))
          (setq cursor-type t)

          (setq anything-isearch-original-deferred-action-list
                (default-value 'deferred-action-list))
          (setq-default deferred-action-list nil)
          (add-hook 'deferred-action-list 'anything-isearch-post-command)

          (use-global-map anything-isearch-map)
          (setq overriding-terminal-local-map anything-isearch-map)

          (setq anything-isearch-pattern "")

          (setq anything-isearch-original-cursor-in-non-selected-windows
                cursor-in-non-selected-windows)
          (setq cursor-in-non-selected-windows nil) 

          (setq anything-isearch-original-point (point-marker))
          (goto-char (point-min))
          (forward-line)
          (anything-mark-current-line)

          (setq anything-isearch-match-positions nil)
          (setq anything-isearch-match-start (point-marker))

          (if anything-isearch-overlay
              ;; make sure the overlay belongs to the anything buffer
              (move-overlay anything-isearch-overlay (point-min) (point-min)
                            (get-buffer (anything-buffer-get)))

            (setq anything-isearch-overlay (make-overlay (point-min) (point-min)))
            (overlay-put anything-isearch-overlay 'face anything-isearch-match-face))

          (setq anything-isearch-message-suffix
                (substitute-command-keys "cancel with \\[anything-isearch-cancel]")))

      (error (anything-isearch-cleanup)))))


(defun anything-isearch-post-command ()
  "Print the current pattern after every command."
  (anything-isearch-message)
  (when (anything-window)
    (with-anything-window
      (move-overlay anything-isearch-overlay anything-isearch-match-start (point)
                    (get-buffer (anything-buffer-get))))))


(defun anything-isearch-printing-char ()
  "Add printing char to the pattern."
  (interactive)
  (let ((char (char-to-string last-command-event)))
    (setq anything-isearch-pattern (concat anything-isearch-pattern char))

    (with-anything-window
      (if (looking-at char)
          (progn
            (push (list 'event 'char
                        'start anything-isearch-match-start
                        'pos (point-marker))
                  anything-isearch-match-positions)
            (forward-char))

        (let ((start (point)))
          (while (and (re-search-forward anything-isearch-pattern nil t)
                      (anything-pos-header-line-p)))
          (if (or (anything-pos-header-line-p)
                  (eq start (point)))
              (progn
                (goto-char start)
                (push (list 'event 'error
                            'start anything-isearch-match-start
                            'pos (point-marker))
                      anything-isearch-match-positions))

            (push (list 'event 'search
                        'start anything-isearch-match-start
                        'pos (copy-marker start))
                  anything-isearch-match-positions)
            (setq anything-isearch-match-start
                  (copy-marker (match-beginning 0))))))
  
      (anything-mark-current-line))))


(defun anything-isearch-again ()
  "Search again for the current pattern"
  (interactive)
  (if (equal anything-isearch-pattern "")
      (setq anything-isearch-message-suffix "no pattern yet")

    (with-anything-window
      (let ((start (point)))
        (while (and (re-search-forward anything-isearch-pattern nil t)
                    (anything-pos-header-line-p)))
        (if (or (anything-pos-header-line-p)
                (eq start (point)))
            (progn
              (goto-char start)
              (unless (eq 'error
                          (plist-get (car anything-isearch-match-positions)
                                     'event))
                (setq anything-isearch-message-suffix "no more matches")))

          (push (list 'event 'search-again
                      'start anything-isearch-match-start
                      'pos (copy-marker start))
                anything-isearch-match-positions)
          (setq anything-isearch-match-start (copy-marker (match-beginning 0)))

          (anything-mark-current-line))))))


(defun anything-isearch-delete ()
  "Undo last event."
  (interactive)
  (unless (equal anything-isearch-pattern "")
    (let ((last (pop anything-isearch-match-positions)))
      (unless (eq 'search-again (plist-get last 'event))
        (setq anything-isearch-pattern
              (substring anything-isearch-pattern 0 -1)))

      (with-anything-window      
        (goto-char (plist-get last 'pos))
        (setq anything-isearch-match-start (plist-get last 'start))
        (anything-mark-current-line)))))


(defun anything-isearch-default-action ()
  "Execute the default action for the selected candidate."
  (interactive)
  (anything-isearch-cleanup)
  (with-current-buffer (anything-buffer-get) (anything-exit-minibuffer)))


(defun anything-isearch-select-action ()
  "Choose an action for the selected candidate."
  (interactive)
  (anything-isearch-cleanup)
  (with-anything-window
    (anything-select-action)))


(defun anything-isearch-cancel ()
  "Cancel Anything isearch."
  (interactive)
  (anything-isearch-cleanup)
  (when (anything-window)
    (with-anything-window
      (goto-char anything-isearch-original-point)
      (anything-mark-current-line))))


(defun anything-isearch-cleanup ()
  "Clean up the mess."
  (setq minibuffer-message-timeout anything-isearch-original-message-timeout)
  (with-current-buffer (anything-buffer-get)
    (setq overriding-terminal-local-map nil)
    (setq cursor-type nil)
    (setq cursor-in-non-selected-windows
          anything-isearch-original-cursor-in-non-selected-windows))
  (when anything-isearch-original-window
    (select-window anything-isearch-original-window))

  (use-global-map anything-isearch-original-global-map)
  (setq-default deferred-action-list
                anything-isearch-original-deferred-action-list)
  (when (overlayp anything-isearch-overlay) 
    (delete-overlay anything-isearch-overlay)))


(defun anything-isearch-message ()
  "Print prompt."
  (if (and (equal anything-isearch-message-suffix "")
           (eq (plist-get (car anything-isearch-match-positions) 'event)
               'error))
      (setq anything-isearch-message-suffix "failing"))

  (unless (equal anything-isearch-message-suffix "")
    (setq anything-isearch-message-suffix 
          (concat " [" anything-isearch-message-suffix "]")))

  (message (concat "Search within results: "
                   anything-isearch-pattern
                   anything-isearch-message-suffix))

  (setq anything-isearch-message-suffix ""))


;; (@* "Utility: Iswitchb integration (unmaintained)")

(defvar anything-iswitchb-candidate-selected nil
  "Indicates whether an anything candidate is selected from iswitchb.")

(defvar anything-iswitchb-frame-configuration nil
  "Saved frame configuration, before anything buffer was displayed.")

(defvar anything-iswitchb-saved-keys nil
  "The original in iswitchb before binding anything keys.")


(defun anything-iswitchb-setup ()
  "Integrate anything completion into iswitchb (UNMAINTAINED).

If the user is idle for `anything-iswitchb-idle-delay' seconds
after typing something into iswitchb then anything candidates are
shown for the current iswitchb input.

ESC cancels anything completion and returns to normal iswitchb.

Some key bindings in `anything-map' are modified.
See also `anything-iswitchb-setup-keys'."
  (interactive)

  (require 'iswitchb)

  ;; disable timid completion during iswitchb
  (put 'iswitchb-buffer 'timid-completion 'disabled)
  (add-hook 'minibuffer-setup-hook  'anything-iswitchb-minibuffer-setup)

  (defadvice iswitchb-visit-buffer
    (around anything-iswitchb-visit-buffer activate)
    (if anything-iswitchb-candidate-selected
        (anything-execute-selection-action)
      ad-do-it))

  (defadvice iswitchb-possible-new-buffer
    (around anything-iswitchb-possible-new-buffer activate)
    (if anything-iswitchb-candidate-selected
        (anything-execute-selection-action)
      ad-do-it))
  (anything-iswitchb-setup-keys)
  (message "Iswitchb integration is activated."))

(defun anything-iswitchb-setup-keys ()
  "Modify `anything-map' for anything-iswitchb users.

C-p is used instead of M-p, because anything uses ESC
 (currently hardcoded) for `anything-iswitchb-cancel-anything' and
Emacs handles ESC and Meta as synonyms, so ESC overrides
other commands with Meta prefix.

Note that iswitchb uses M-p and M-n by default for history
navigation, so you should bind C-p and C-n in
`iswitchb-mode-map' if you use the history keys and don't want
to use different keys for iswitchb while anything is not yet
kicked in. These keys are not bound automatically by anything
in `iswitchb-mode-map' because they (C-n at least) already have
a standard iswitchb binding which you might be accustomed to.

Binding M-s is used instead of C-s, because C-s has a binding in
iswitchb.  You can rebind it AFTER `anything-iswitchb-setup'.

Unbind C-r to prevent problems during anything-isearch."
  (define-key anything-map (kbd "C-s") nil)
  (define-key anything-map (kbd "M-p") nil)
  (define-key anything-map (kbd "M-n") nil)
  (define-key anything-map (kbd "M-v") nil)
  (define-key anything-map (kbd "C-v") nil)
  (define-key anything-map (kbd "C-p") 'anything-previous-history-element)
  (define-key anything-map (kbd "C-n") 'anything-next-history-element)
  (define-key anything-map (kbd "M-s") nil)
  (define-key anything-map (kbd "M-s") 'anything-isearch)
  (define-key anything-map (kbd "C-r") nil))

(defun anything-iswitchb-minibuffer-setup ()
  (when (eq this-command 'iswitchb-buffer)
    (add-hook 'minibuffer-exit-hook  'anything-iswitchb-minibuffer-exit)

    (setq anything-iswitchb-frame-configuration nil)
    (setq anything-iswitchb-candidate-selected nil)
    (add-hook 'anything-update-hook 'anything-iswitchb-handle-update)

    (anything-initialize)
    
    (add-hook 'deferred-action-list 'anything-iswitchb-check-input)))


(defun anything-iswitchb-minibuffer-exit ()
  (remove-hook 'minibuffer-exit-hook  'anything-iswitchb-minibuffer-exit)
  (remove-hook 'deferred-action-list 'anything-iswitchb-check-input)
  (remove-hook 'anything-update-hook 'anything-iswitchb-handle-update)

  (anything-cleanup)

  (when anything-iswitchb-frame-configuration
    (anything-set-frame/window-configuration
     anything-iswitchb-frame-configuration)
    (setq anything-iswitchb-frame-configuration nil)))


(defun anything-iswitchb-check-input ()
  "Extract iswitchb input and check if it needs to be handled."
  (declare (special iswitchb-text))
  (if (or anything-iswitchb-frame-configuration
          (sit-for anything-iswitchb-idle-delay))
      (anything-check-new-input iswitchb-text)))


(defun anything-iswitchb-handle-update ()
  "Pop up the anything buffer if it's not empty and it's not
shown yet and bind anything commands in iswitchb."
  (unless (or (anything-empty-buffer-p anything-buffer)
              anything-iswitchb-frame-configuration)
    (setq anything-iswitchb-frame-configuration
          (anything-current-frame/window-configuration))

    (save-selected-window 
      (if (not anything-samewindow)
          (pop-to-buffer anything-buffer)

        (select-window (get-lru-window))
        (switch-to-buffer anything-buffer)))

    (with-current-buffer (window-buffer (active-minibuffer-window))
      (let* ((anything-prefix "anything-")
             (prefix-length (length anything-prefix))
             (commands 
              (delete-dups
               (remove-if 'null
                          (mapcar 
                           (lambda (binding)
                             (let ((command (cdr binding)))
                               (when (and (symbolp command)
                                          (eq (compare-strings 
                                               anything-prefix 
                                               0 prefix-length
                                               (symbol-name command)
                                               0 prefix-length)
                                              t))
                                 command)))
                           (cdr anything-map)))))
             (bindings (mapcar (lambda (command)
                                 (cons command 
                                       (where-is-internal command anything-map)))
                               commands)))

        (push (list 'anything-iswitchb-cancel-anything (kbd "<ESC>"))
              bindings)

        (setq anything-iswitchb-saved-keys nil)

      (let* ((iswitchb-prefix "iswitchb-")
             (prefix-length (length iswitchb-prefix)))
        (dolist (binding bindings)
          (dolist (key (cdr binding))
            (let ((old-command (lookup-key (current-local-map) key)))
              (unless (and anything-iswitchb-dont-touch-iswithcb-keys
                           (symbolp old-command)
                           (eq (compare-strings iswitchb-prefix 
                                                0 prefix-length
                                                (symbol-name old-command)
                                                0 prefix-length)
                               t))
                (push (cons key old-command)
                      anything-iswitchb-saved-keys)
                (define-key (current-local-map) key (car binding)))))))))))


(defun anything-iswitchb-cancel-anything ()
  "Cancel anything completion and return to standard iswitchb."
  (interactive)
  (save-excursion
    (dolist (binding anything-iswitchb-saved-keys)
      (define-key (current-local-map) (car binding) (cdr binding)))
    (anything-iswitchb-minibuffer-exit)))

;; (@* "Compatibility")

;; Copied assoc-default from XEmacs version 21.5.12
(unless (fboundp 'assoc-default)
  (defun assoc-default (key alist &optional test default)
    "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element (or the element's car,
if it is a cons) is compared with KEY by evaluating (TEST (car elt) KEY).
If that is non-nil, the element matches;
then `assoc-default' returns the element's cdr, if it is a cons,
or DEFAULT if the element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
    (let (found (tail alist) value)
      (while (and tail (not found))
        (let ((elt (car tail)))
          (when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
            (setq found t value (if (consp elt) (cdr elt) default))))
        (setq tail (cdr tail)))
      value)))

;; Function not available in XEmacs, 
(unless (fboundp 'minibuffer-contents)
  (defun minibuffer-contents ()
    "Return the user input in a minbuffer as a string.
The current buffer must be a minibuffer."
    (field-string (point-max)))

  (defun delete-minibuffer-contents  ()
    "Delete all user input in a minibuffer.
The current buffer must be a minibuffer."
    (delete-field (point-max))))

;; Function not available in older Emacs (<= 22.1).
(unless (fboundp 'buffer-chars-modified-tick)
  (defun buffer-chars-modified-tick (&optional buffer)
    "Return BUFFER's character-change tick counter.
Each buffer has a character-change tick counter, which is set to the
value of the buffer's tick counter (see `buffer-modified-tick'), each
time text in that buffer is inserted or deleted.  By comparing the
values returned by two individual calls of `buffer-chars-modified-tick',
you can tell whether a character change occurred in that buffer in
between these calls.  No argument or nil as argument means use current
buffer as BUFFER."
    (with-current-buffer (or buffer (current-buffer))
      (if (listp buffer-undo-list)
          (length buffer-undo-list)
        (buffer-modified-tick)))))

;; (@* "CUA workaround")
(defadvice cua-delete-region (around anything-avoid-cua activate)
  (ignore-errors ad-do-it))
(defadvice copy-region-as-kill (around anything-avoid-cua activate)
  (if cua-mode
      (ignore-errors ad-do-it)
    ad-do-it))


;;(@* "Attribute Documentation")
(defun anything-describe-anything-attribute (anything-attribute)
  "Display the full documentation of ANYTHING-ATTRIBUTE (a symbol)."
  (interactive (list (intern
                      (completing-read
                       "Describe anything attribute: "
                       (mapcar 'symbol-name anything-additional-attributes)))))
  (with-output-to-temp-buffer "*Help*"
    (princ (get anything-attribute 'anything-attrdoc))))

(anything-document-attribute 'name "mandatory"
  "  The name of the source. It is also the heading which appears
  above the list of matches from the source. Must be unique. ")
(anything-document-attribute 'header-name "optional"
  "  A function returning the display string of the header. Its
  argument is the name of the source. This attribute is useful to
  add an additional information with the source name. ")
(anything-document-attribute 'candidates "mandatory if candidates-in-buffer attribute is not provided"
  "  Specifies how to retrieve candidates from the source. It can
  either be a variable name, a function called with no parameters
  or the actual list of candidates.

  The list must be a list whose members are strings, symbols
  or (DISPLAY . REAL) pairs.

  In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
  in the Anything buffer, but the REAL one is used as action
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
  Anything will take care of managing the process (receiving the
  output from it, killing it if necessary, etc.). The process
  should return candidates matching the current pattern (see
  variable `anything-pattern'.)

  Note that currently results from asynchronous sources appear
  last in the anything buffer regardless of their position in
  `anything-sources'. ")
(anything-document-attribute 'action "mandatory if type attribute is not provided"
  "  It is a list of (DISPLAY . FUNCTION) pairs or FUNCTION.
  FUNCTION is called with one parameter: the selected candidate.

  An action other than the default can be chosen from this list
  of actions for the currently selected candidate (by default
  with TAB). The DISPLAY string is shown in the completions
  buffer and the FUNCTION is invoked when an action is
  selected. The first action of the list is the default. ")
(anything-document-attribute 'coerce "optional"
  "  It's a function called with one argument: the selected candidate.
  
  This function is intended for type convertion.
  In normal case, the selected candidate (string) is passed to action function.
  If coerce function is specified, it is called just before action function.

  Example: converting string to symbol
    (coerce . intern)
")
(anything-document-attribute 'type "optional if action attribute is provided"
  "  Indicates the type of the items the source returns. 

  Merge attributes not specified in the source itself from
  `anything-type-attributes'.

  This attribute is implemented by plug-in. ")
(anything-document-attribute 'init "optional"
  "  Function called with no parameters when anything is started. It
  is useful for collecting current state information which can be
  used to create the list of candidates later.

  For example, if a source needs to work with the current
  directory then it can store its value here, because later
  anything does its job in the minibuffer and in the
  `anything-buffer' and the current directory can be different
  there. ")
(anything-document-attribute 'delayed-init "optional"
  "  Function called with no parameters before candidate function is
  called.  It is similar with `init' attribute, but its
  evaluation is deferred. It is useful to combine with ")
(anything-document-attribute 'match "optional"
  "  List of functions called with one parameter: a candidate. The
  function should return non-nil if the candidate matches the
  current pattern (see variable `anything-pattern').

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
  themselves. ")
(anything-document-attribute 'candidate-transformer "optional"
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
  . REAL) format. ")
(anything-document-attribute 'filtered-candidate-transformer "optional"
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
  imposed by `anything-candidate-number-limit'.

  Note that `candidates' and `candidate-transformer' is run
  already, so the given transformer function should also be able
  to handle candidates with (DISPLAY . REAL) format.

  This option has no effect for asynchronous sources. (Not yet,
  at least. ")
(anything-document-attribute 'action-transformer "optional"
  "  It's a function or a list of functions called with two
  arguments when the action list from the source is
  assembled. The first argument is the list of actions, the
  second is the current selection.  If it is a list of functions,
  it calls each function sequentially.

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate. ")
(anything-document-attribute 'pattern-transformer "optional"
  "  It's a function or a list of functions called with one argument
  before computing matches. Its argument is `anything-pattern'.
  Functions should return transformed `anything-pattern'.

  It is useful to change interpretation of `anything-pattern'. ")
(anything-document-attribute 'delayed "optional"
  "  Candidates from the source are shown only if the user stops
  typing and is idle for `anything-idle-delay' seconds. ")
(anything-document-attribute 'volatile "optional"
  "  Indicates the source assembles the candidate list dynamically,
  so it shouldn't be cached within a single Anything
  invocation. It is only applicable to synchronous sources,
  because asynchronous sources are not cached. ")
(anything-document-attribute 'requires-pattern "optional"
  "  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates. ")
(anything-document-attribute 'persistent-action "optional"
  "  Function called with one parameter; the selected candidate.

  An action performed by `anything-execute-persistent-action'.
  If none, use the default action. ")
(anything-document-attribute 'candidates-in-buffer "optional"
  "  Shortcut attribute for making and narrowing candidates using
  buffers.  This newly-introduced attribute prevents us from
  forgetting to add volatile and match attributes.

  See docstring of `anything-candidates-in-buffer'.

  (candidates-in-buffer) is equivalent of three attributes:
    (candidates . anything-candidates-in-buffer)
    (volatile)
    (match identity)

  (candidates-in-buffer . candidates-function) is equivalent of:
    (candidates . candidates-function)
    (volatile)
    (match identity)

  This attribute is implemented by plug-in. ")
(anything-document-attribute 'search "optional"
  "  List of functions like `re-search-forward' or `search-forward'.
  Buffer search function used by `anything-candidates-in-buffer'.
  By default, `anything-candidates-in-buffer' uses `re-search-forward'.
  This attribute is meant to be used with
  (candidates . anything-candidates-in-buffer) or
  (candidates-in-buffer) in short. ")
(anything-document-attribute 'search-from-end "optional"
  "  Make `anything-candidates-in-buffer' search from the end of buffer.
  If this attribute is specified, `anything-candidates-in-buffer' uses
  `re-search-backward' instead. ")
(anything-document-attribute 'get-line "optional"
  "  A function like `buffer-substring-no-properties' or `buffer-substring'.
  This function converts point of line-beginning and point of line-end,
  which represents a candidate computed by `anything-candidates-in-buffer'.
  By default, `anything-candidates-in-buffer' uses
  `buffer-substring-no-properties'. ")
(anything-document-attribute 'display-to-real "optional"
  "  Function called with one parameter; the selected candidate.

  The function transforms the selected candidate, and the result
  is passed to the action function.  The display-to-real
  attribute provides another way to pass other string than one
  shown in Anything buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if REAL
  can be generated from DISPLAY, display-to-real is more
  convenient and faster. ")
(anything-document-attribute 'real-to-display "optional"
  "  Function called with one parameter; the selected candidate.

  The inverse of display-to-real attribute.

  The function transforms the selected candidate, which is passed
  to the action function, for display.  The real-to-display
  attribute provides the other way to pass other string than one
  shown in Anything buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if
  DISPLAY can be generated from REAL, real-to-display is more
  convenient.

  Note that DISPLAY parts returned from candidates /
  candidate-transformer are IGNORED as the name `display-to-real'
  says. ")
(anything-document-attribute 'cleanup "optional"
  "  Function called with no parameters when *anything* buffer is closed. It
  is useful for killing unneeded candidates buffer.

  Note that the function is executed BEFORE performing action. ")
(anything-document-attribute 'candidate-number-limit "optional"
  "  Override `anything-candidate-number-limit' only for this source. ")
(anything-document-attribute 'accept-empty "optional"
  "  Pass empty string \"\" to action function. ")
(anything-document-attribute 'disable-shortcuts "optional"
  "  Disable `anything-enable-shortcuts' in current `anything' session.

  This attribute is implemented by plug-in. ")
(anything-document-attribute 'dummy "optional"
  "  Set `anything-pattern' to candidate. If this attribute is
  specified, The candidates attribute is ignored.

  This attribute is implemented by plug-in.
  This plug-in implies disable-shortcuts plug-in. ")
(anything-document-attribute 'multiline "optional"
  "  Enable to selection multiline candidates. ")
(anything-document-attribute 'update "optional"
  "  Function called with no parameters when \\<anything-map>\\[anything-force-update] is pressed. ")
(anything-document-attribute 'mode-line "optional"
  "  source local `anything-mode-line-string'. (included in `mode-line-format')
  It accepts also variable/function name. ")
(anything-document-attribute 'header-line "optional"
  "  source local `header-line-format'.
  It accepts also variable/function name. ")
(anything-document-attribute 'resume "optional" "  Function called with no parameters when `anything-resume' is started.")

;; (@* "Bug Report")
(defvar anything-maintainer-mail-address
  (concat "rubiki" "tch@ru" "by-lang.org"))
(defvar anything-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of anything.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"anything.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.
# If you are a Japanese, please write in Japanese:-)")
(defvar anything-no-dump-variables
  '(anything-candidate-buffer-alist
    anything-digit-overlays
    anything-help-message
    anything-candidate-cache
    )
  "Variables not to dump in bug report.")

(defun anything-dumped-variables-in-bug-report ()
  (let ((hash (make-hash-table)))
    (loop for var in (apropos-internal "anything-" 'boundp)
          for vname = (symbol-name var)
          unless (or (string-match "-map$" vname)
                     (string-match "^anything-c-source-" vname)
                     (string-match "-hash$" vname)
                     (string-match "-face$" vname)
                     (memq var anything-no-dump-variables))
          collect var)))

(defun anything-send-bug-report ()
  "Send a bug report of anything.el."
  (interactive)
  (with-current-buffer (or anything-last-buffer
                           (current-buffer))
    (reporter-submit-bug-report
     anything-maintainer-mail-address
     "anything.el"
     (anything-dumped-variables-in-bug-report)
     nil nil
     anything-bug-report-salutation)))

(defun anything-send-bug-report-from-anything ()
  "Send a bug report of anything.el in anything session."
  (interactive)
  (anything-run-after-quit 'anything-send-bug-report))

;; (@* "Unit Tests")

(defun* anything-test-candidates (sources &optional (input "") (compile-source-functions anything-compile-source-functions-default))
  "Test helper function for anything.
Given pseudo `anything-sources' and `anything-pattern', returns list like
  ((\"source name1\" (\"candidate1\" \"candidate2\"))
   (\"source name2\" (\"candidate3\" \"candidate4\")))
"
  (let ((anything-test-mode t)
        anything-enable-shortcuts
        anything-candidate-cache
        (anything-compile-source-functions compile-source-functions)
        anything-before-initialize-hook
        anything-after-initialize-hook
        anything-update-hook
        anything-test-candidate-list)
    (get-buffer-create anything-buffer)

    (anything-initialize-1 nil input sources)
    (anything-update)
    ;; test-mode spec: select 1st candidate!
    (with-current-buffer anything-buffer
      (forward-line 1)
      (anything-mark-current-line))
    (prog1
        anything-test-candidate-list
      (anything-cleanup))))

(defmacro anything-test-update (sources pattern)
  "Test helper macro for anything. It is meant for testing *anything* buffer contents."
  `(progn (stub anything-get-sources => ,sources)
          (stub anything-log-run-hook => nil)
          (stub anything-maybe-fit-frame => nil)
          (stub run-with-idle-timer => nil)
          (let (anything-test-mode (anything-pattern ,pattern))
            (anything-update))))

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "anything-current-buffer")
      (expect "__a_buffer"
        (with-current-buffer (get-buffer-create "__a_buffer")
          (anything-test-candidates '(((name . "FOO"))) "")
          (prog1
              (buffer-name anything-current-buffer)
            (kill-buffer "__a_buffer")
            )))
      (desc "anything-buffer-file-name")
      (expect (regexp "/__a_file__")
        (with-current-buffer (get-buffer-create "__a_file__")
          (setq buffer-file-name "/__a_file__")
          (anything-test-candidates '(((name . "FOO"))) "")
          (prog1
              anything-buffer-file-name
            ;;(kill-buffer "__a_file__")
            )))
      (desc "anything-interpret-value")
      (expect "literal"
        (anything-interpret-value "literal"))
      (expect "lambda"
        (anything-interpret-value (lambda () "lambda")))
      (expect "lambda with source name"
        (let ((source '((name . "lambda with source name"))))
          (anything-interpret-value (lambda () anything-source-name) source)))
      (expect "function symbol"
        (flet ((f () "function symbol"))
          (anything-interpret-value 'f)))
      (expect "variable symbol"
        (let ((v "variable symbol"))
          (anything-interpret-value 'v)))
      (expect (error error *)
        (anything-interpret-value 'unbounded-1))
      (desc "anything-compile-sources")
      (expect '(((name . "foo")))
        (anything-compile-sources '(((name . "foo"))) nil)
        )
      (expect '(((name . "foo") (type . test) (action . identity)))
        (let ((anything-type-attributes '((test (action . identity)))))
          (anything-compile-sources '(((name . "foo") (type . test)))
                                    '(anything-compile-source--type))))
      (desc "anything-sources accepts symbols")
      (expect '(((name . "foo")))
        (let* ((foo '((name . "foo"))))
          (anything-compile-sources '(foo) nil)))
      (desc "anything-get-sources action")
      (expect '(((name . "Actions") (candidates . actions)))
        (stub anything-action-window => t)
        (let (anything-compiled-sources
              (anything-sources '(((name . "Actions") (candidates . actions)))))
          (anything-get-sources)))
      (desc "get-buffer-create candidates-buffer")
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer . anything-candidates-in-buffer)
                 (candidates . anything-candidates-in-buffer)
                 (volatile) (match identity)))
        (anything-compile-sources
         '(((name . "many") (init . many-init)
            (candidates-in-buffer . anything-candidates-in-buffer)))
         '(anything-compile-source--candidates-in-buffer)))
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer)
                 (candidates . anything-candidates-in-buffer)
                 (volatile) (match identity)))
        (anything-compile-sources
         '(((name . "many") (init . many-init)
            (candidates-in-buffer)))
         '(anything-compile-source--candidates-in-buffer)))
      (expect '(((name . "many") (init . many-init)
                 (candidates-in-buffer)
                 (type . test)
                 (action . identity)
                 (candidates . anything-candidates-in-buffer)
                 (volatile) (match identity)))
        (let ((anything-type-attributes '((test (action . identity)))))
          (anything-compile-sources
           '(((name . "many") (init . many-init)
              (candidates-in-buffer)
              (type . test)))
           '(anything-compile-source--type
             anything-compile-source--candidates-in-buffer))))

      (desc "anything-get-candidates")
      (expect '("foo" "bar")
        (anything-get-candidates '((name . "foo") (candidates "foo" "bar"))))
      (expect '("FOO" "BAR")
        (anything-get-candidates '((name . "foo") (candidates "foo" "bar")
                                   (candidate-transformer
                                    . (lambda (cands) (mapcar 'upcase cands))))))
      (expect '("foo" "bar")
        (anything-get-candidates '((name . "foo")
                                   (candidates . (lambda () '("foo" "bar"))))))
      (expect '("foo" "bar")
        (let ((var '("foo" "bar")))
          (anything-get-candidates '((name . "foo")
                                     (candidates . var)))))
      (expect (error error *)
        (anything-get-candidates '((name . "foo")
                                   (candidates . "err"))))
      (expect (error error *)
        (let ((var "err"))
          (anything-get-candidates '((name . "foo")
                                     (candidates . var)))))
      (expect (error error *)
        (anything-get-candidates '((name . "foo")
                                   (candidates . unDeFined-syMbol))))
      (desc "anything-compute-matches")
      (expect '("foo" "bar")
        (let ((anything-pattern ""))
          (anything-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("foo")
        (let ((anything-pattern "oo"))
          (anything-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("bar")
        (let ((anything-pattern "^b"))
          (anything-compute-matches '((name . "FOO") (candidates "foo" "bar") (volatile)))))
      (expect '("a" "b")
        (let ((anything-pattern "")
              (anything-candidate-number-limit 2))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b")
        (let ((anything-pattern ".")
              (anything-candidate-number-limit 2))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b" "c")
        (let ((anything-pattern "")
              anything-candidate-number-limit)
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '("a" "b" "c")
        (let ((anything-pattern "[abc]")
              anything-candidate-number-limit)
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c") (volatile)))))
      (expect '(a b c)
        (let ((anything-pattern "[abc]")
              anything-candidate-number-limit)
          (anything-compute-matches '((name . "FOO") (candidates a b c) (volatile)))))
      (expect '(("foo" . "FOO") ("bar" . "BAR"))
        (let ((anything-pattern ""))
          (anything-compute-matches '((name . "FOO") (candidates ("foo" . "FOO") ("bar" . "BAR")) (volatile)))))
      (expect '(("foo" . "FOO"))
        (let ((anything-pattern "foo"))
          (anything-compute-matches '((name . "FOO") (candidates ("foo" . "FOO") ("bar" . "foo")) (volatile)))))
      ;; using anything-test-candidate-list
      (desc "anything-test-candidates")
      (expect '(("FOO" ("foo" "bar")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")))))
      (expect '(("FOO" ("bar")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar"))) "ar"))
      (expect '(("T1" ("hoge" "aiue"))
                ("T2" ("test" "boke")))
        (anything-test-candidates '(((name . "T1") (candidates "hoge" "aiue"))
                                    ((name . "T2") (candidates "test" "boke")))))
      (expect '(("T1" ("hoge"))
                ("T2" ("boke")))
        (anything-test-candidates '(((name . "T1") (candidates "hoge" "aiue"))
                                    ((name . "T2") (candidates "test" "boke"))) "o"))
      (desc "requires-pattern attribute")
      (expect nil
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (requires-pattern . 1)))))
      (expect '(("FOO" ("bar")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (requires-pattern . 1))) "b"))
      (desc "delayed attribute(for test)")
      (expect '(("T2" ("boke"))
                ("T1" ("hoge")))
        (anything-test-candidates
         '(((name . "T1") (candidates "hoge" "aiue") (delayed))
           ((name . "T2") (candidates "test" "boke")))
         "o"))
      (desc "match attribute(prefix search)")
      (expect '(("FOO" ("bar")))
        (anything-test-candidates
         '(((name . "FOO") (candidates "foo" "bar")
            (match (lambda (c) (string-match (concat "^" anything-pattern) c)))))
         "ba"))
      (expect nil
        (anything-test-candidates
         '(((name . "FOO") (candidates "foo" "bar")
            (match (lambda (c) (string-match (concat "^" anything-pattern) c)))))
         "ar"))
      (expect "TestSource"
        (let (x)
          (anything-test-candidates
           '(((name . "TestSource") (candidates "a")
              (match (lambda (c) (setq x anything-source-name)))))
           "a")
          x))
      (desc "init attribute")
      (expect '(("FOO" ("bar")))
        (let (v)
          (anything-test-candidates
           '(((name . "FOO") (init . (lambda () (setq v '("foo" "bar"))))
              (candidates . v)))
           "ar")))
      (desc "candidate-transformer attribute")
      (expect '(("FOO" ("BAR")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (candidate-transformer
                                      . (lambda (cands) (mapcar 'upcase cands)))))
                                  "ar"))
      (desc "filtered-candidate-transformer attribute")
      ;; needs more tests
      (expect '(("FOO" ("BAR")))
        (anything-test-candidates '(((name . "FOO") (candidates "foo" "bar")
                                     (filtered-candidate-transformer
                                      . (lambda (cands src) (mapcar 'upcase cands)))))
                                  "ar"))
      (desc "anything-transform-candidates in process")
      (expect (mock (anything-composed-funcall-with-source
                     '((name . "FOO") (candidates "foo" "bar")
                       (filtered-candidate-transformer
                        . (lambda (cands src) (mapcar 'upcase cands))))
                     (lambda (cands src) (mapcar 'upcase cands))
                     '("foo" "bar")
                     '((name . "FOO") (candidates "foo" "bar")
                       (filtered-candidate-transformer
                        . (lambda (cands src) (mapcar 'upcase cands))))
                     t))
        (stub anything-process-candidate-transformer => '("foo" "bar"))
        (anything-transform-candidates
         '("foo" "bar")
         '((name . "FOO") (candidates "foo" "bar")
           (filtered-candidate-transformer
            . (lambda (cands src) (mapcar 'upcase cands))))
         t)
        )
      (desc "anything-candidates-in-buffer-1")
      (expect nil
        (anything-candidates-in-buffer-1
         nil ""
         'buffer-substring-no-properties '(re-search-forward) 50 nil))
      (expect '("foo+" "bar+" "baz+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) ""
           'buffer-substring-no-properties '(re-search-forward) 5 nil)))
      (expect '("foo+" "bar+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) ""
           'buffer-substring-no-properties '(re-search-forward) 2 nil)))
      (expect '("foo+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) "oo\\+"
           'buffer-substring-no-properties '(re-search-forward) 50 nil)))
      (expect '("foo+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1 
           (current-buffer) "oo+"
           #'buffer-substring-no-properties '(search-forward) 50 nil)))
      (expect '("foo+" "bar+")
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) "."
           'buffer-substring-no-properties '(re-search-forward) 2 nil)))
      (expect '(("foo+" "FOO+"))
        (with-temp-buffer
          (insert "foo+\nbar+\nbaz+\n")
          (anything-candidates-in-buffer-1
           (current-buffer) "oo\\+"
           (lambda (s e)
             (let ((l (buffer-substring-no-properties s e)))
               (list l (upcase l))))
           '(re-search-forward) 50 nil)))
      (desc "anything-candidates-in-buffer")
      (expect '(("TEST" ("foo+" "bar+" "baz+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))))
      (expect '(("TEST" ("foo+" "bar+" "baz+")))
        (let (anything-candidate-number-limit)
          (anything-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                              (insert "foo+\nbar+\nbaz+\n"))))
              (candidates . anything-candidates-in-buffer)
              (match identity)
              (volatile))))))
      (expect '(("TEST" ("foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo\\+"))
      ;; BUG remain empty string, but the pattern is rare case.
      (expect '(("a" ("" "a" "b")))
        (anything-test-candidates
         '(((name . "a")
            (init . (lambda ()
                      (with-current-buffer (anything-candidate-buffer 'global)
                        (insert "a\nb\n"))))
            (candidates-in-buffer)))
         "a*"))
      (desc "search attribute")
      (expect '(("TEST" ("foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("foo+" "ooo")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search search-forward re-search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("foo+" "ooo")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\nooo\n"))))
            (search re-search-forward search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      (expect '(("TEST" ("ooo" "foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search re-search-forward search-forward)
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "oo+"))
      ;; faster exact match
      (expect '(("TEST" ("bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search (lambda (pattern &rest _)
                      (and (search-forward (concat "\n" pattern "\n") nil t)
                           (forward-line -1))))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "bar+"))
      ;; faster prefix match
      (expect '(("TEST" ("bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "bar+\nbaz+\nooo\nfoo+\n"))))
            (search (lambda (pattern &rest _)
                      (search-forward (concat "\n" pattern) nil t)))
            (candidates . anything-candidates-in-buffer)
            (match identity)
            (volatile)))
         "ba"))
      (desc "anything-current-buffer-is-modified")
      (expect '(("FOO" ("modified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (anything-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources))))
      (expect '(("FOO" ("unmodified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (anything-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources)
            (anything-test-candidates sources))))
      (expect '(("FOO" ("modified")))
        (let ((sources '(((name . "FOO")
                          (candidates
                           . (lambda ()
                               (if (anything-current-buffer-is-modified)
                                   '("modified")
                                 '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources)
            (insert "2")
            (anything-test-candidates sources))))
      (expect '(("BAR" ("modified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2))))
      (expect '(("FOO" ("unmodified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2)
            (anything-test-candidates sources1))))
      (expect '(("BAR" ("unmodified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2)
            (anything-test-candidates sources2))))
      (expect '(("BAR" ("modified")))
        (let ((sources1 '(((name . "FOO")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified")))))))
              (sources2 '(((name . "BAR")
                           (candidates
                            . (lambda ()
                                (if (anything-current-buffer-is-modified)
                                    '("modified")
                                  '("unmodified"))))))))
          (with-temp-buffer
            (clrhash anything-tick-hash)
            (insert "1")
            (anything-test-candidates sources1)
            (anything-test-candidates sources2)
            (with-temp-buffer
              (anything-test-candidates sources2)))))
      (desc "anything-source-name")
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (init
                                        . (lambda () (setq v anything-source-name)))
                                       (candidates "ok"))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates
                                        . (lambda ()
                                            (setq v anything-source-name)
                                            '("ok"))))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (candidate-transformer
                                        . (lambda (c)
                                            (setq v anything-source-name)
                                            c)))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (filtered-candidate-transformer
                                        . (lambda (c s)
                                            (setq v anything-source-name)
                                            c)))))
          v))
      (expect "FOO"
        (let (v)
          (anything-test-candidates '(((name . "FOO")
                                       (candidates "ok")
                                       (display-to-real
                                        . (lambda (c)
                                            (setq v anything-source-name)
                                            c))
                                       (action . identity))))
          (anything-execute-selection-action)
          v))
      (desc "anything-candidate-buffer create")
      (expect " *anything candidates:FOO*"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (buf (anything-candidate-buffer 'global)))
          (prog1 (buffer-name buf)
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (anything-current-buffer (get-buffer-create "aTestBuffer"))
               (buf (anything-candidate-buffer 'local)))
          (prog1 (buffer-name buf)
            (kill-buffer anything-current-buffer)
            (kill-buffer buf))))
      (expect 0
        (let (anything-candidate-buffer-alist
              (anything-source-name "FOO") buf)
          (with-current-buffer  (anything-candidate-buffer 'global)
            (insert "1"))
          (setq buf  (anything-candidate-buffer 'global))
          (prog1 (buffer-size buf)
            (kill-buffer buf))))
      (desc "anything-candidate-buffer get-buffer")
      (expect " *anything candidates:FOO*"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (buf (anything-candidate-buffer 'global)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (anything-current-buffer (get-buffer-create "aTestBuffer"))
               (buf (anything-candidate-buffer 'local)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer anything-current-buffer)
            (kill-buffer buf))))
      (expect " *anything candidates:FOO*"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (buf-local (anything-candidate-buffer 'local))
               (buf-global (anything-candidate-buffer 'global)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer buf-local)
            (kill-buffer buf-global))))
      (expect " *anything candidates:FOO*aTestBuffer"
        (let* (anything-candidate-buffer-alist
               (anything-source-name "FOO")
               (anything-current-buffer (get-buffer-create "aTestBuffer"))
               (buf-global (anything-candidate-buffer 'global))
               (buf-local (anything-candidate-buffer 'local)))
          (prog1 (buffer-name (anything-candidate-buffer))
            (kill-buffer buf-local)
            (kill-buffer buf-global))))
      (expect nil
        (let* (anything-candidate-buffer-alist
               (anything-source-name "NOP__"))
          (anything-candidate-buffer)))
      (desc "anything-candidate-buffer register-buffer")
      (expect " *anything test candidates*"
        (let (anything-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (prog1 (buffer-name (anything-candidate-buffer buf))
              (kill-buffer (current-buffer))))))
      (expect " *anything test candidates*"
        (let (anything-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (anything-candidate-buffer buf)
            (prog1 (buffer-name (anything-candidate-buffer))
              (kill-buffer (current-buffer))))))
      (expect "1\n2\n"
        (let (anything-candidate-buffer-alist
              (buf (get-buffer-create " *anything test candidates*")))
          (with-current-buffer buf
            (insert "1\n2\n")
            (anything-candidate-buffer buf)
            (prog1 (buffer-string)
              (kill-buffer (current-buffer))))))
      (expect "buf1"
        (let (anything-candidate-buffer-alist
              (anything-source-name "foo")
              (buf1 (get-buffer-create "buf1"))
              (buf2 (get-buffer-create "buf2")))
          (anything-candidate-buffer buf1)
          (anything-candidate-buffer buf2)
          (prog1 (buffer-name (anything-candidate-buffer buf1))
            (kill-buffer buf1)
            (kill-buffer buf2))))
      (desc "action attribute")
      (expect "foo"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . identity)))))
        (anything-execute-selection-action))
      (expect "foo"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . (lambda (c) (identity c)))))))
        (anything-execute-selection-action))
      (desc "anything-get-default-action")
      (expect 'upcase
        (anything-get-default-action '(("upcase" . upcase))))
      (expect 'downcase
        (anything-get-default-action '(("downcase" . downcase))))
      (expect (lambda (x) (capitalize x))
        (anything-get-default-action (lambda (x) (capitalize x))))
      (expect 'identity
        (anything-get-default-action 'identity))
      (desc "anything-execute-selection-action")
      (expect "FOO"
        (anything-execute-selection-action
         "foo" '(("upcase" . upcase))  nil))
      (expect "FOO"
        (anything-execute-selection-action
         "foo" '(("upcase" . (lambda (c) (upcase c)))) nil))

      (desc "display-to-real attribute")
      (expect "FOO"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (display-to-real . upcase)
            (action ("identity" . identity)))))
        (anything-execute-selection-action))
      (desc "cleanup test")
      (expect 'cleaned
        (let (v)
          (anything-test-candidates
           '(((name . "TEST")
              (cleanup . (lambda () (setq v 'cleaned))))))
          v))
      (desc "anything-get-current-source")
      ;; in init/candidates/action/candidate-transformer/filtered-candidate-transformer
      ;; display-to-real/cleanup function
      (expect "FOO"
        (assoc-default
         'name
         (anything-funcall-with-source '((name . "FOO")) 'anything-get-current-source)))
      ;; init
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (anything-get-current-source)))))))
          (assoc-default 'name v)))
      ;; candidates
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates . (lambda () (setq v (anything-get-current-source)) '("a"))))))
          (assoc-default 'name v)))
      ;; action
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (action
               . (lambda (c) (setq v (anything-get-current-source)) c)))))
          (anything-execute-selection-action)
          (assoc-default 'name v)))
      ;; candidate-transformer
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (candidate-transformer
               . (lambda (c) (setq v (anything-get-current-source)) c)))))
          (assoc-default 'name v)))
      ;; filtered-candidate-transformer
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (filtered-candidate-transformer
               . (lambda (c s) (setq v (anything-get-current-source)) c)))))
          (assoc-default 'name v)))
      ;; action-transformer
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (action-transformer
               . (lambda (a c) (setq v (anything-get-current-source)) a))
              (action . identity))))
          (anything-execute-selection-action)
          (assoc-default 'name v)))
      ;; display-to-real
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                                   (insert "a\n"))))
              (candidates-in-buffer)
              (display-to-real
               . (lambda (c) (setq v (anything-get-current-source)) c))
              (action . identity))))
          (anything-execute-selection-action)
          (assoc-default 'name v)))
      ;; cleanup
      (expect "FOO"
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (candidates "a")
              (cleanup
               . (lambda () (setq v (anything-get-current-source)))))))
          (assoc-default 'name v)))
      ;; candidates are displayed
      (expect "TEST"
        (anything-test-candidates
         '(((name . "TEST")
            (candidates "foo")
            (action ("identity" . identity)))))
        (assoc-default 'name (anything-get-current-source)))
      (desc "anything-attr")
      (expect "FOO"
        (anything-funcall-with-source
         '((name . "FOO"))
         (lambda ()
           (anything-attr 'name))))
      (expect 'fuga
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (hoge . fuga)
              (init . (lambda () (setq v (anything-attr 'hoge))))
              (candidates "a"))))
          v))
      (expect nil
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (anything-attr 'hoge))))
              (candidates "a"))))
          v))
      (expect nil
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (hoge)                    ;INCOMPATIBLE!
              (init . (lambda () (setq v (anything-attr 'hoge))))
              (candidates "a"))))
          v))
      (desc "anything-attr*")
      (expect "generic"
        (let (v (value1 "generic"))
          (anything-test-candidates
           '(((name . "FOO")
              (hoge . value1)
              (init . (lambda () (setq v (anything-attr* 'hoge)))))))
          v))
      (desc "anything-attr-defined")
      (expect (non-nil)
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (hoge)
              (init . (lambda () (setq v (anything-attr-defined 'hoge))))
              (candidates "a"))))
          v))      
      (expect nil
        (let (v)
          (anything-test-candidates
           '(((name . "FOO")
              (init . (lambda () (setq v (anything-attr-defined 'hoge))))
              (candidates "a"))))
          v))      
      (desc "anything-attrset")
      (expect '((name . "FOO") (hoge . 77))
        (let ((src '((name . "FOO") (hoge))))
          (anything-attrset 'hoge 77 src)
          src))
      (expect 77
        (anything-attrset 'hoge 77 '((name . "FOO") (hoge))))

      (expect '((name . "FOO") (hoge . 77))
        (let ((src '((name . "FOO") (hoge . 1))))
          (anything-attrset 'hoge 77 src)
          src))

      (expect '((name . "FOO") (hoge . 77) (x))
        (let ((src '((name . "FOO") (x))))
          (anything-attrset 'hoge 77 src)
          src))
      (expect 77
        (anything-attrset 'hoge 77 '((name . "FOO"))))
      (desc "anything-preselect")
      ;; entire candidate
      (expect "foo"
        (with-current-buffer (anything-create-anything-buffer t)
          (let ((anything-pattern "")
                (anything-test-mode t))
            (anything-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (anything-preselect "foo")
            (anything-get-selection))))
      ;; regexp
      (expect "foo"
        (with-current-buffer (anything-create-anything-buffer t)
          (let ((anything-pattern "")
                (anything-test-mode t))
            (anything-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (anything-preselect "fo+")
            (anything-get-selection))))
      ;; no match -> first entry
      (expect "hoge"
        (with-current-buffer (anything-create-anything-buffer t)
          (let ((anything-pattern "")
                (anything-test-mode t))
            (anything-process-source '((name . "test")
                                       (candidates "hoge" "foo" "bar")))
            (anything-preselect "not found")
            (anything-get-selection))))
      (desc "anything-check-new-input")
      (expect "newpattern"
        (stub anything-update)
        (stub anything-action-window)
        (let ((anything-pattern "pattern"))
          (anything-check-new-input "newpattern")
          anything-pattern))
      ;; anything-input == nil when action window is available
      (expect nil
        (stub anything-update)
        (stub anything-action-window => t)
        (let ((anything-pattern "pattern")
              anything-input)
          (anything-check-new-input "newpattern")
          anything-input))
      ;; anything-input == anything-pattern unless action window is available
      (expect "newpattern"
        (stub anything-update)
        (stub anything-action-window => nil)
        (let ((anything-pattern "pattern")
              anything-input)
          (anything-check-new-input "newpattern")
          anything-input))
      (expect (mock (anything-update))
        (stub anything-action-window)
        (let (anything-pattern)
          (anything-check-new-input "foo")))
      (desc "anything-update")
      (expect (mock (anything-process-source '((name . "1"))))
        (anything-test-update '(((name . "1"))) ""))
      ;; (find-function 'anything-update)
      ;; TODO el-mock.el should express 2nd call of function.
      ;;     (expect (mock (anything-process-source '((name . "2"))))
      ;;       (stub anything-get-sources => '(((name . "1")) ((name . "2"))))
      ;;       (stub anything-log-run-hook)
      ;;       (stub anything-maybe-fit-frame)
      ;;       (stub run-with-idle-timer)
      ;;       (anything-update))
      (expect (mock (run-with-idle-timer * nil 'anything-process-delayed-sources
                                         '(((name . "2") (delayed)))))
        (stub anything-get-sources => '(((name . "1"))
                                        ((name . "2") (delayed))))
        (stub anything-log-run-hook)
        (stub anything-maybe-fit-frame)
        (let ((anything-pattern "") anything-test-mode)
          (anything-update)))

      (desc "requires-pattern attribute")
      (expect (not-called anything-process-source)
        (anything-test-update '(((name . "1") (requires-pattern))) ""))
      (expect (not-called anything-process-source)
        (anything-test-update '(((name . "1") (requires-pattern . 3))) "xx"))

      (desc "anything-normalize-sources")
      (expect '(anything-c-source-test)
        (anything-normalize-sources 'anything-c-source-test))
      (expect '(anything-c-source-test)
        (anything-normalize-sources '(anything-c-source-test)))
      (expect '(anything-c-source-test)
        (let ((anything-sources '(anything-c-source-test)))
          (anything-normalize-sources nil)))
      (expect '(((name . "test")))
        (anything-normalize-sources '((name . "test"))))
      (expect '(((name . "test")))
        (anything-normalize-sources '(((name . "test")))))
      (desc "anything-get-action")
      (expect '(("identity" . identity))
        (stub buffer-size => 1)
        (stub anything-get-current-source => '((name . "test")
                                               (action ("identity" . identity))))
        (anything-get-action))
      (expect 'identity
        (stub buffer-size => 1)
        (stub anything-get-current-source => '((name . "test")
                                               (action . identity)))
        (anything-get-action))
      (expect '((("identity" . identity)) "action-transformer is called")
        (stub buffer-size => 1)
        (stub anything-get-current-source
              => '((name . "test")
                   (action ("identity" . identity))
                   (action-transformer
                    . (lambda (actions selection)
                        (list actions selection)))))
        (stub anything-get-selection => "action-transformer is called")
        (anything-get-action))
      (desc "anything-select-nth-action")
      (expect (error error *)
        (stub anything-get-selection => nil)
        (anything-select-nth-action 0))
      (desc "anything-get-nth-action")
      (expect 'cadr
        (anything-get-nth-action 2 '(("0" . car) ("1" . cdr) ("2" . cadr))))
      (expect (error error *)
        (anything-get-nth-action 2 '(("0" . car))))
      (expect 'identity
        (anything-get-nth-action 0 'identity))
      (expect (error error *)
        (anything-get-nth-action 1 'identity))
      (expect (error error *)
        (anything-get-nth-action 0 'unbound-function-xxx))
      (expect (error error *)
        (anything-get-nth-action 0 "invalid data"))
      (desc "anything-funcall-foreach")
      (expect (mock (upcase "foo"))
        (stub anything-get-sources => '(((init . (lambda () (upcase "foo"))))))
        (anything-funcall-foreach 'init))
      (expect (mock (downcase "bar"))
        (stub anything-get-sources => '(((init . (lambda () (upcase "foo"))))
                                        ((init . (lambda () (downcase "bar"))))))
        (anything-funcall-foreach 'init))
      (expect (not-called anything-funcall-with-source)
        (stub anything-get-sources => '(((init . (lambda () (upcase "foo"))))))
        (anything-funcall-foreach 'not-found))
      ;; TODO anything-select-with-digit-shortcut test
      (desc "anything-get-cached-candidates")
      (expect '("cached" "version")
        (let ((anything-candidate-cache '(("test" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")))))
      (expect '("new")
        (let ((anything-candidate-cache '(("other" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")))))
      (expect '(("test" "new")
                ("other" "cached" "version"))
        (let ((anything-candidate-cache '(("other" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")))
          anything-candidate-cache))
      (expect '(("other" "cached" "version"))
        (let ((anything-candidate-cache '(("other" "cached" "version"))))
          (anything-get-cached-candidates '((name . "test")
                                            (candidates "new")
                                            (volatile)))
          anything-candidate-cache))
      ;; TODO when candidates == process
      ;; TODO anything-output-filter
      (desc "candidate-number-limit attribute")
      (expect '("a" "b")
        (let ((anything-pattern "")
              (anything-candidate-number-limit 20))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c")
                                      (candidate-number-limit . 2) (volatile)))))
      (expect '("a" "b")
        (let ((anything-pattern "[abc]")
              (anything-candidate-number-limit 20))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c")
                                      (candidate-number-limit . 2) (volatile)))))
      (expect '("a" "b" "c" "d")
        (let ((anything-pattern "[abcd]")
              (anything-candidate-number-limit 2))
          (anything-compute-matches '((name . "FOO") (candidates "a" "b" "c" "d")
                                      (candidate-number-limit) (volatile)))))
      (expect '(("TEST" ("a" "b" "c")))
        (let ((anything-candidate-number-limit 2))
          (anything-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                              (insert "a\nb\nc\nd\n"))))
              (candidates . anything-candidates-in-buffer)
              (match identity)
              (candidate-number-limit . 3)
              (volatile))))))
      (expect '(("TEST" ("a" "b" "c")))
        (let ((anything-candidate-number-limit 2))
          (anything-test-candidates
           '(((name . "TEST")
              (init
               . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                              (insert "a\nb\nc\nd\n"))))
              (candidates . anything-candidates-in-buffer)
              (match identity)
              (candidate-number-limit . 3)
              (volatile)))
           ".")))
      (desc "multiple init")
      (expect '(1 . 2)
        (let (a b)
          (anything-test-candidates
           '(((name . "test")
              (init (lambda () (setq a 1))
                    (lambda () (setq b 2))))))
          (cons a b)))
      (expect 1
        (let (a)
          (anything-test-candidates
           '(((name . "test")
              (init (lambda () (setq a 1))))))
          a))
      (desc "multiple cleanup")
      (expect '(1 . 2)
        (let (a b)
          (anything-test-candidates
           '(((name . "test")
              (cleanup (lambda () (setq a 1))
                       (lambda () (setq b 2))))))
          (cons a b)))
      (desc "anything-mklist")
      (expect '(1)
        (anything-mklist 1))
      (expect '(2)
        (anything-mklist '(2)))
      (expect '((lambda ()))
        (anything-mklist (lambda ())))
      (desc "anything-before-initialize-hook")
      (expect 'called
        (let ((anything-before-initialize-hook '((lambda () (setq v 'called))))
              v)
          (anything-initialize)
          v))
      (desc "anything-after-initialize-hook")
      (expect '(b a)
        (let ((anything-before-initialize-hook
               '((lambda () (setq v '(a)))))
              (anything-after-initialize-hook
               '((lambda () (setq v (cons 'b v)))))
              v)
          (anything-initialize)
          v))
      (expect 0
        (let ((anything-after-initialize-hook
               '((lambda () (setq v (buffer-size (get-buffer anything-buffer))))))
              v)
          (anything-initialize)
          v))
      (desc "get-line attribute")
      (expect '(("TEST" ("FOO+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (get-line . (lambda (s e) (upcase (buffer-substring-no-properties s e))))))
         "oo\\+"))
      (desc "with-anything-restore-variables")
      (expect '(7 8)
        (let ((a 7) (b 8)
              (anything-restored-variables '(a b)))
          (with-anything-restore-variables
            (setq a 0 b 0))
          (list a b)))
      (desc "anything-cleanup-hook")
      (expect 'called
        (let ((anything-cleanup-hook
               '((lambda () (setq v 'called))))
              v)
          (anything-cleanup)
          v))
      (desc "with-anything-display-same-window")
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (display-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (pop-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (split-window)
          
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (switch-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (display-buffer buf)
              (eq win (get-buffer-window buf))))))
      (expect (non-nil)
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (get-buffer-create " tmp"))
                (win (selected-window)))
            (with-anything-display-same-window
              (pop-to-buffer buf)
              (eq win (get-buffer-window buf))))))
      (desc "search-from-end attribute")
      (expect '(("TEST" ("baz+" "bar+" "foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)))))
      (expect '(("TEST" ("baz+" "bar+" "foo+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)))
         "\\+"))
      (expect '(("TEST" ("baz+" "bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)
            (candidate-number-limit . 2)))))
      (expect '(("TEST" ("baz+" "bar+")))
        (anything-test-candidates
         '(((name . "TEST")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (insert "foo+\nbar+\nbaz+\n"))))
            (candidates-in-buffer)
            (search-from-end)
            (candidate-number-limit . 2)))
         "\\+"))
      (expect '(("a" ("c2" "c1")))
        (anything-test-candidates
         '(((name . "a")
            (init . (lambda ()
                      (with-current-buffer (anything-candidate-buffer 'global)
                        (insert "c1\nc2\n"))))
            (search-from-end)
            (candidates-in-buffer)))))
      ;; BUG remain empty string, but the pattern is rare case.
      (expect '(("a" ("c" "b" "a" "")))
        (anything-test-candidates
         '(((name . "a")
            (init . (lambda ()
                      (with-current-buffer (anything-candidate-buffer 'global)
                        (insert "a\nb\nc\n"))))
            (search-from-end)
            (candidates-in-buffer)))
         "a*"))
      (desc "header-name attribute")
      (expect "original is transformed"
        (anything-test-update '(((name . "original")
                                 (candidates "1")
                                 (header-name
                                  . (lambda (name)
                                      (format "%s is transformed" name)))))
                              "")
        (with-current-buffer (anything-buffer-get)
          (buffer-string)
          (overlay-get (car (overlays-at (1+(point-min)))) 'display)))
      (desc "volatile and match attribute")
      ;; candidates function is called once per `anything-process-delayed-sources'
      (expect 1
        (let ((v 0))
          (anything-test-candidates '(((name . "test")
                                       (candidates . (lambda () (incf v) '("ok")))
                                       (volatile)
                                       (match identity identity identity)))
                                    "o")
          v))
      (desc "accept-empty attribute")
      (expect nil
        (anything-test-candidates
         '(((name . "test") (candidates "") (action . identity))))
        (anything-execute-selection-action))
      (expect ""
        (anything-test-candidates
         '(((name . "test") (candidates "") (action . identity) (accept-empty))))
        (anything-execute-selection-action))
      (desc "anything-tick-hash")
      (expect nil
        (with-current-buffer (get-buffer-create " *00create+*")
          (puthash " *00create+*/xxx" 1 anything-tick-hash)
          (kill-buffer (current-buffer)))
        (gethash " *00create+*/xxx" anything-tick-hash))
      (desc "anything-execute-action-at-once-if-once")
      (expect "HOGE"
        (let ((anything-execute-action-at-once-if-one t))
          (anything '(((name . "one test1")
                       (candidates "hoge")
                       (action . upcase))))))
      (expect "ANY"
        (let ((anything-execute-action-at-once-if-one t))
          (anything '(((name . "one test2")
                       (candidates "hoge" "any")
                       (action . upcase)))
                    "an")))
      ;; candidates > 1
      (expect (mock (read-string "word: " nil))
        (let ((anything-execute-action-at-once-if-one t))
          (anything '(((name . "one test3")
                       (candidates "hoge" "foo" "bar")
                       (action . identity)))
                    nil "word: ")))
      (desc "anything-quit-if-no-candidate")
      (expect nil
        (let ((anything-quit-if-no-candidate t))
          (anything '(((name . "zero test1") (candidates) (action . upcase))))))
      (expect 'called
        (let (v (anything-quit-if-no-candidate (lambda () (setq v 'called))))
          (anything '(((name . "zero test2") (candidates) (action . upcase))))
          v))
      (desc "real-to-display attribute")
      (expect '(("test" (("DDD" . "ddd"))))
        (anything-test-candidates '(((name . "test")
                                     (candidates "ddd")
                                     (real-to-display . upcase)
                                     (action . identity)))))
      (expect '(("test" (("DDD" . "ddd"))))
        (anything-test-candidates '(((name . "test")
                                     (candidates ("ignored" . "ddd"))
                                     (real-to-display . upcase)
                                     (action . identity)))))
      (expect '(("Commands" (("xxxhoge" . "hoge") ("xxxboke" . "boke"))))
        (anything-test-candidates '(((name . "Commands")
                                     (candidates
                                      "hoge" "boke")
                                     (real-to-display . (lambda (x) (concat "xxx" x)))
                                     (action . identity)))
                                  "xxx"))
      (expect "test\nDDD\n"
        (anything-test-update '(((name . "test")
                                 (candidates "ddd")
                                 (real-to-display . upcase)
                                 (action . identity)))
                              "")
        (with-current-buffer (anything-buffer-get) (buffer-string)))
      (desc "real-to-display and candidate-transformer attribute")
      (expect '(("test" (("DDD" . "ddd"))))
        (anything-test-candidates
         '(((name . "test")
            (candidates "ddd")
            (candidate-transformer (lambda (cands) (mapcar (lambda (c) (cons "X" c)) cands)))
            (real-to-display . upcase)
            (action . identity)))))
      (expect "test\nDDD\n"
        (anything-test-update
         '(((name . "test")
            (candidates "ddd")
            (candidate-transformer (lambda (cands) (mapcar (lambda (c) (cons "X" c)) cands)))
            (real-to-display . upcase)
            (action . identity)))
         "")
        (with-current-buffer (anything-buffer-get) (buffer-string)))
      (desc "real-to-display and candidates-in-buffer")
      (expect '(("test" (("A" . "a") ("B" . "b"))))
        (anything-test-candidates
         '(((name . "test")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (erase-buffer)
                            (insert "a\nb\n"))))
            (candidates-in-buffer)
            (real-to-display . upcase)
            (action . identity)))))
      (expect "test\nA\nB\n"
        (stub read-string)
        (anything
         '(((name . "test")
            (init
             . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                            (erase-buffer)
                            (insert "a\nb\n"))))
            (candidates-in-buffer)
            (real-to-display . upcase)
            (action . identity))))
        (with-current-buffer (anything-buffer-get) (buffer-string)))
      (desc "Symbols are acceptable as candidate.")
      (expect '(("test" (sym "str")))
        (anything-test-candidates
         '(((name . "test")
            (candidates sym "str")))))
      (expect '(("test" ((sym . realsym) ("str" . "realstr"))))
        (anything-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))))
      (expect '(("test" (sym)))
        (anything-test-candidates
         '(((name . "test")
            (candidates sym "str")))
         "sym"))
      (expect '(("test" ("str")))
        (anything-test-candidates
         '(((name . "test")
            (candidates sym "str")))
         "str"))
      (expect '(("test" ((sym . realsym))))
        (anything-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))
         "sym"))
      (expect '(("test" (("str" . "realstr"))))
        (anything-test-candidates
         '(((name . "test")
            (candidates (sym . realsym) ("str" . "realstr"))))
         "str"))
      (desc "multiple transformers")
      (expect '(("test" ("<FOO>")))
        (anything-test-candidates
         '(((name . "test")
            (candidates "foo")
            (candidate-transformer
             . (lambda (cands)
                 (anything-compose (list cands)
                                   (list (lambda (c) (mapcar 'upcase c))
                                         (lambda (c) (list (concat "<" (car c) ">")))))))))))
      (expect '("<FOO>")
        (anything-composed-funcall-with-source
         '((name . "test"))
         (list (lambda (c) (mapcar 'upcase c))
               (lambda (c) (list (concat "<" (car c) ">"))))
         '("foo"))
        )
      (expect '(("test" ("<FOO>")))
        (anything-test-candidates
         '(((name . "test")
            (candidates "foo")
            (candidate-transformer
             (lambda (c) (mapcar 'upcase c))
             (lambda (c) (list (concat "<" (car c) ">"))))))))
      (expect '(("test" ("<BAR>")))
        (anything-test-candidates
         '(((name . "test")
            (candidates "bar")
            (filtered-candidate-transformer
             (lambda (c s) (mapcar 'upcase c))
             (lambda (c s) (list (concat "<" (car c) ">"))))))))
      (expect '(("find-file" . find-file)
                ("view-file" . view-file))
        (stub zerop => nil)
        (stub anything-get-current-source
              => '((name . "test")
                   (action)
                   (action-transformer
                    . (lambda (a s)
                        (anything-compose
                         (list a s)
                         (list (lambda (a s) (push '("view-file" . view-file) a))
                               (lambda (a s) (push '("find-file" . find-file) a))))))))
        (anything-get-action))
      (expect '(("find-file" . find-file)
                ("view-file" . view-file))
        (stub zerop => nil)
        (stub anything-get-current-source
              => '((name . "test")
                   (action)
                   (action-transformer
                    (lambda (a s) (push '("view-file" . view-file) a))
                    (lambda (a s) (push '("find-file" . find-file) a)))))
        (anything-get-action))
      (desc "define-anything-type-attribute")
      (expect '((file (action . find-file)))
        (let (anything-type-attributes)
          (define-anything-type-attribute 'file '((action . find-file)))
          anything-type-attributes))
      (expect '((file (action . find-file)))
        (let ((anything-type-attributes '((file (action . view-file)))))
          (define-anything-type-attribute 'file '((action . find-file)))
          anything-type-attributes))
      (expect '((file (action . find-file))
                (buffer (action . switch-to-buffer)))
        (let (anything-type-attributes)
          (define-anything-type-attribute 'buffer '((action . switch-to-buffer)))
          (define-anything-type-attribute 'file '((action . find-file)))
          anything-type-attributes))
      (desc "anything-approximate-candidate-number")
      (expect 0
        (with-temp-buffer
          (let ((anything-buffer (current-buffer)))
            (anything-approximate-candidate-number))))
      (expect 1
        (with-temp-buffer
          (let ((anything-buffer (current-buffer)))
            (insert "Title\n"
                    "candiate1\n")
            (anything-approximate-candidate-number))))
      (expect t
        (with-temp-buffer
          (let ((anything-buffer (current-buffer)))
            (insert "Title\n"
                    "candiate1\n"
                    "candiate2\n")
            (<= 2 (anything-approximate-candidate-number)))))
      (expect 1
        (with-temp-buffer
          (let ((anything-buffer (current-buffer)))
            (insert "Title\n"
                    (propertize "multi\nline\n" 'anything-multiline t))
            (anything-approximate-candidate-number))))
      (expect t
        (with-temp-buffer
          (let ((anything-buffer (current-buffer))
                (anything-candidate-separator "-----"))
            (insert "Title\n"
                    (propertize "multi\nline1\n" 'anything-multiline t)
                    "-----\n"
                    (propertize "multi\nline2\n" 'anything-multiline t))
            (<= 2 (anything-approximate-candidate-number)))))
      (desc "delayed-init attribute")
      (expect 0
        (let ((value 0))
          (anything-test-candidates '(((name . "test")
                                       (delayed-init . (lambda () (incf value)))
                                       (candiates "abc")
                                       (requires-pattern . 2)))
                                    "")
          value))
      (expect 1
        (let ((value 0))
          (anything-test-candidates '(((name . "test")
                                       (delayed-init . (lambda () (incf value)))
                                       (candiates "abc")
                                       (requires-pattern . 2)))
                                    "abc")
          value))
      (expect 2
        (let ((value 0))
          (anything-test-candidates '(((name . "test")
                                       (delayed-init (lambda () (incf value))
                                                     (lambda () (incf value)))
                                       (candiates "abc")
                                       (requires-pattern . 2)))
                                    "abc")
          value))
      (expect t
        (let (value)
          (with-temp-buffer
            (anything-test-candidates '(((name . "test")
                                         (delayed-init
                                          . (lambda () (setq value
                                                             (eq anything-current-buffer (current-buffer)))))
                                         (candiates "abc")
                                         (requires-pattern . 2)))
                                      "abc")
            value)))
      (desc "pattern-transformer attribute")
      (expect '(("test2" ("foo")) ("test3" ("bar")))
        (anything-test-candidates '(((name . "test1")
                                     (candidates "foo" "bar"))
                                    ((name . "test2")
                                     (pattern-transformer . (lambda (pat) (substring pat 1)))
                                     (candidates "foo" "bar"))
                                    ((name . "test3")
                                     (pattern-transformer . (lambda (pat) "bar"))
                                     (candidates "foo" "bar")))
                                  "xfoo"))
      (expect '(("test2" ("foo")) ("test3" ("bar")))
        (anything-test-candidates '(((name . "test1")
                                     (candidates "foo" "bar"))
                                    ((name . "test2")
                                     (pattern-transformer (lambda (pat) (substring pat 1)))
                                     (candidates "foo" "bar"))
                                    ((name . "test3")
                                     (pattern-transformer (lambda (pat) "bar"))
                                     (candidates "foo" "bar")))
                                  "xfoo"))
      (expect '(("test2" ("foo")) ("test3" ("bar")))
        (anything-test-candidates '(((name . "test1")
                                     (init
                                      . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                                                     (insert "foo\nbar\n"))))
                                     (candidates-in-buffer))
                                    ((name . "test2")
                                     (pattern-transformer . (lambda (pat) (substring pat 1)))
                                     (init
                                      . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                                                     (insert "foo\nbar\n"))))
                                     (candidates-in-buffer))
                                    ((name . "test3")
                                     (pattern-transformer . (lambda (pat) "bar"))
                                     (init
                                      . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                                                     (insert "foo\nbar\n"))))
                                     (candidates-in-buffer)))
                                  "xfoo"))
      (desc "anything-recent-push")
      (expect '("foo" "bar" "baz")
        (let ((lst '("bar" "baz")))
          (anything-recent-push "foo" 'lst)))
      (expect '("foo" "bar" "baz")
        (let ((lst '("foo" "bar" "baz")))
          (anything-recent-push "foo" 'lst)))
      (expect '("foo" "bar" "baz")
        (let ((lst '("bar" "foo" "baz")))
          (anything-recent-push "foo" 'lst)))
      (desc "anything-require-at-least-version")
      (expect nil
        (anything-require-at-least-version "1.1"))
      (expect nil
        (anything-require-at-least-version "1.200"))
      (expect nil
        (anything-require-at-least-version
         (and (string-match "1\.\\([0-9]+\\)" anything-version)
              (match-string 0 anything-version))))
      (expect (error)
        (anything-require-at-least-version "1.999"))
      (expect (error)
        (anything-require-at-least-version "1.2000"))
      (desc "anything-once")
      (expect 2
        (let ((i 0))
          (anything-test-candidates
           '(((name . "1")
              (init . (lambda () (incf i))))
             ((name . "2")
              (init . (lambda () (incf i))))))
          i))
      (expect 1
        (let ((i 0))
          (anything-test-candidates
           '(((name . "1")
              (init . (lambda () (anything-once (lambda () (incf i))))))
             ((name . "2")
              (init . (lambda () (anything-once (lambda () (incf i))))))))
          i))
      (expect 1
        (let ((i 0))
          (flet ((init1 () (anything-once (lambda () (incf i)))))
            (anything-test-candidates
             '(((name . "1")
                (init . init1))
               ((name . "2")
                (init . init1)))))
          i))
      (desc "anything-marked-candidates")
      (expect '("mark3" "mark1")
        (let* ((source '((name . "mark test")))
               (anything-marked-candidates
                `((,source . "mark1")
                  (((name . "other")) . "mark2")
                  (,source . "mark3"))))
          (stub anything-buffer-get => (current-buffer))
          (stub anything-get-current-source => source)
          (anything-marked-candidates)))
      (expect '("current")
        (let* ((source '((name . "mark test")))
               (anything-marked-candidates nil))
          (stub anything-get-current-source => source)
          (stub anything-get-selection => "current")
          (anything-marked-candidates)))
      (desc "anything-marked-candidates with coerce")
      (expect '(mark3 mark1)
        (let* ((source '((name . "mark test")
                         (coerce . intern)))
               (anything-marked-candidates
                `((,source . "mark1")
                  (((name . "other")) . "mark2")
                  (,source . "mark3"))))
          (stub anything-buffer-get => (current-buffer))
          (stub anything-get-current-source => source)
          (anything-marked-candidates)))
      (desc "anything-let")
      (expect '(1 10000 nil)
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let ((a 1)
                         (b (1+ a))
                         c)
            (anything-create-anything-buffer))
          (with-current-buffer anything-buffer
            (list a b c))))
      (expect (non-nil)
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let ((a 1)
                         (b (1+ a))
                         c)
            (anything-create-anything-buffer))
          (with-current-buffer anything-buffer
            (and (assq 'a (buffer-local-variables))
                 (assq 'b (buffer-local-variables))
                 (assq 'c (buffer-local-variables))))))
      (expect 'retval
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let ((a 1)
                         (b (1+ a))
                         c)
            'retval)))
      (desc "anything-let*")
      (expect '(1 2 nil)
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let* ((a 1)
                          (b (1+ a))
                          c)
            (anything-create-anything-buffer))
          (with-current-buffer anything-buffer
            (list a b c))))
      (expect (non-nil)
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let* ((a 1)
                          (b (1+ a))
                          c)
            (anything-create-anything-buffer))
          (with-current-buffer anything-buffer
            (and (assq 'a (buffer-local-variables))
                 (assq 'b (buffer-local-variables))
                 (assq 'c (buffer-local-variables))))))
      (expect 'retval*
        (let ((a 9999)
              (b 8)
              (c)
              (anything-buffer (exps-tmpbuf)))
          (anything-let* ((a 1)
                          (b (1+ a))
                          c)
            'retval*)))
      (desc "anything with keyword")
      (expect (mock (anything-internal 'test-source "input" "prompt: " nil "preselect" "*test*" nil))
        (anything :sources   'test-source
                  :input     "input"
                  :prompt    "prompt: "
                  :resume    nil
                  :preselect "preselect"
                  :buffer    "*test*"
                  :keymap    nil))
      (expect (mock (anything-internal 'test-source nil nil nil nil "*test*" nil))
        (anything :sources                'test-source
                  :buffer                 "*test*"
                  :candidate-number-limit 20))
      (expect (mock (anything-internal 'test-source nil nil nil nil "*test*" nil))
        (anything 'test-source nil nil nil nil "*test*" nil))
      (desc "anything-log-eval-internal")
      (expect (mock (anything-log "%S = %S" '(+ 1 2) 3))
        (anything-log-eval-internal '((+ 1 2))))
      (expect (mock (anything-log "%S = ERROR!" 'unDeFined))
        (anything-log-eval-internal '(unDeFined)))

      (desc "anything-output-filter--collect-candidates")
      (expect '("a" "b" "")
        (split-string "a\nb\n" "\n"))
      (expect '("a" "b")
        (anything-output-filter--collect-candidates
         '("a" "b" "") (cons 'incomplete-line  "")))
      (expect '("a" "b")
        (split-string "a\nb" "\n"))
      (expect '("a")
        (anything-output-filter--collect-candidates
         '("a" "b") (cons 'incomplete-line  "")))
      (expect '(incomplete-line . "b")
        (let ((incomplete-line-info (cons 'incomplete-line  "")))
          (anything-output-filter--collect-candidates
           '("a" "b") incomplete-line-info)
          incomplete-line-info))
      (expect '("" "c" "")
        (split-string "\nc\n" "\n"))
      (expect '("b" "c")
        ;; "a\nb" + "\nc\n"
        (let ((incomplete-line-info (cons 'incomplete-line  "")))
          (anything-output-filter--collect-candidates
           '("a" "b") incomplete-line-info)
          (anything-output-filter--collect-candidates
           '("" "c" "") incomplete-line-info)))
      (desc "coerce attribute")
      (expect "string"
        (anything :sources '(((name . "test")
                              (candidates "string")
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'symbol
        (anything :sources '(((name . "test")
                              (candidates "symbol")
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'real
        (anything :sources '(((name . "test")
                              (candidates ("display" . "real"))
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'real
        (anything :sources '(((name . "test")
                              (candidates)
                              (candidate-transformer
                               (lambda (c) '(("display" . "real"))))
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'real
        (anything :sources '(((name . "test")
                              (candidates)
                              (filtered-candidate-transformer
                               (lambda (c s) '(("display" . "real"))))
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (expect 'real
        (anything :sources '(((name . "test")
                              (candidates "dummy")
                              (display-to-real (lambda (disp) "real"))
                              (coerce . intern)
                              (action . identity)))
                  :execute-action-at-once-if-one t))
      (desc "anything-next-point-in-list")
      (expect 10
        (anything-next-point-in-list 5 '(10 20) nil))
      (expect 20
        (anything-next-point-in-list 15 '(10 20) nil))
      (expect 25
        (anything-next-point-in-list 25 '(10 20) nil))
      (expect 5
        (anything-next-point-in-list 5 '(10 20) t))
      (expect 10
        (anything-next-point-in-list 15 '(10 20) t))
      (expect 20
        (anything-next-point-in-list 25 '(10 20) t))
      (expect 5
        (anything-next-point-in-list 5 '() nil))
      (expect 5
        (anything-next-point-in-list 5 '() t))
      (expect 10
        (anything-next-point-in-list 5 '(10) nil))
      (expect 10
        (anything-next-point-in-list 15 '(10) t))
      (expect 20
        (anything-next-point-in-list 10 '(10 20) nil))
      (expect 10
        (anything-next-point-in-list 20 '(10 20) t))
      (expect 20
        (anything-next-point-in-list 30 '(10 20 30) t))
      )))


(provide 'anything)
;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything.el"))
;;; anything.el ends here
