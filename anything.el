;;; anything.el --- open anything / QuickSilver-like candidate-selection framework
;; $Id: anything.el,v 1.45 2008-08-16 11:27:59 rubikitch Exp $

;; Copyright (C) 2007  Tamas Patrovics
;;               2008  rubikitch <rubikitch@ruby-lang.org>

;; Author: Tamas Patrovics
;; Maintainer: rubikitch <rubikitch@ruby-lang.org>
;; Keywords: files, frames, help, matching, outlines, processes, tools, convenience, anything
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything.el
;; Site: http://www.emacswiki.org/cgi-bin/emacs/Anything

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
;; select with up/down/pgup/pgdown, choose with enter, left/right
;; moves between sources. With TAB actions can be selected if the
;; selected candidate has more than one possible action.
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
;; Maintainer's configuration is in the EmacsWiki.
;;
;; http://www.emacswiki.org/cgi-bin/emacs/RubikitchAnythingConfiguration
;;
;; Tested on Emacs 22.
;;
;;
;; Thanks to Vagn Johansen for ideas.
;; Thanks to Stefan Kamphausen for fixes and XEmacs support.
;; Thanks to Tassilo Horn for fixes.
;; Thanks to Drew Adams for various fixes (frame, isearch, customization, etc.)
;; Thanks to IMAKADO for candidates-in-buffer idea.
;;

;;; Tips:

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
;; Using `anything-candidates-buffer' and the candidates-in-buffer
;; attribute is much faster than traditional "candidates and match"
;; way. And `anything-current-buffer-is-modified' avoids to
;; recalculate candidates for unmodified buffer. See docstring of
;; them.
;;
;; [EVAL IT] (describe-function 'anything-candidates-buffer)
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


;; TODO:
;;
;;   - process status indication
;;
;;   - results from async sources should appear in the order they are
;;     specified in anything-sources
;;
;;   - async sources doesn't honor digit-shortcut-count
;;
;;   - anything-candidate-number-limit can't be nil everywhere
;;
;;   - support multi line candidates

;; HISTORY:
;; $Log: anything.el,v $
;; Revision 1.45  2008-08-16 11:27:59  rubikitch
;; refactoring
;;  `anything-aif': Anaphoric if.
;;  `anything-compile-source-functions': make `anything-get-sources' customizable.
;;
;; Revision 1.44  2008/08/16 09:38:15  rubikitch
;; *** empty log message ***
;;
;; Revision 1.43  2008/08/15 11:44:28  rubikitch
;; `anything-read-string-mode': minor mode for `anything' version of read functions. (experimental)
;;
;; Revision 1.42  2008/08/15 11:03:20  rubikitch
;; update docs
;;
;; Revision 1.41  2008/08/14 20:51:28  rubikitch
;; New `anything-sources' attribute: cleanup
;;
;; Revision 1.40  2008/08/14 10:34:04  rubikitch
;; `anything': SOURCES: accept symbols
;;
;; Revision 1.39  2008/08/10 22:46:01  rubikitch
;; `anything-move-selection': avoid infinite loop
;;
;; Revision 1.38  2008/08/09 21:38:25  rubikitch
;; `anything-read-file-name': experimental implementation.
;;
;; Revision 1.37  2008/08/09 17:54:25  rubikitch
;; action test
;;
;; Revision 1.36  2008/08/09 17:13:00  rubikitch
;; fixed test
;;
;; Revision 1.35  2008/08/09 10:43:08  rubikitch
;; New `anything-sources' attribute: display-to-real
;;
;; Revision 1.34  2008/08/07 13:15:44  rubikitch
;; New `anything-sources' attribute: search
;;
;; Revision 1.33  2008/08/05 23:14:20  rubikitch
;; `anything-candidates-buffer': bugfix
;;
;; Revision 1.32  2008/08/05 21:42:15  rubikitch
;; *** empty log message ***
;;
;; Revision 1.31  2008/08/05 21:06:23  rubikitch
;; `anything-candidates-buffer': candidates buffer registration
;;
;; Revision 1.30  2008/08/05 19:46:36  rubikitch
;; New `anything-sources' attribute: candidates-in-buffer
;;
;; Revision 1.29  2008/08/05 17:58:31  rubikitch
;; *** empty log message ***
;;
;; Revision 1.28  2008/08/05 17:46:04  rubikitch
;; memoized `anything-get-sources'
;;
;; Revision 1.27  2008/08/05 17:29:40  rubikitch
;; update doc
;;
;; Revision 1.26  2008/08/05 08:35:45  rubikitch
;; `anything-completing-read': accept obarray
;;
;; Revision 1.25  2008/08/05 07:26:17  rubikitch
;; `anything-completing-read': guard from non-string return value
;;
;; Revision 1.24  2008/08/04 12:05:41  rubikitch
;; Wrote Tips and some docstrings.
;; `anything-candidates-buffer': buffer-local by default
;;
;; Revision 1.23  2008/08/04 05:29:46  rubikitch
;; `anything-buffer-file-name': `buffer-file-name' when `anything' is invoked.
;;
;; Revision 1.22  2008/08/04 00:10:13  rubikitch
;; `anything-candidates-buffer': new API
;;
;; Revision 1.21  2008/08/03 22:05:08  rubikitch
;; `anything-candidates-buffer': Return a buffer containing candidates of current source.
;;
;; Revision 1.20  2008/08/03 20:47:56  rubikitch
;; `anything-current-buffer-is-modified': modify checker
;;
;; Revision 1.19  2008/08/03 19:06:18  rubikitch
;; `anything-candidates-in-buffer': use `with-current-buffer' instead.
;;
;; Revision 1.18  2008/08/03 05:55:01  rubikitch
;; `anything-candidates-in-buffer': extract candidates in a buffer for speed.
;;
;; Revision 1.17  2008/08/02 21:31:29  rubikitch
;; Extended `anything' optional arguments.
;; `anything-completing-read': experimental implementation.
;;
;; Revision 1.16  2008/08/02 20:32:54  rubikitch
;; Extended `anything' optional arguments.
;;
;; Revision 1.15  2008/08/02 16:53:40  rubikitch
;; Fixed a small bug of `anything-test-candidates'.
;;
;; Revision 1.14  2008/08/02 16:48:29  rubikitch
;; Refactored to testable code.
;; Added many candidate tests with `anything-test-candidates'.
;;
;; Revision 1.13  2008/08/02 15:08:14  rubikitch
;; *** empty log message ***
;;
;; Revision 1.12  2008/08/02 14:29:31  rubikitch
;; `anything-sources' accepts symbols. (patched by Sugawara)
;;
;; Revision 1.11  2008/08/02 10:20:36  rubikitch
;; `anything-resume' is usable with other (let-binded) `anything-sources'.
;;
;; Revision 1.10  2008/08/01 19:44:01  rubikitch
;; `anything-resume': resurrect previously invoked `anything'.
;;
;; Revision 1.9  2008/07/30 15:44:49  rubikitch
;; *** empty log message ***
;;
;; Revision 1.8  2008/07/30 15:38:51  rubikitch
;; *** empty log message ***
;;
;; Revision 1.7  2008/07/30 15:21:48  rubikitch
;; `anything-scroll-other-window', `anything-scroll-other-window-down':
;; Scroll other window (for persistent action).
;;
;; Revision 1.6  2008/07/30 15:12:36  rubikitch
;; *** empty log message ***
;;
;; Revision 1.5  2008/07/30 15:06:32  rubikitch
;; `anything-select-2nd-action', `anything-select-3rd-action', `anything-select-4th-action':
;; Select other than default action without pressing Tab.
;;
;; Revision 1.4  2008/07/30 14:58:27  rubikitch
;; `anything-current-buffer': Store current buffer when `anything' is invoked.
;; `anything-current-position': Restore position when keyboard-quitted.
;;
;; Revision 1.3  2008/07/30 14:38:04  rubikitch
;; Implemented persistent action.
;;
;; Revision 1.2  2008/07/30 13:37:16  rubikitch
;; Update doc.
;;
;; Revision 1.1  2008/07/30 13:22:06  rubikitch
;; New maintainer.
;;

(require 'cl)

;; User Configuration 

;; This is only an example. Customize it to your own taste!
(defvar anything-sources `(((name . "Buffers")
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

                           ((name . "Manual Pages")
                            (candidates . ,(progn
                                             ;; XEmacs doesn't have a woman :)
                                             (condition-case nil
                                                 (progn
                                                   (require 'woman)
                                                   (woman-file-name "")
                                                   (sort (mapcar 'car
                                                                 woman-topic-all-completions)
                                                         'string-lessp))
                                               (error nil))))
                            (action . (("Open Manual Page" . woman)))
                            (requires-pattern . 2))

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

Attributes:

- name (mandatory)

  The name of the source. It is also the heading which appears
  above the list of matches from the source. Must be unique.

- candidates (mandatory if candidates-in-buffer attribute is not provided)

  Specifies how to retrieve candidates from the source. It can
  either be a variable name, a function called with no parameters
  or the actual list of candidates.

  The list must be a list of strings, so it's the responsibility
  of the source to convert candidates to strings if necessary.

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
  `anything-sources'.

- action (mandatory if type attribute is not provided)

  It is a list of (DISPLAY . FUNCTION) pairs. FUNCTION is called
  with one parameter: the selected candidate.

  An action other than the default can be chosen from this list
  of actions for the currently selected candidate (by default
  with TAB). The DISPLAY string is shown in the completions
  buffer and the FUNCTION is invoked when an action is
  selected. The first action of the list is the default.

- type (optional if action attribute is provided)

  Indicates the type of the items the source returns. 

  Merge attributes not specified in the source itself from
  `anything-type-attributes'.

- init (optional)

  Function called with no parameters when anything is started. It
  is useful for collecting current state information which can be
  used to create the list of candidates later.

  For example, if a source needs to work with the current
  directory then it can store its value here, because later
  anything does its job in the minibuffer and in the
  `anything-buffer' and the current directory can be different
  there.

- match (optional)

  List of functions called with one parameter: a candidate. The
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
  themselves.

- candidate-transformer (optional)

  It's a function called with one argument when the completion
  list from the source is built. The argument is the list of
  candidates retrieved from the source. The function should
  return a transformed list of candidates which will be used for
  the actual completion.

  This can be used to transform or remove items from the list of
  candidates.

  The function can also substitute candidates in the returned
  list with (DISPLAY . REAL) pairs. In this case the DISPLAY
  string is shown in the Anything buffer, but the REAL one is
  used as action argument when the candidate is selected. This
  allows a more readable presentation for candidates which would
  otherwise be, for example, too long or have a common part
  shared with other candidates which can be safely replaced with
  an abbreviated string for display purposes.

  Note that if the (DISPLAY . REAL) form is used then pattern
  matching is done on the displayed string, not on the real
  value.

- filtered-candidate-transformer (optional)

  It has the same format as `candidate-transformer', except the
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

  Note that `candidate-transformer' is run already, so the given
  transformer function should also be able to handle candidates
  with (DISPLAY . REAL) format.

  This option has no effect for asynchronous sources. (Not yet,
  at least.

- action-transformer (optional)

  It's a function called with two arguments when the action list
  from the source is assembled. The first argument is the list of
  actions, the second is the current selection.

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate.

- delayed (optional)

  Candidates from the source are shown only if the user stops
  typing and is idle for `anything-idle-delay' seconds.

- volatile (optional)

  Indicates the source assembles the candidate list dynamically,
  so it shouldn't be cached within a single Anything
  invocation. It is only applicable to synchronous sources,
  because asynchronous sources are not cached.

- requires-pattern (optional)

  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates.

- persistent-action (optional)

  Function called with one parameter; the selected candidate.

  An action performed by `anything-execute-persistent-action'.
  If none, use the default action.

- candidates-in-buffer (optional)

  Shortcut attribute for making and narrowing candidates using
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

- search (optional)

  Function like `re-search-forward' or `search-forward'.
  Buffer search function used by `anything-candidates-in-buffer'.
  By default, `anything-candidates-in-buffer' uses `re-search-forward'.
  This attribute is meant to be used with
  (candidates . anything-candidates-in-buffer) or
  (candidates-in-buffer) in short.

- display-to-real (optional)

  Function called with one parameter; the selected candidate.

  The function transforms the selected candidate, and the result
  is passed to the action function.  The display-to-real
  attribute provides another way to pass other string than one
  shown in Anything buffer.

  Traditionally, it is possible to make candidates,
  candidate-transformer or filtered-candidate-transformer
  function return a list with (DISPLAY . REAL) pairs. But if REAL
  can be generated from DISPLAY, display-to-real is more
  convenient and faster.

- cleanup (optional)

  Function called with no parameters when *anything* buffer is closed. It
  is useful for killing unneeded candidates buffer.
")


;; This value is only provided as an example. Customize it to your own
;; taste!
(defvar anything-type-attributes
  '((file (action . (("Find File" . find-file)
                     ("Delete File" . (lambda (file)
                                        (if (y-or-n-p (format "Really delete file %s? "
                                                              file))
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


(defvar anything-enable-digit-shortcuts nil
  "*If t then the first nine matches can be selected using
  Ctrl+<number>.")


(defvar anything-candidate-number-limit 50
  "*Do not show more candidates than this limit from inidividual
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
    (define-key map (kbd "<prior>") 'anything-previous-page)
    (define-key map (kbd "<next>") 'anything-next-page)
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
    (define-key map (kbd "C-i") 'anything-select-action)
    ;; the defalias is needed because commands are bound by name when
    ;; using iswitchb, so only commands having the prefix anything-
    ;; get rebound
    (defalias 'anything-previous-history-element 'previous-history-element)
    ;; C-p is used instead of M-p, because anything uses ESC
    ;; (currently hardcoded) for `anything-iswitchb-cancel-anything' and
    ;; Emacs handles ESC and Meta as synonyms, so ESC overrides
    ;; other commands with Meta prefix.
    ;;
    ;; Note that iswitchb uses M-p and M-n by default for history
    ;; navigation, so you should bind C-p and C-n in
    ;; `iswitchb-mode-map' if you use the history keys and don't want
    ;; to use different keys for iswitchb while anything is not yet
    ;; kicked in. These keys are not bound automatically by anything
    ;; in `iswitchb-mode-map' because they (C-n at least) already have
    ;; a standard iswitchb binding which you might be accustomed to.
    (define-key map (kbd "C-p") 'anything-previous-history-element)
    (defalias 'anything-next-history-element 'next-history-element)
    (define-key map (kbd "C-n") 'anything-next-history-element)
    ;; Binding M-s is used instead of C-s, because C-s has a binding in
    ;; iswitchb.  You can rebind it, of course.
    (define-key map (kbd "M-s") 'anything-isearch)
    ;; unbind C-r to prevent problems during anything-isearch
    (define-key map (kbd "C-r") nil)
    map)
  "Keymap for anything.")


(defvar anything-isearch-map
  (let ((map (copy-keymap (current-global-map))))
    (define-key map (kbd "<return>") 'anything-isearch-default-action)
    (define-key map (kbd "C-i") 'anything-isearch-select-action)
    (define-key map (kbd "C-g") 'anything-isearch-cancel)
    (define-key map (kbd "M-s") 'anything-isearch-again)
    (define-key map (kbd "<backspace>") 'anything-isearch-delete)
    ;; add printing chars
    (let ((i 32))
      (while (< i 256)
        (define-key map (vector i) 'anything-isearch-printing-char)
        (setq i (1+ i))))
    map)
  "Keymap for anything incremental search.")


(defgroup anything nil
  "Open anything." :prefix "anything-" :group 'convenience)

(if (facep 'header-line)
    (copy-face 'header-line 'anything-header)
  
  (defface anything-header 
    '((t (:bold t :underline t))) 
    "Face for header lines in the anything buffer." :group 'anything))

(defvar anything-header-face 'anything-header
  "Face for header lines in the anything buffer.")

(defface anything-isearch-match '((t (:background "Yellow")))
  "Face for isearch in the anything buffer." :group 'anything)

(defvar anything-isearch-match-face 'anything-isearch-match
  "Face for matches during incremental search.")

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
;; Public functions
;;----------------------------------------------------------------------
;;
;; These functions are the public API of Anything. See their
;; documentation for more information.
;;
;; anything 
;; anything-iswitchb-setup
;;
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

;;----------------------------------------------------------------------

(defconst anything-buffer "*anything*"
  "Buffer showing completions.")

(defvar anything-selection-overlay nil
  "Overlay used to highlight the currently selected file.")

(defvar anything-isearch-overlay nil
  "Overlay used to highlight the current match during isearch.")

(defvar anything-digit-overlays nil
  "Overlays for digit shortcuts. See `anything-enable-digit-shortcuts'.")

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

(defvar anything-update-hook nil
  "Run after the aything buffer was updated according the new
  input pattern.")

(defvar anything-saved-sources nil
  "Saved value of the original `anything-sources' when the action
  list is shown.")

(defvar anything-saved-selection nil
  "Saved value of the currently selected object when the action
  list is shown.")

(defvar anything-original-source-filter nil
  "Original value of `anything-source-filter' before Anything was started.")

(defvar anything-current-buffer nil
  "Current buffer when `anything' is invoked.")

(defvar anything-buffer-file-name nil
  "`buffer-file-name' when `anything' is invoked.")

(defvar anything-current-position nil
  "Cons of (point) and (window-start) when `anything' is invoked.
It is needed because restoring position when `anything' is keyboard-quitted.")

(defvar anything-saved-action nil
  "Saved value of the currently selected action by key.")

(defvar anything-last-sources nil
  "Sources of previously invoked `anything'.")

(defvar anything-saved-current-source nil
  "Saved value of the original (anything-get-current-source) when the action
  list is shown.")

(defvar anything-compiled-sources nil
  "Compiled version of `anything-sources'.
If you change `anything-sources' dynamically, set this variables to nil.")

(put 'anything 'timid-completion 'disabled)

;; internal variables
(defvar anything-test-candidate-list nil)
(defvar anything-test-mode nil)
(defvar anything-buffer-chars-modified-tick 0)
(make-variable-buffer-local 'anything-buffer-chars-modified-tick)
(defvar anything-source-name nil)
(defvar anything-candidates-buffer-alist nil)

(defmacro anything-aif (test-form then-form &optional else-form)
  "Anaphoric if."
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))  
(put 'anything-aif 'lisp-indent-function 2)

(defun anything-check-minibuffer-input ()
  "Extract input string from the minibuffer and check if it needs
to be handled."
   (with-selected-window (minibuffer-window)
     (anything-check-new-input (minibuffer-contents))))


(defun anything-check-new-input (input)
  "Check input string and update the anything buffer if
necessary."
  (unless (equal input anything-pattern)
    (setq anything-pattern input)
    (unless anything-saved-sources
      (setq anything-input anything-pattern))
    (anything-update)))


(defun anything-update ()
  "Update the list of matches in the anything buffer according to
the current pattern."
  (setq anything-digit-shortcut-count 0)
  (anything-kill-async-processes)
  (with-current-buffer anything-buffer
    (erase-buffer)

    (if anything-enable-digit-shortcuts
        (dolist (overlay anything-digit-overlays)
          (delete-overlay overlay)))

    (let (delayed-sources)
      (dolist (source (anything-get-sources))
        (if (or (not anything-source-filter)
                (member (assoc-default 'name source) anything-source-filter))
          (if (equal anything-pattern "")
              (unless (assoc 'requires-pattern source)
                (if (assoc 'delayed source)
                    (push source delayed-sources)
                  (anything-process-source source)))

            (let ((min-pattern-length (assoc-default 'requires-pattern source)))
              (unless (and min-pattern-length
                           (< (length anything-pattern) min-pattern-length))
                (if (assoc 'delayed source)
                    (push source delayed-sources)
                  (anything-process-source source)))))))

      (goto-char (point-min))
      (run-hooks 'anything-update-hook)
      (anything-next-line)


      (if anything-test-mode
          (dolist (source delayed-sources)
            (anything-process-source source))
        (anything-maybe-fit-frame)
        (run-with-idle-timer (if (featurep 'xemacs)
                                 0.1
                               0)
                             nil
                             'anything-process-delayed-sources
                             delayed-sources)))))

(defvar anything-compile-source-functions
  '(anything-compile-source--type anything-compile-source--candidates-in-buffer)
  "Functions to compile elements of `anything-sources'.")
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
   (anything-saved-sources
    anything-sources)
   ;; memoized
   (anything-compiled-sources)
   ;; first time
   (t
    (setq anything-compiled-sources
          (mapcar
           (lambda (source)
             (loop with source = (if (listp source) source (symbol-value source))
                   for f in anything-compile-source-functions
                   do (setq source (funcall f source))
                   finally (return source)))
           anything-sources)))))

(defun anything-compile-source--type (source)
  (anything-aif (assoc-default 'type source)
      (append source (assoc-default it anything-type-attributes) nil)
    source))
    
(defun anything-compile-source--candidates-in-buffer (source)
  (anything-aif (assoc 'candidates-in-buffer source)
      (append source `((candidates . ,(or (cdr it) 'anything-candidates-in-buffer))
                       (volatile) (match identity)))
    source))
  

(defun anything-compute-matches (source)
  "Compute matches from SOURCE according to its settings."
  (let (matches)
    (if (equal anything-pattern "")
        (progn
          (setq matches (anything-get-cached-candidates source))
          (if (> (length matches) anything-candidate-number-limit)
              (setq matches 
                    (subseq matches 0 anything-candidate-number-limit))))

      (condition-case nil
          (let ((item-count 0)
                (functions (assoc-default 'match source))
                exit)

            (unless functions
              (setq functions
                    (list (lambda (candidate)
                            (string-match anything-pattern candidate)))))

            (dolist (function functions)
              (let (newmatches)
                (dolist (candidate (anything-get-cached-candidates source))
                  (when (and (not (member candidate matches))
                             (funcall function (if (listp candidate)
                                                   (car candidate)
                                                 candidate)))
                    (push candidate newmatches)

                    (when anything-candidate-number-limit
                      (incf item-count)
                      (when (= item-count anything-candidate-number-limit)
                        (setq exit t)
                        (return)))))

                (setq matches (append matches (reverse newmatches)))

                (if exit
                    (return)))))

        (invalid-regexp (setq matches nil))))

    (anything-aif (assoc-default 'filtered-candidate-transformer source)
        (setq matches
              (let ((anything-source-name (assoc-default 'name source)))
                (funcall it matches source))))
    matches))

(defun anything-process-source (source)
  "Display matches from SOURCE according to its settings."
  (let ((matches (anything-compute-matches source)))
    (when matches
      (when anything-test-mode
          (setq anything-test-candidate-list
                `(,@anything-test-candidate-list
                  (,(assoc-default 'name source)
                   ,matches))))
      (anything-insert-header (assoc-default 'name source))
      (dolist (match matches)
        (when (and anything-enable-digit-shortcuts
                   (not (eq anything-digit-shortcut-count 9)))
          (move-overlay (nth anything-digit-shortcut-count
                             anything-digit-overlays)
                        (line-beginning-position)
                        (line-beginning-position))
          (incf anything-digit-shortcut-count))

        (anything-insert-match match 'insert)))))

(defun anything-insert-match (match insert-function)
  "Insert MATCH into the anything buffer. If MATCH is a list then
insert the string inteneded to appear on the display and store
the real value in a text property."
  (if (not (listp match))
      (funcall insert-function match)

    (funcall insert-function (car match))
    (put-text-property (line-beginning-position) (line-end-position) 
                       'anything-realvalue (cdr match)))
  (funcall insert-function "\n"))


(defun anything-process-delayed-sources (delayed-sources)
  "Process delayed sources if the user is idle for
`anything-idle-delay' seconds."
  (if (sit-for anything-idle-delay)
      (with-current-buffer anything-buffer        
        (save-excursion
          (goto-char (point-max))
          (dolist (source delayed-sources)
            (anything-process-source source))

          (when (and (not (equal (buffer-size) 0))
                     ;; no selection yet
                     (= (overlay-start anything-selection-overlay)
                        (overlay-end anything-selection-overlay)))
            (goto-char (point-min))
            (run-hooks 'anything-update-hook)
            (anything-next-line)))

        (anything-maybe-fit-frame))))


(defun anything (&optional sources input prompt resume)
  "Select anything."
  ;; TODO more document
  (interactive)
  (condition-case v
      (let ((frameconfig (current-frame-configuration))
            (anything-sources (cond ((and sources (symbolp sources)) (list sources))
                                    (sources)
                                    (t anything-sources))))
        (add-hook 'post-command-hook 'anything-check-minibuffer-input)

        (unless resume (anything-initialize))

        (if anything-samewindow
            (switch-to-buffer anything-buffer)
          (pop-to-buffer anything-buffer))

        (unwind-protect
            (progn
              (unless resume (anything-update))
              (select-frame-set-input-focus (window-frame (minibuffer-window)))
              (let ((minibuffer-local-map anything-map))
                (read-string (or prompt "pattern: ") (if resume anything-pattern input))))

          (anything-cleanup)
          (remove-hook 'post-command-hook 'anything-check-minibuffer-input)
          (set-frame-configuration frameconfig))
        (anything-execute-selection-action))
    (quit
     (goto-char (car anything-current-position))
     (set-window-start (selected-window) (cdr anything-current-position)))))

(defun anything-resume ()
  "Resurrect previously invoked `anything'."
  (interactive)
  (anything (or anything-last-sources anything-sources) nil nil t))

(defun anything-execute-selection-action (&optional selection action clear-saved-action display-to-real)
  "If a candidate was selected then perform the associated
action."
  (setq selection (or selection
                      (if anything-saved-sources
                          ;; the action list is shown
                          anything-saved-selection
                        (anything-get-selection))))
  (setq action (or action
                   anything-saved-action
                   (if anything-saved-sources
                       ;; the action list is shown
                       (anything-get-selection)
                     (anything-get-action))))
  (setq display-to-real
        (or display-to-real
            (assoc-default 'display-to-real
                           (or anything-saved-current-source
                               (anything-get-current-source)))
            #'identity))
  (if (and (listp action)
           (not (functionp action)))    ; lambda
      ;;select the default action
      (setq action (cdar action)))
  (unless clear-saved-action (setq anything-saved-action nil))
  (if (and selection action)
      (funcall action (funcall display-to-real selection))))


(defun anything-get-selection ()
  "Return the currently selected item or nil."
  (unless (= (buffer-size (get-buffer anything-buffer)) 0)
    (with-current-buffer anything-buffer
      (let ((selection
             (or (get-text-property (overlay-start
                                     anything-selection-overlay)
                                    'anything-realvalue)
                 (buffer-substring-no-properties
                  (overlay-start anything-selection-overlay)
                  (1- (overlay-end anything-selection-overlay))))))
        (unless (equal selection "")
          selection)))))


(defun anything-get-action ()
  "Return the associated action for the selected candidate."
  (unless (= (buffer-size (get-buffer anything-buffer)) 0)
    (let* ((source (anything-get-current-source))
           (actions (assoc-default 'action source)))

      (anything-aif (assoc-default 'action-transformer source)
          (funcall it actions (anything-get-selection))
        actions))))

(defun anything-select-action ()
  "Select an action for the currently selected candidate."
  (interactive)
  (if anything-saved-sources
      (error "Already showing the action list"))

  (setq anything-saved-selection (anything-get-selection))
  (unless anything-saved-selection
    (error "Nothing is selected."))
  (setq anything-saved-current-source (anything-get-current-source))
  (let ((actions (anything-get-action)))
    (setq anything-source-filter nil)
    (setq anything-saved-sources anything-sources)
    (setq anything-sources `(((name . "Actions")
                              (candidates . ,actions))))
    (with-selected-window (minibuffer-window)
      (delete-minibuffer-contents))
    (setq anything-pattern 'dummy)      ; so that it differs from the
                                        ; previous one
    (anything-check-minibuffer-input)))

(defun anything-select-nth-action (n)
  "Select the nth action for the currently selected candidate."
  (setq anything-saved-selection (anything-get-selection))
  (unless anything-saved-selection
    (error "Nothing is selected."))
  (setq anything-saved-action (cdr (elt (anything-get-action) n)))
  (anything-exit-minibuffer))

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

(defun anything-funcall-foreach (sym)
  "Call the sym function for each source if any."
  (dolist (source (anything-get-sources))
    (anything-aif (assoc-default sym source)
        (let ((anything-source-name (assoc-default 'name source)))
          (funcall it)))))

(defun anything-initialize ()
  "Initialize anything settings and set up the anything buffer."
  (setq anything-current-buffer (current-buffer))
  (setq anything-buffer-file-name buffer-file-name)
  (setq anything-current-position (cons (point) (window-start)))
  (setq anything-compiled-sources nil)
  (setq anything-saved-sources nil)
  (setq anything-saved-current-source nil)
  ;; Call the init function for sources where appropriate
  (anything-funcall-foreach 'init)

  (setq anything-pattern "")
  (setq anything-input "")
  (setq anything-candidate-cache nil)
  (setq anything-original-source-filter anything-source-filter)
  (setq anything-last-sources anything-sources)

  (with-current-buffer (get-buffer-create anything-buffer)
    (setq cursor-type nil)
    (setq mode-name "Anything"))

  (if anything-selection-overlay
      ;; make sure the overlay belongs to the anything buffer if
      ;; it's newly created
      (move-overlay anything-selection-overlay (point-min) (point-min)
                    (get-buffer anything-buffer))

    (setq anything-selection-overlay 
          (make-overlay (point-min) (point-min) (get-buffer anything-buffer)))
    (overlay-put anything-selection-overlay 'face 'highlight))

  (if anything-enable-digit-shortcuts
      (unless anything-digit-overlays
        (dotimes (i 9)
          (push (make-overlay (point-min) (point-min)
                              (get-buffer anything-buffer))
                anything-digit-overlays)
          (overlay-put (car anything-digit-overlays)
                       'before-string (concat (int-to-string (1+ i)) " - ")))
        (setq anything-digit-overlays (nreverse anything-digit-overlays)))

    (when anything-digit-overlays
      (dolist (overlay anything-digit-overlays)
        (delete-overlay overlay))
      (setq anything-digit-overlays nil))))


(defun anything-cleanup ()
  "Clean up the mess."
  (setq anything-source-filter anything-original-source-filter)
  (if anything-saved-sources
      (setq anything-sources anything-saved-sources))
  (with-current-buffer anything-buffer
    (setq cursor-type t))
  (with-current-buffer anything-current-buffer
    (setq anything-buffer-chars-modified-tick (buffer-chars-modified-tick)))
  (bury-buffer anything-buffer)
  (anything-funcall-foreach 'cleanup)
  (anything-kill-async-processes))


(defun anything-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (anything-move-selection 'line 'previous))


(defun anything-next-line ()
  "Move selection to the next line."
  (interactive)
  (anything-move-selection 'line 'next))


(defun anything-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (anything-move-selection 'page 'previous))


(defun anything-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (anything-move-selection 'page 'next))


(defun anything-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (anything-move-selection 'source 'previous))


(defun anything-next-source ()
  "Move selection to the next source."
  (interactive)
  (anything-move-selection 'source 'next))


(defun anything-move-selection (unit direction)
  "Move the selection marker to a new position determined by
UNIT and DIRECTION."
  (unless (or (= (buffer-size (get-buffer anything-buffer)) 0)
              (not (get-buffer-window anything-buffer 'visible)))
    (save-selected-window
      (select-window (get-buffer-window anything-buffer 'visible))

      (case unit
        (line (forward-line (case direction
                              (next 1)
                              (previous -1)
                              (t (error "Invalid direction.")))))

        (page (case direction
                (next (condition-case nil
                          (scroll-up)
                        (end-of-buffer (goto-char (point-max)))))
                (previous (condition-case nil
                              (scroll-down)
                            (beginning-of-buffer (goto-char (point-min)))))
                (t (error "Invalid direction."))))

        (source (case direction
                   (next (goto-char (or (anything-get-next-header-pos)
                                        (point-min))))
                   (previous (progn
                               (forward-line -1)
                               (if (bobp)
                                   (goto-char (point-max))
                                 (if (anything-pos-header-line-p)
                                     (forward-line -1)
                                   (forward-line 1)))
                               (goto-char (anything-get-previous-header-pos))
                               (forward-line 1)))
                   (t (error "Invalid direction."))))

        (t (error "Invalid unit.")))

      (while (and (not (bobp)) (anything-pos-header-line-p))
        (forward-line (if (and (eq direction 'previous)
                               (not (eq (line-beginning-position)
                                        (point-min))))
                          -1
                        1)))
      (if (bobp)
          (forward-line 1))
      (if (eobp)
          (forward-line -1))

      (anything-mark-current-line))))


(defun anything-mark-current-line ()
  "Move selection overlay to current line."
  (move-overlay anything-selection-overlay
                (line-beginning-position)
                (1+ (line-end-position))))


(defun anything-select-with-digit-shortcut ()
  (interactive)
  (if anything-enable-digit-shortcuts
      (let* ((index (- (event-basic-type (elt (this-command-keys-vector) 0)) ?1))
             (overlay (nth index anything-digit-overlays)))
        (if (overlay-buffer overlay)
            (save-selected-window
              (select-window (get-buffer-window anything-buffer 'visible))          
              (goto-char (overlay-start overlay))
              (anything-mark-current-line)
              (anything-exit-minibuffer))))))


(defun anything-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (interactive)
  (setq anything-iswitchb-candidate-selected (anything-get-selection))
  (exit-minibuffer))


(defun anything-get-current-source ()
  "Return the source for the current selection."
  (with-current-buffer anything-buffer
      ;; This goto-char shouldn't be necessary, but point is moved to
      ;; point-min somewhere else which shouldn't happen.
      (goto-char (overlay-start anything-selection-overlay))
      (let* ((header-pos (anything-get-previous-header-pos))
             (source-name
              (save-excursion
                (assert header-pos)
                (goto-char header-pos)
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))))
        (some (lambda (source)
                (if (equal (assoc-default 'name source)
                           source-name)
                    source))
              (anything-get-sources)))))


(defun anything-get-next-header-pos ()
  "Return the position of the next header from point."
  (next-single-property-change (point) 'anything-header))


(defun anything-get-previous-header-pos ()
  "Return the position of the previous header from point"
  (previous-single-property-change (point) 'anything-header))


(defun anything-pos-header-line-p ()
  "Return t if the current line is a header line."
  (or (get-text-property (line-beginning-position) 'anything-header)
      (get-text-property (line-beginning-position) 'anything-header-separator)))


(defun anything-get-candidates (source)
  "Retrieve and return the list of candidates from
SOURCE."
  (let* ((candidate-source (assoc-default 'candidates source))
         (candidates
          (if (functionp candidate-source)
              (let ((anything-source-name (assoc-default 'name source)))
                (funcall candidate-source))
            (if (listp candidate-source)
                candidate-source
              (if (and (symbolp candidate-source)
                       (boundp candidate-source))
                  (symbol-value candidate-source)
                (error (concat "Candidates must either be a function, "
                               " a variable or a list: %s")
                       candidate-source))))))
    (if (processp candidates)
        candidates
      (anything-transform-candidates candidates source))))
         

(defun anything-transform-candidates (candidates source)
  "Transform CANDIDATES according to candidate transformers."
  (anything-aif (assoc-default 'candidate-transformer source)
      (let ((anything-source-name (assoc-default 'name source)))
        (funcall it candidates))
    candidates))


(defun anything-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is not yet a cached value."
  (let* ((name (assoc-default 'name source))
         (candidate-cache (assoc name anything-candidate-cache))
         candidates)

    (if candidate-cache
        (setq candidates (cdr candidate-cache))

      (setq candidates (anything-get-candidates source))

      (if (processp candidates)
          (progn
            (push (cons candidates
                        (append source 
                                (list (cons 'item-count 0)
                                      (cons 'incomplete-line ""))))
                  anything-async-processes)
            (set-process-filter candidates 'anything-output-filter)
            (setq candidates nil))

        (unless (assoc 'volatile source)
          (setq candidate-cache (cons name candidates))
          (push candidate-cache anything-candidate-cache))))

    candidates))

(defun* anything-candidates-in-buffer (&optional (get-line-fn 'buffer-substring-no-properties))
  "Get candidates from the candidates buffer according to `anything-pattern'.

BUFFER is `anything-candidates-buffer' by default.  Each
candidate must be placed in one line.  This function is meant to
be used in candidates-in-buffer or candidates attribute of an
anything source.  Especially fast for many (1000+) candidates.

eg.
 '((name . \"many files\")
   (init . (lambda () (with-current-buffer (anything-candidates-buffer 'local)
                        (insert-many-filenames))))
   (search . re-search-forward)  ; optional
   (candidates-in-buffer)
   (type . file))

GET-LINE-FN (default: buffer-substring-no-properties) specifies a function
called with two arguments:point of line-beginning and point of line-end. 
This function creates a candidate.

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
by `anything-candidates-buffer'.  Candidates are stored in a line.

The candidates function narrows all candidates, IOW creates a
subset of candidates dynamically. It is the task of
`anything-candidates-in-buffer'.  As long as
`anything-candidates-buffer' is used,`(candidates-in-buffer)' is
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

See also `anything-sources' docstring.

"
  (anything-candidates-in-buffer-1 (anything-candidates-buffer)
                                   anything-pattern get-line-fn
                                   ;; use external variable `source'.
                                   (or (assoc-default 'search source)
                                       #'re-search-forward)))

(defun* anything-candidates-in-buffer-1 (buffer &optional (pattern anything-pattern) (get-line-fn 'buffer-substring-no-properties) (search-fn 're-search-forward))
  ;; buffer == nil when candidates buffer does not exist.
  (when buffer
    (with-current-buffer buffer
      (goto-char (point-min))
      (loop while (funcall search-fn pattern nil t)
            for i from 1 to anything-candidate-number-limit
            unless (eobp)
            collecting (funcall get-line-fn (point-at-bol) (point-at-eol))
            do (forward-line 1)))))

(defun anything-current-buffer-is-modified ()
  "Return non-nil when `anything-current-buffer' is modified since `anything' was invoked."
  (with-current-buffer anything-current-buffer
    (/= anything-buffer-chars-modified-tick (buffer-chars-modified-tick))))

(defun anything-candidates-buffer (&optional create-or-buffer)
  "Register and return a buffer containing candidates of current source.
`anything-candidates-buffer' searches buffer-local candidates buffer first,
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
  Create a new global candidates buffer,
  named \" *anything candidates:SOURCE*ANYTHING-CURRENT-BUFFER\".
"
  (let* ((gbufname (format " *anything candidates:%s*" anything-source-name))
         (lbufname (concat gbufname (buffer-name anything-current-buffer)))
         buf)
    (when create-or-buffer
      (if (bufferp create-or-buffer)
          (add-to-list 'anything-candidates-buffer-alist
                       (cons anything-source-name create-or-buffer))
        (with-current-buffer
            (get-buffer-create (if (eq create-or-buffer 'global) gbufname lbufname))
          (buffer-disable-undo)
          (erase-buffer)
          (font-lock-mode -1))))
    (or (get-buffer lbufname)
        (get-buffer gbufname)
        (assoc-default anything-source-name anything-candidates-buffer-alist))))

(defun anything-output-filter (process string)
  "Process output from PROCESS."
  (let* ((process-assoc (assoc process anything-async-processes))
         (process-info (cdr process-assoc))
         (insertion-marker (assoc-default 'insertion-marker process-info))
         (incomplete-line-info (assoc 'incomplete-line process-info))
         (item-count-info (assoc 'item-count process-info)))

    (with-current-buffer anything-buffer
      (save-excursion
        (if insertion-marker
            (goto-char insertion-marker)
        
          (goto-char (point-max))
          (anything-insert-header (assoc-default 'name process-info))
          (setcdr process-assoc
                  (append process-info `((insertion-marker . ,(point-marker))))))

        (let ((lines (split-string string "\n"))
              candidates)
          (while lines
            (if (not (cdr lines))
                ;; store last incomplete line until new output arrives
                (setcdr incomplete-line-info (car lines))

              (if (cdr incomplete-line-info)
                  (progn
                    (push (concat (cdr incomplete-line-info) (car lines))
                          candidates)
                    (setcdr incomplete-line-info nil))

              (push (car lines) candidates)))
                  
            (pop lines))

          (setq candidates (reverse candidates))
          (dolist (candidate (anything-transform-candidates candidates process-info))
            (anything-insert-match candidate 'insert-before-markers)
            (incf (cdr item-count-info))
            (when (>= (cdr item-count-info) anything-candidate-number-limit)
              (anything-kill-async-process process)
              (return)))))

      (anything-maybe-fit-frame)

      (run-hooks 'anything-update-hook)

      (if (bobp)
          (anything-next-line)

        (save-selected-window
          (select-window (get-buffer-window anything-buffer 'visible))
          (anything-mark-current-line))))))


(defun anything-kill-async-processes ()
  "Kill all known asynchronous processes according to
`anything-async-processes'."
    "Kill locate process."
    (dolist (process-info anything-async-processes)
      (anything-kill-async-process (car process-info)))
    (setq anything-async-processes nil))


(defun anything-kill-async-process (process)
  "Kill PROCESS and detach the associated functions."
  (set-process-filter process nil)
  (delete-process process))
  

(defun anything-insert-header (name)
  "Insert header of source NAME into the anything buffer."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (put-text-property start (point) 'anything-header-separator t)))

  (let ((start (point)))
    (insert name)
    (put-text-property (line-beginning-position)
                       (line-end-position) 'anything-header t)
    (insert "\n")
    (put-text-property start (point) 'face anything-header-face)))


(defun anything-set-source-filter (sources)
  "Sets the value of `anything-source-filter' and updates the list of results."
  (setq anything-source-filter sources)
  (anything-update))


(defun anything-maybe-fit-frame ()
   "Fit anything frame to its buffer, and put it at top right of display.
 To inhibit fitting, set `fit-frame-inhibit-fitting-flag' to t.
 You can set user options `fit-frame-max-width-percent' and
 `fit-frame-max-height-percent' to control max frame size."
   (when (and (require 'fit-frame nil t)
              (boundp 'fit-frame-inhibit-fitting-flag)
              (not fit-frame-inhibit-fitting-flag)
              (get-buffer-window anything-buffer 'visible))
     (with-selected-window (get-buffer-window anything-buffer 'visible)
       (fit-frame nil nil nil t)
       (modify-frame-parameters
        (selected-frame)
        `((left . ,(- (x-display-pixel-width) (+ (frame-pixel-width) 7)))
          (top . 0)))))) ; The (top . 0) shouldn't be necessary (Emacs bug).

;;---------------------------------------------------------------------
;; Persistent Action
;;----------------------------------------------------------------------
(defun anything-execute-persistent-action ()
  "If a candidate is selected then perform the associated action without quitting anything."
  (interactive)
  (save-selected-window
    (select-window (get-buffer-window anything-buffer))
    (select-window (setq minibuffer-scroll-window
                         (if (one-window-p t) (split-window)
                           (next-window (selected-window) 1))))
    (anything-execute-selection-action
     nil
     (or (assoc-default 'persistent-action (anything-get-current-source))
         (anything-get-action))
     t)))

;; scroll-other-window(-down)? for persistent-action
(defun anything-scroll-other-window-base (command)
  (save-selected-window
    (other-window 2)
    (call-interactively command)))

(defun anything-scroll-other-window ()
  "Scroll other window (not *Anything* window) upward."
  (interactive)
  (anything-scroll-other-window-base 'scroll-other-window))
(defun anything-scroll-other-window-down ()
  "Scroll other window (not *Anything* window) downward."
  (interactive)
  (anything-scroll-other-window-base 'scroll-other-window-down))

;;---------------------------------------------------------------------
;; Incremental search within results
;;----------------------------------------------------------------------

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

(defvar anything-isearch-original-post-command-hook nil
  "Original value of post-command-hook before isearch is started.")

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
  "Start incremental search within results."
  (interactive)
  (if (eq (buffer-size (get-buffer anything-buffer)) 0)
      (message "There are no results.")

    (setq anything-isearch-original-message-timeout minibuffer-message-timeout)
    (setq minibuffer-message-timeout nil)

    (setq anything-isearch-original-global-map global-map)

    (condition-case nil
        (progn
          (setq anything-isearch-original-window (selected-window))
          (select-window (get-buffer-window anything-buffer 'visible))
          (setq cursor-type t)

          (setq anything-isearch-original-post-command-hook
                (default-value 'post-command-hook))
          (setq-default post-command-hook nil)
          (add-hook 'post-command-hook 'anything-isearch-post-command)

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
                            (get-buffer anything-buffer))

            (setq anything-isearch-overlay (make-overlay (point-min) (point-min)))
            (overlay-put anything-isearch-overlay 'face anything-isearch-match-face))

          (setq anything-isearch-message-suffix
                (substitute-command-keys "cancel with \\[anything-isearch-cancel]")))

      (error (anything-isearch-cleanup)))))


(defun anything-isearch-post-command ()
  "Print the current pattern after every command."
  (anything-isearch-message)
  (when (get-buffer-window anything-buffer 'visible)
    (with-selected-window (get-buffer-window anything-buffer 'visible)
      (move-overlay anything-isearch-overlay anything-isearch-match-start (point)
                    (get-buffer anything-buffer)))))


(defun anything-isearch-printing-char ()
  "Add printing char to the pattern."
  (interactive)
  (let ((char (char-to-string last-command-char)))
    (setq anything-isearch-pattern (concat anything-isearch-pattern char))

    (with-selected-window (get-buffer-window anything-buffer 'visible)
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
            (setq anything-isearch-match-start (copy-marker (match-beginning 0))))))
  
      (anything-mark-current-line))))


(defun anything-isearch-again ()
  "Search again for the current pattern"
  (interactive)
  (if (equal anything-isearch-pattern "")
      (setq anything-isearch-message-suffix "no pattern yet")

    (with-selected-window (get-buffer-window anything-buffer 'visible)
      (let ((start (point)))
        (while (and (re-search-forward anything-isearch-pattern nil t)
                    (anything-pos-header-line-p)))
        (if (or (anything-pos-header-line-p)
                (eq start (point)))
            (progn
              (goto-char start)
              (unless (eq 'error (plist-get (car anything-isearch-match-positions)
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

      (with-selected-window (get-buffer-window anything-buffer 'visible)      
        (goto-char (plist-get last 'pos))
        (setq anything-isearch-match-start (plist-get last 'start))
        (anything-mark-current-line)))))


(defun anything-isearch-default-action ()
  "Execute the default action for the selected candidate."
  (interactive)
  (anything-isearch-cleanup)
  (with-current-buffer anything-buffer (anything-exit-minibuffer)))


(defun anything-isearch-select-action ()
  "Choose an action for the selected candidate."
  (interactive)
  (anything-isearch-cleanup)
  (with-selected-window (get-buffer-window anything-buffer 'visible)
    (anything-select-action)))


(defun anything-isearch-cancel ()
  "Cancel Anything isearch."
  (interactive)
  (anything-isearch-cleanup)
  (when (get-buffer-window anything-buffer 'visible)
    (with-selected-window (get-buffer-window anything-buffer 'visible)
      (goto-char anything-isearch-original-point)
      (anything-mark-current-line))))


(defun anything-isearch-cleanup ()
  "Clean up the mess."
  (setq minibuffer-message-timeout anything-isearch-original-message-timeout)
  (with-current-buffer anything-buffer
    (setq overriding-terminal-local-map nil)
    (setq cursor-type nil)
    (setq cursor-in-non-selected-windows
          anything-isearch-original-cursor-in-non-selected-windows))
  (when anything-isearch-original-window
    (select-window anything-isearch-original-window))

  (use-global-map anything-isearch-original-global-map)
  (setq-default post-command-hook anything-isearch-original-post-command-hook)
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


;;---------------------------------------------------------------------
;; Iswitchb integration
;;----------------------------------------------------------------------

(defvar anything-iswitchb-candidate-selected nil
  "Indicates whether an anything candidate is selected from iswitchb.")

(defvar anything-iswitchb-frame-configuration nil
  "Saved frame configuration, before anything buffer was displayed.")

(defvar anything-iswitchb-saved-keys nil
  "The original in iswitchb before binding anything keys.")


(defun anything-iswitchb-setup ()
  "Integrate anything completion into iswitchb.

If the user is idle for `anything-iswitchb-idle-delay' seconds
after typing something into iswitchb then anything candidates are
shown for the current iswitchb input.

ESC cancels anything completion and returns to normal iswitchb."
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

  (message "Iswitchb integration is activated."))


(defun anything-iswitchb-minibuffer-setup ()
  (when (eq this-command 'iswitchb-buffer)
    (add-hook 'minibuffer-exit-hook  'anything-iswitchb-minibuffer-exit)

    (setq anything-iswitchb-frame-configuration nil)
    (setq anything-iswitchb-candidate-selected nil)
    (add-hook 'anything-update-hook 'anything-iswitchb-handle-update)

    (anything-initialize)
    
    (add-hook 'post-command-hook 'anything-iswitchb-check-input)))


(defun anything-iswitchb-minibuffer-exit ()
  (remove-hook 'minibuffer-exit-hook  'anything-iswitchb-minibuffer-exit)
  (remove-hook 'post-command-hook 'anything-iswitchb-check-input)
  (remove-hook 'anything-update-hook 'anything-iswitchb-handle-update)

  (anything-cleanup)

  (when anything-iswitchb-frame-configuration
    (set-frame-configuration anything-iswitchb-frame-configuration)
    (setq anything-iswitchb-frame-configuration nil)))


(defun anything-iswitchb-check-input ()
  "Extract iswitchb input and check if it needs to be handled."
  (if (or anything-iswitchb-frame-configuration
          (sit-for anything-iswitchb-idle-delay))
      (anything-check-new-input iswitchb-text)))


(defun anything-iswitchb-handle-update ()
  "Pop up the anything buffer if it's not empty and it's not
shown yet and bind anything commands in iswitchb."
  (unless (or (equal (buffer-size (get-buffer anything-buffer)) 0)
              anything-iswitchb-frame-configuration)
    (setq anything-iswitchb-frame-configuration (current-frame-configuration))

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


;;----------------------------------------------------------------------
;; dummy source
;;----------------------------------------------------------------------
(defun anything-define-dummy-source (name func &rest other-attrib)
  `((name . ,name)
    (candidates "dummy")
    ,@other-attrib
    (filtered-candidate-transformer
     . (lambda (candidates source)
         (funcall ',func)))
    (requires-pattern . 1)
    (volatile)))

(defun anything-dummy-candidate ()
  ;; `source' is defined in filtered-candidate-transformer
  (list (cons (concat "New input: " anything-pattern)
              anything-pattern))) 

;;----------------------------------------------------------------------
;; `completing-read' compatible read function (experimental)
;;----------------------------------------------------------------------
(defun anything-completing-read (prompt collection &optional predicate require-match initial hist default inherit-input-method)
  (let ((result (anything (acr-sources
                           prompt
                           (if (arrayp collection)
                               (all-completions "" collection)
                             collection)
                           predicate require-match initial
                          hist default inherit-input-method)
                         initial prompt)))
    (when (stringp result)
      (prog1 result
        (add-to-list (or hist 'minibuffer-history) result)))))

(defun acr-sources (prompt collection predicate require-match initial hist default inherit-input-method)
  "`anything' replacement for `completing-read'."
  (let ((transformer-func
         (if predicate
             `(candidate-transformer
               . (lambda (cands)
                   (remove-if-not (lambda (c) (,predicate
                                               (if (listp c) (car c) c))) cands)))))
        (new-input-source (unless require-match
                            (anything-define-dummy-source
                             prompt #'anything-dummy-candidate '(action . identity))))
        (history-source (unless require-match
                          `((name . "History")
                            (candidates . ,(or hist 'minibuffer-history))
                            (action . identity))))
        (default-source (when default
                          `((name . "Default")
                            (candidates  ,default)
                            (filtered-candidate-transformer
                             . (lambda (cands source)
                                 (if (string= anything-pattern "")  cands nil)))
                            (action . identity)))))
  `(,default-source
    ((name . "Completions")
     (candidates . ,collection)
     (action . identity)
     ,transformer-func)
    ,history-source
    ,new-input-source)))
;; (anything-completing-read "Command: " obarray 'commandp t)
;; (anything-completing-read "Test: " '(("hoge")("foo")("bar")) nil t)
;; (completing-read "Test: " '(("hoge")("foo")("bar")) nil t)
;; (anything-completing-read "Test: " '(("hoge")("foo")("bar")) nil nil "f" nil)
;; (completing-read "Test: " '(("hoge")("foo")("bar")) nil nil "f" nil nil t)
;; (anything-completing-read "Test: " '(("hoge")("foo")("bar")) nil nil nil nil "nana")
;; (anything-completing-read "Test: " '("hoge" "foo" "bar"))

;;----------------------------------------------------------------------
;; `read-file-name' compatible read function (experimental)
;;----------------------------------------------------------------------
(defvar anything-read-file-name-map (copy-keymap anything-map))
(define-key anything-read-file-name-map "/" 'anything-read-file-name-follow-directory)

(defun anything-read-file-name-follow-directory ()
  (interactive)
  (let* ((sel (anything-get-selection))
         (f (expand-file-name sel dir)))
    (cond ((and (file-directory-p f) (not (string-match "/\\.$" sel)))
           (with-selected-window (minibuffer-window) (delete-minibuffer-contents))
           (setq anything-pattern "")
           (setq dir f)
           (setq anything-compiled-sources nil
                 anything-sources
                 (arfn-sources
                  prompt f default-filename require-match nil predicate))
           (anything-update))
          (t
           (insert "/")))))

(defun anything-read-file-name (prompt &optional dir default-filename require-match initial-input predicate)
  "`anything' replacement for `read-file-name'."
  (let* ((anything-map anything-read-file-name-map)
         (result (anything (arfn-sources
                           prompt dir default-filename require-match
                           initial-input predicate)
                          initial-input prompt)))
    (when (stringp result)
      (prog1 result
        (add-to-list 'minibuffer-history result)))))

(defun arfn-candidates (dir)
  (loop for (f _ _ _ _ _ _ _ _ perm _ _ _) in (directory-files-and-attributes dir t)
        for basename = (file-name-nondirectory f)
        when (string= "d" (substring perm 0 1))
        collect (cons (concat basename "/") f)
        else collect (cons basename f)))

(defun arfn-sources (prompt dir default-filename require-match initial-input predicate)
  (let* ((dir (or dir default-directory))
         (transformer-func
          (if predicate
              `(candidate-transformer
                . (lambda (cands)
                    (remove-if-not
                     (lambda (c) (,predicate (if (listp c) (car c) c))) cands)))))
         (new-input-source (unless require-match
                             (anything-define-dummy-source
                              prompt #'anything-dummy-candidate
                              '(action . identity))))
         (history-source (unless require-match
                           `((name . "History")
                             (candidates . minibuffer-history)
                             (action . identity))))
         (d2r `(display-to-real . (lambda (f) (expand-file-name f ,dir))))
         (default-source (when default-filename
                           `((name . "Default")
                             (candidates  ,default-filename)
                             (filtered-candidate-transformer
                              . (lambda (cands source)
                                  (if (string= anything-pattern "")  cands nil)))
                             ,d2r
                             (action . identity)))))
    `(,default-source
       ((name . ,dir)
        (candidates . (lambda () (arfn-candidates dir)))
        (action . identity)
        ,transformer-func)
       ,new-input-source
       ,history-source)))
;; (anything-read-file-name "file: " "~" ".emacs")
;; (read-file-name "file: " "/tmp")

(defvar anything-read-string-mode nil)
(unless anything-read-string-mode
  (defalias 'anything-old-completing-read (symbol-function 'completing-read))
  (defalias 'anything-old-read-file-name (symbol-function 'read-file-name)))
  
;; (anything-read-string-mode -1)
;; (anything-read-string-mode 1)
;; (anything-read-string-mode 0)
(defun anything-read-string-mode (arg)
  "If this minor mode is on, use `anything' version of `completing-read' and `read-file-name'."
  (interactive "P")
  (setq anything-read-string-mode (if arg (> (prefix-numeric-value arg) 0) (not anything-read-string-mode)))
  (cond (anything-read-string-mode
         ;; redefine to anything version
         (defalias 'completing-read (symbol-function 'anything-completing-read))
         (defalias 'read-file-name (symbol-function 'anything-read-file-name))
         (message "Installed anything version of read functions."))
        (t
         ;; restore to original version
         (defalias 'completing-read (symbol-function 'anything-old-completing-read))
         (defalias 'read-file-name (symbol-function 'anything-old-read-file-name))
         (message "Uninstalled anything version of read functions."))))

;;----------------------------------------------------------------------
;; XEmacs compatibility
;;----------------------------------------------------------------------

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

;;----------------------------------------------------------------------
;; Unit Tests
;;----------------------------------------------------------------------

(defun* anything-test-candidates (sources &optional (input ""))
  "Test helper function for anything.
Given pseudo `anything-sources' and `anything-pattern', returns list like
  ((\"source name1\" (\"candidate1\" \"candidate2\"))
   (\"source name2\" (\"candidate3\" \"candidate4\")))
"
  (let ((anything-test-mode t)
        anything-enable-digit-shortcuts
        anything-candidate-cache
        (anything-sources sources)
        anything-update-hook
        anything-test-candidate-list)
    (get-buffer-create anything-buffer)

    (anything-initialize)
    (setq anything-input input anything-pattern input)
    (anything-update)
    ;; test-mode spec: select 1st candidate!
    (with-current-buffer anything-buffer
      (forward-line 1)
      (anything-mark-current-line))
    (prog1
        anything-test-candidate-list
      (anything-cleanup))))

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(when (fboundp 'expectations)
  (expectations
    (desc "anything-current-buffer")
    (expect "__a_buffer"
      (with-current-buffer (get-buffer-create "__a_buffer")
        (anything-test-candidates nil "")
        (prog1
            (buffer-name anything-current-buffer)
          (kill-buffer (current-buffer)))))
    (desc "anything-buffer-file-name")
    (expect (regexp "/__a_file__")
      (with-current-buffer (find-file-noselect "__a_file__")
        (anything-test-candidates nil "")
        (prog1
            anything-buffer-file-name
          (kill-buffer (current-buffer)))))
    (desc "anything-get-sources")
    (expect '(((name . "foo")))
      (let (anything-compiled-sources
            (anything-sources '(((name . "foo")))))
        (anything-get-sources)))
    (expect '(((name . "foo") (type . test) (action . identity)))
      (let (anything-compiled-sources
            (anything-sources '(((name . "foo") (type . test))))
            (anything-type-attributes '((test (action . identity)))))
        (anything-get-sources)))
    (desc "anything-sources accepts symbols")
    (expect '(((name . "foo")))
      (let* (anything-compiled-sources
             (foo '((name . "foo")))
             (anything-sources '(foo)))
        (anything-get-sources)))
    (desc "anything-get-sources action")
    (expect '(((name . "Actions") (candidates . actions)))
      (let (anything-compiled-sources
            (anything-saved-sources '(((name . "dummy"))))
            (anything-sources '(((name . "Actions") (candidates . actions)))))
        (anything-get-sources)))
    (desc "get-buffer-create candidates-buffer")
    (expect '(((name . "many") (init . many-init)
               (candidates-in-buffer . anything-candidates-in-buffer)
               (candidates . anything-candidates-in-buffer)
               (volatile) (match identity)))
      (let (anything-compiled-sources
            (anything-sources 
             '(((name . "many") (init . many-init)
                (candidates-in-buffer . anything-candidates-in-buffer)))))
        (anything-get-sources)))
    (expect '(((name . "many") (init . many-init)
               (candidates-in-buffer)
               (candidates . anything-candidates-in-buffer)
               (volatile) (match identity)))
      (let (anything-compiled-sources
            (anything-sources 
             '(((name . "many") (init . many-init)
                (candidates-in-buffer)))))
        (anything-get-sources)))
    (expect '(((name . "many") (init . many-init)
               (candidates-in-buffer)
               (type . test)
               (action . identity)
               (candidates . anything-candidates-in-buffer)
               (volatile) (match identity)))
      (let (anything-compiled-sources
            (anything-type-attributes '((test (action . identity))))
            (anything-sources 
             '(((name . "many") (init . many-init)
                (candidates-in-buffer)
                (type . test)))))
        (anything-get-sources)))

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
    (desc "anything-candidates-in-buffer-1")
    (expect nil
      (anything-candidates-in-buffer-1 nil))
    (expect '("foo+" "bar+" "baz+")
      (with-temp-buffer
        (insert "foo+\nbar+\nbaz+\n")
        (let ((anything-candidate-number-limit 5))
          (anything-candidates-in-buffer-1 (current-buffer) ""))))
    (expect '("foo+" "bar+")
      (with-temp-buffer
        (insert "foo+\nbar+\nbaz+\n")
        (let ((anything-candidate-number-limit 2))
          (anything-candidates-in-buffer-1 (current-buffer) ""))))
    (expect '("foo+")
      (with-temp-buffer
        (insert "foo+\nbar+\nbaz+\n")
        (anything-candidates-in-buffer-1 (current-buffer) "oo\\+")))
    (expect '("foo+")
      (with-temp-buffer
        (insert "foo+\nbar+\nbaz+\n")
        (anything-candidates-in-buffer-1 
           (current-buffer) "oo+"
           #'buffer-substring-no-properties #'search-forward)))
    (expect '(("foo+" "FOO+"))
      (with-temp-buffer
        (insert "foo+\nbar+\nbaz+\n")
        (anything-candidates-in-buffer-1
         (current-buffer) "oo\\+"
         (lambda (s e)
           (let ((l (buffer-substring-no-properties s e)))
             (list l (upcase l)))))))
    (desc "anything-candidates-in-buffer")
    (expect '(("TEST" ("foo+" "bar+" "baz+")))
      (anything-test-candidates
       '(((name . "TEST")
          (init
           . (lambda () (with-current-buffer (anything-candidates-buffer 'global)
                          (insert "foo+\nbar+\nbaz+\n"))))
          (candidates . anything-candidates-in-buffer)
          (match identity)
          (volatile)))))
    (expect '(("TEST" ("foo+")))
      (anything-test-candidates
       '(((name . "TEST")
          (init
           . (lambda () (with-current-buffer (anything-candidates-buffer 'global)
                          (insert "foo+\nbar+\nbaz+\n"))))
          (candidates . anything-candidates-in-buffer)
          (match identity)
          (volatile)))
       "oo\\+"))
    (expect '(("TEST" (("foo+" "FOO+"))))
      (anything-test-candidates
       '(((name . "TEST")
          (init
           . (lambda () (with-current-buffer (anything-candidates-buffer 'global)
                          (insert "foo+\nbar+\nbaz+\n"))))
          (candidates
           . (lambda ()
               (anything-candidates-in-buffer
                (lambda (s e)
                  (let ((l (buffer-substring-no-properties s e)))
                    (list l (upcase l)))))))
          (match identity)
          (volatile)))
       "oo\\+"))
    (desc "search attribute")
    (expect '(("TEST" ("foo+")))
      (anything-test-candidates
       '(((name . "TEST")
          (init
           . (lambda () (with-current-buffer (anything-candidates-buffer 'global)
                          (insert "foo+\nbar+\nbaz+\nooo\n"))))
          (search . search-forward)
          (candidates . anything-candidates-in-buffer)
          (match identity)
          (volatile)))
       "oo+"))
    (desc "anything-current-buffer-is-modified")
    (expect nil
      (with-temp-buffer
        (insert "1")
        (setq anything-current-buffer (current-buffer))
        (anything-cleanup)
        (anything-current-buffer-is-modified)))
    (expect t
      (with-temp-buffer
        (insert "1")
        (setq anything-current-buffer (current-buffer))
        (anything-cleanup)
        (insert "2")
        (anything-current-buffer-is-modified)))
    (expect '(("FOO" ("modified")))
      (let ((sources '(((name . "FOO")
                        (candidates
                         . (lambda ()
                             (if (anything-current-buffer-is-modified)
                                 '("modified")
                               '("unmodified"))))))))
        (with-temp-buffer
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
          (insert "1")
          (anything-test-candidates sources)
          (anything-test-candidates sources))))
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
    (desc "anything-candidates-buffer create")
    (expect " *anything candidates:FOO*"
      (let* (anything-candidates-buffer-alist
             (anything-source-name "FOO")
             (buf (anything-candidates-buffer 'global)))
        (prog1 (buffer-name buf)
          (kill-buffer buf))))
    (expect " *anything candidates:FOO*aTestBuffer"
      (let* (anything-candidates-buffer-alist
             (anything-source-name "FOO")
             (anything-current-buffer (get-buffer-create "aTestBuffer"))
             (buf (anything-candidates-buffer 'local)))
        (prog1 (buffer-name buf)
          (kill-buffer anything-current-buffer)
          (kill-buffer buf))))
    (expect 0
      (let (anything-candidates-buffer-alist
            (anything-source-name "FOO") buf)
        (with-current-buffer  (anything-candidates-buffer 'global)
          (insert "1"))
        (setq buf  (anything-candidates-buffer 'global))
        (prog1 (buffer-size buf)
          (kill-buffer buf))))
    (desc "anything-candidates-buffer get-buffer")
    (expect " *anything candidates:FOO*"
      (let* (anything-candidates-buffer-alist
             (anything-source-name "FOO")
             (buf (anything-candidates-buffer 'global)))
        (prog1 (buffer-name (anything-candidates-buffer))
          (kill-buffer buf))))
    (expect " *anything candidates:FOO*aTestBuffer"
      (let* (anything-candidates-buffer-alist
             (anything-source-name "FOO")
             (anything-current-buffer (get-buffer-create "aTestBuffer"))
             (buf (anything-candidates-buffer 'local)))
        (prog1 (buffer-name (anything-candidates-buffer))
          (kill-buffer anything-current-buffer)
          (kill-buffer buf))))
    (expect nil
      (let* (anything-candidates-buffer-alist
             (anything-source-name "NOP__"))
        (anything-candidates-buffer)))
    (desc "anything-candidates-buffer register-buffer")
    (expect " *anything test candidates*"
      (let (anything-candidates-buffer-alist
            (buf (get-buffer-create " *anything test candidates*")))
        (with-current-buffer buf
          (insert "1\n2\n")
          (prog1 (buffer-name (anything-candidates-buffer buf))
            (kill-buffer (current-buffer))))))
    (expect " *anything test candidates*"
      (let (anything-candidates-buffer-alist
            (buf (get-buffer-create " *anything test candidates*")))
        (with-current-buffer buf
          (insert "1\n2\n")
          (anything-candidates-buffer buf)
          (prog1 (buffer-name (anything-candidates-buffer))
            (kill-buffer (current-buffer))))))
    (expect "1\n2\n"
      (let (anything-candidates-buffer-alist
            (buf (get-buffer-create " *anything test candidates*")))
        (with-current-buffer buf
          (insert "1\n2\n")
          (anything-candidates-buffer buf)
          (prog1 (buffer-string)
            (kill-buffer (current-buffer))))))
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
    (desc "anything-execute-selection-action")
    (expect "FOO"
      (anything-execute-selection-action
       "foo" '(("upcase" . upcase))  nil #'identity))
    (expect "FOO"
      (anything-execute-selection-action
       "foo" '(("upcase" . (lambda (c) (upcase c)))) nil #'identity))
    (desc "display-to-real attribute")
    (expect "FOO"
      (anything-execute-selection-action
       "foo"
       '(("identity" . identity))
       nil
       #'upcase
       ))
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

    ))


(provide 'anything)
;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything.el")
;;; anything.el ends here
