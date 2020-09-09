;;; helm-mode.el --- Enable helm completion everywhere. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2019 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-files)

(defvar crm-separator)
(defvar ido-everywhere)
(defvar completion-flex-nospace)
(defvar helm-completion--sorting-done)
(defvar helm-mode)

(declare-function ido-mode "ido.el")
(declare-function helm-apropos-init "helm-elisp")
(declare-function helm-lisp-completion-persistent-action "helm-elisp")
(declare-function helm-lisp-completion-persistent-help "helm-elisp")

(defgroup helm-mode nil
  "Enable helm completion."
  :group 'helm)

(defcustom helm-completing-read-handlers-alist
  '((find-tag . helm-completing-read-default-find-tag)
    (xref-find-definitions . helm-completing-read-default-find-tag)
    (xref-find-references . helm-completing-read-default-find-tag)
    (ggtags-find-tag-dwim . helm-completing-read-default-find-tag)
    (tmm-menubar . nil)
    (find-file . nil)
    (execute-extended-command . nil)
    (dired-do-rename . helm-read-file-name-handler-1)
    (dired-do-copy . helm-read-file-name-handler-1)
    (dired-do-symlink . helm-read-file-name-handler-1)
    (dired-do-relsymlink . helm-read-file-name-handler-1)
    (dired-do-hardlink . helm-read-file-name-handler-1)
    (basic-save-buffer . helm-read-file-name-handler-1)
    (write-file . helm-read-file-name-handler-1)
    (write-region . helm-read-file-name-handler-1))
  "Completing read functions for specific Emacs commands.

By default `helm-mode' use `helm-completing-read-default-handler' to
provide helm completion in each `completing-read' or `read-file-name'
found, but other functions can be specified here for specific
commands. This also allows disabling helm completion for some commands
when needed.

Each entry is a cons cell like (EMACS_COMMAND . COMPLETING-READ_HANDLER)
where key and value are symbols.

Each key is an Emacs command that use originaly `completing-read'.

Each value maybe a helm function that takes same arguments as
`completing-read' plus NAME and BUFFER, where NAME is the name of the new
helm source and BUFFER the name of the buffer we will use, but it can
be also a function not using helm, in this case the function should
take the same args as `completing-read' and not be prefixed by \"helm-\".

`helm' will use the name of the command calling `completing-read' as
NAME and BUFFER will be computed as well with NAME but prefixed with
\"*helm-mode-\".

This function prefix name must start by \"helm-\" when it uses helm,
otherwise `helm' assumes the function is not a helm function and
expects the same args as `completing-read', this allows you to define a
handler not using helm completion.

Example:

    (defun foo/test ()
      (interactive)
      (message \"%S\" (completing-read \"test: \" '(a b c d e))))

    (defun helm-foo/test-completing-read-handler (prompt collection
                                                  predicate require-match
                                                  initial-input hist def
                                                  inherit-input-method
                                                  name buffer)
      (helm-comp-read prompt collection :marked-candidates t
                                        :name name
                                        :buffer buffer))

    (add-to-list 'helm-completing-read-handlers-alist
                 '(foo/test . helm-foo/test-completing-read-handler))


We want here to make the regular `completing-read' in `foo/test'
return a list of candidate(s) instead of a single candidate.

Note that this function will be reused for ALL the `completing-read'
of this command, so it should handle all cases. E.g.,
if first `completing-read' completes against symbols and
second `completing-read' should handle only buffer,
your specialized function should handle both.

If the value of an entry is nil completion will fall back to
Emacs vanilla behaviour.
Example:

If you want to disable helm completion for `describe-function', use:

    (describe-function . nil)

Ido is also supported, you can use `ido-completing-read' and
`ido-read-file-name' as value of an entry or just 'ido.
Example:
Enable ido completion for `find-file':

    (find-file . ido)

same as

    (find-file . ido-read-file-name)

Note that you don't need to enable `ido-mode' for this to work, see
`helm-mode' documentation."
  :group 'helm-mode
  :type '(alist :key-type symbol :value-type symbol))

(defcustom helm-comp-read-case-fold-search helm-case-fold-search
  "Default Local setting of `helm-case-fold-search' for `helm-comp-read'.
See `helm-case-fold-search' for more info."
  :group 'helm-mode
  :type 'symbol)

(defcustom helm-mode-handle-completion-in-region t
  "Whether to replace or not `completion-in-region-function'.
This enables support for `completing-read-multiple' and `completion-at-point'
when non--nil."
  :group 'helm-mode
  :type 'boolean)

(defcustom helm-mode-no-completion-in-region-in-modes nil
  "A list of modes that do not want helm for `completion-in-region'."
  :group 'helm-mode
  :type 'boolean)

(defcustom helm-mode-reverse-history t
  "Display history source after current source when non nil.

Apply only in `helm-mode' handled commands."
  :group 'helm-mode
  :type 'boolean)

(defcustom helm-completion-in-region-default-sort-fn
  'helm-completion-in-region-sort-fn
  "The default sort function to sort candidates in completion-in-region.

When nil no sorting is done.
The function is a `filtered-candidate-transformer' function which takes
two args CANDIDATES and SOURCE.
The function must use the flag `helm-completion--sorting-done' and
return CANDIDATES unchanged when the flag is nil.
See default function `helm-completion-in-region-sort-fn' as example.
It will be used only when `helm-completion-style' is either Emacs or
helm, otherwise when helm-fuzzy style is used, the fuzzy sort function
will be used."
  :group 'helm-mode
  :type 'function)

(defcustom helm-mode-fuzzy-match nil
  "Enable fuzzy matching in `helm-mode' globally.

This is deprecated, use instead helm-fuzzy as `helm-completion-style' or
even better 'emacs as `helm-completion-style' and add 'flex to
`completion-styles' (emacs-27) or 'helm-flex if 'flex is not available
in `completion-styles-alist' (emacs-26)."
  :group 'helm-mode
  :type 'boolean)
(make-obsolete-variable 'helm-mode-fuzzy-match 'helm-completion-style "3.6.0")

(defcustom helm-completion-mark-suffix t
  "Push mark at end of suffix when non nil."
  :group 'helm-mode
  :type 'boolean)

(defvar helm-mode-minibuffer-setup-hook-black-list '(minibuffer-completion-help)
  "Incompatible `minibuffer-setup-hook' functions go here.
A list of symbols.  `helm-mode' is rejecting all lambda's, byte-code fns
and all functions belonging in this list from `minibuffer-setup-hook'.
This is mainly needed to prevent \"*Completions*\" buffers to popup.")

(defface helm-mode-prefix
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       (:background "red" :foreground "black")))
  "Face used for prefix completion."
  :group 'helm-mode)

(defvar helm-comp-read-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-return>") 'helm-cr-empty-string)
    (define-key map (kbd "M-RET")      'helm-cr-empty-string)
    map)
  "Keymap for `helm-comp-read'.")

(defun helm-mode-delete-char-backward-1 ()
  (interactive)
  (condition-case err
      (call-interactively 'delete-backward-char)
    (text-read-only
     (if (with-selected-window (minibuffer-window)
           (not (string= (minibuffer-contents) "")))
         (message "Trying to delete prefix completion, next hit will quit")
       (user-error "%s" (car err))))))
(put 'helm-mode-delete-char-backward-1 'helm-only t)

(defun helm-mode-delete-char-backward-2 ()
  (interactive)
  (condition-case _err
      (call-interactively 'delete-backward-char)
    (text-read-only
     (unless (with-selected-window (minibuffer-window)
               (string= (minibuffer-contents) ""))
       (with-helm-current-buffer
         (run-with-timer 0.1 nil (lambda ()
                                   (call-interactively 'delete-backward-char))))
       (helm-keyboard-quit)))))
(put 'helm-mode-delete-char-backward-2 'helm-only t)

(helm-multi-key-defun helm-mode-delete-char-backward-maybe
    "Delete char backward when text is not the prefix helm is completing against.
First call warns user about deleting prefix completion.
Second call deletes backward char in current-buffer and quits helm completion,
letting the user start a new completion with a new prefix."
  '(helm-mode-delete-char-backward-1 helm-mode-delete-char-backward-2) 1)

(defcustom helm-completion-style 'emacs
  "Style of completion to use in `completion-in-region'.

This affects only `completion-at-point' and friends, and
the `completing-read' using the default handler
i.e. `helm-completing-read-default-handler'.

NB: This has nothing to do with `completion-styles', it is independent from
helm, but when using 'emacs as helm-completion-style helm
will use the `completion-styles' for its completions.
Up to the user to configure `completion-styles'.

There are three possible values to use:

- helm, use multi match regular helm completion.

- helm-fuzzy, use fuzzy matching.  Note that as usual when
  entering a space helm switch to multi matching mode.

- emacs, use regular Emacs completion according to
  `completion-styles'.  Note that even in this style, helm allows using
  multi match.  Emacs-27 provides a style called `flex' that can be used
  aside `helm' style (see `completion-styles-alist').  When `flex' style
  is not available (Emacs<27) helm provides `helm-flex' style which is similar to
  `flex' and helm fuzzy matching.

For a better experience, if you don't know what to use, set
`completion-styles' to '(flex) if you are using emacs-27 or to
\'(helm-flex) if you are using emacs-26 and keep 'emacs as default
value for `helm-completion-style'.  Advanced users can also have a
look to `completion-category-overrides' to set styles according to category.

Please use custom interface or `customize-set-variable' to set this,
NOT `setq'."
  :group 'helm-mode
  :type '(choice (const :tag "Emacs" emacs)
                 (const :tag "Helm" helm)
                 (const :tag "Helm-fuzzy" helm-fuzzy))
  :set (lambda (var val)
         (set var val)
         (if (memq val '(helm helm-fuzzy))
             (define-key helm-comp-read-map (kbd "DEL") 'helm-mode-delete-char-backward-maybe)
           (define-key helm-comp-read-map (kbd "DEL") 'delete-backward-char))))

(defconst helm-completion--all-styles
  (let ((flex (if (assq 'flex completion-styles-alist)
                  'flex 'helm-flex)))
    (helm-fast-remove-dups
     (append (list 'helm flex)
             (mapcar 'car completion-styles-alist)))))

(defconst helm-completion--styles-type
  `(repeat :tag "with other completion styles"
           (choice ,@(mapcar (lambda (x) (list 'const x))
                             helm-completion--all-styles))))

(defcustom helm-completion-styles-alist '((gud-mode . helm))
  "Allow configuring `helm-completion-style' per mode.

Each entry is a cons cell like (mode . style) where style must be a
suitable value for `helm-completion-style'.
When specifying emacs as style for a mode, `completion-styles' can be
specified by using a cons cell specifying completion-styles to use
with helm emacs style, e.g. (foo-mode . (emacs helm flex)) will set
`completion-styles' to '(helm flex) for foo-mode.  This affects only
completions happening in buffers and not minibuffer completions,
i.e. completing-read's."
  :group 'helm-mode
  :type
  `(alist :key-type (symbol :tag "Major Mode")
          :value-type
          (choice :tag "Use helm style or completion styles"
                  (radio :tag "Helm Style"
                         (const helm)
                         (const helm-fuzzy)
                         (const emacs))
                  (cons :tag "Completion Styles"
                        (const :tag "Using Helm `emacs' style" emacs)
                        ,helm-completion--styles-type))))

;;; helm-comp-read
;;
;;
(defvar helm-comp-read-use-marked nil
  "[INTERNAL] When non nil `helm-comp-read' will return marked candidates.

Use this ONLY in `let', NOT globally, this allows third party packages
to use a list as return value when `helm-mode' is enabled, e.g.

    (let ((helm-comp-read-use-marked t))
      (completing-read \"test: \" '(a b c d e f g)))

")

(defun helm-cr-empty-string ()
  "Return empty string."
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action
     (lambda (_candidate)
         (identity "")))))
(put 'helm-cr-empty-string 'helm-only t)

(defun helm-mode--keyboard-quit ()
  ;; Use this instead of `keyboard-quit'
  ;; to avoid deactivating mark in current-buffer.
  (let ((debug-on-quit nil))
    (signal 'quit nil)))

(cl-defun helm-comp-read-get-candidates (collection &optional
                                                    test sort-fn alistp
                                                    (input helm-pattern))
  "Convert COLLECTION to list removing elements that don't match TEST.
See `helm-comp-read' about supported COLLECTION arguments.

SORT-FN is a predicate to sort COLLECTION.

ALISTP when non--nil will not use `all-completions' to collect
candidates because it doesn't handle alists correctly for helm.
i.e In `all-completions' the car of each pair is used as value.
In helm we want to use the cdr instead like \(display . real\),
so we return the alist as it is with no transformation by
`all-completions'.

e.g

\(setq A '((a . 1) (b . 2) (c . 3)))
==>((a . 1) (b . 2) (c . 3))
\(helm-comp-read \"test: \" A :alistp nil
                              :exec-when-only-one t
                              :initial-input \"a\")
==>\"a\" Which is not what we expect.

\(helm-comp-read \"test: \" A :alistp t
                              :exec-when-only-one t
                              :initial-input \"1\")
==>\"1\"

See docstring of `all-completions' for more info.

INPUT is the string you want to complete against, defaulting to
`helm-pattern' which is the value of what you enter in minibuffer.
Note that when using a function as COLLECTION this value will be
available with the input argument of the function only when using a
sync source from `helm-comp-read', i.e. not using
`:candidates-in-buffer', otherwise the function is called only once
with an empty string as value for `helm-pattern' because
`helm-pattern' is not yet computed, which is what we want otherwise
data would not be fully collected at init time.

If COLLECTION is an `obarray', a TEST should be needed. See `obarray'."
  ;; Ensure COLLECTION is computed from `helm-current-buffer'
  ;; because some functions used as COLLECTION work
  ;; only in the context of current-buffer (Issue #1030) .
  (with-helm-current-buffer
    (let ((cands
           (cond ((vectorp collection)
                  (all-completions input collection test))
                 ((and (symbolp collection) (boundp collection)
                       ;; Issue #324 history is let-bounded and given
                       ;; quoted as hist argument of completing-read.
                       ;; See example in `rcirc-browse-url'.
                       (symbolp (symbol-value collection)))
                  nil)
                 ;; When collection is a symbol, most of the time
                 ;; it should be a symbol used as a minibuffer-history.
                 ;; The value of this symbol in this case return a list
                 ;; of string which maybe are converted later as symbol
                 ;; in special cases.
                 ;; we treat here commandp as a special case as it return t
                 ;; also with a string unless its last arg is provided.
                 ;; Also, the history collections generally collect their
                 ;; elements as string, so intern them to call predicate.
                 ((and (symbolp collection) (boundp collection) test)
                  (let ((predicate (lambda (elm)
                                     (condition-case _err
                                         (if (eq test 'commandp)
                                             (funcall test (intern elm))
                                             (funcall test elm))
                                       (wrong-type-argument
                                        (funcall test (intern elm)))))))
                    (all-completions input (symbol-value collection) predicate)))
                 ((and (symbolp collection) (boundp collection))
                  (all-completions input (symbol-value collection)))
                 ;; Normally file completion should not be handled here,
                 ;; but special cases like `find-file-at-point' do it.
                 ;; Handle here specially such cases.
                 ((and (functionp collection) (not (string= input ""))
                       (or minibuffer-completing-file-name
                           (eq (completion-metadata-get
                                (completion-metadata input collection test)
                                'category)
                               'file)))
                  (cl-loop for f in (funcall collection input test t)
                           unless (member f '("./" "../"))
                           if (string-match helm--url-regexp input)
                           collect f
                           else
                           collect (concat (file-name-as-directory
                                            (helm-basedir input))
                                           f)))
                 ((functionp collection)
                  (funcall collection input test t))
                 ((and alistp (null test)) collection)
                 ;; Next test ensure circular objects are removed
                 ;; with `all-completions' (Issue #1530).
                 (t (all-completions input collection test)))))
      (if sort-fn (sort cands sort-fn) cands))))

(defun helm-cr--pattern-in-candidates-p (candidates)
  (or (assoc helm-pattern candidates)
      (assq (intern helm-pattern) candidates)
      (member helm-pattern candidates)
      (member (downcase helm-pattern) candidates)
      (member (upcase helm-pattern) candidates)))

(defun helm-cr-default-transformer (candidates source)
  "Default filter candidate function for `helm-comp-read'."
  (let ((must-match (helm-attr 'must-match source))
        unknown-pattern)
    (unless (or (eq must-match t)
                (string= helm-pattern "")
                ;; Check if pattern is already member of candidates.
                (helm-cr--pattern-in-candidates-p candidates))
      (setq candidates (append (list
                                ;; Unquote helm-pattern
                                ;; when it is added
                                ;; as candidate: Why? #2015
                                ;; (replace-regexp-in-string
                                ;;  "\\s\\" "" helm-pattern)
                                helm-pattern)
                               candidates))
      ;; Notify pattern have been added to candidates.
      (setq unknown-pattern t))
    (cl-loop for c in candidates
             for cand = (if (stringp c)
                            (replace-regexp-in-string "\\s\\" "" c)
                          c)
             for pat = (replace-regexp-in-string "\\s\\" "" helm-pattern)
             if (and (or (equal c pat) (equal c helm-pattern))
                     unknown-pattern)
             collect
             (cons (concat (propertize
                            " " 'display
                            (propertize "[?]" 'face 'helm-ff-prefix))
                           c)
                   c)
             into lst
             else collect (if (and (stringp cand)
                                   (string-match "\n" cand))
                              (cons (replace-regexp-in-string "\n" "->" c) c)
                            c)
             into lst
             finally return (helm-fast-remove-dups lst :test 'equal))))

(defun helm-comp-read--move-to-first-real-candidate ()
  (helm-aif (helm-get-selection nil 'withprop)
      ;; Avoid error with candidates with an image as display (#2296).
      (when (equal (get-text-property 0 'display it) "[?]")
        (helm-next-line))))

(defun helm-cr-default (default cands)
  (delq nil
        (cond ((and (stringp default)
                    (not (string= default ""))
                    (string= helm-pattern ""))
               (cons default (delete default cands)))
              ((and (consp default) (string= helm-pattern ""))
               (append (cl-loop for d in default
                                ;; Don't convert
                                ;; nil to "nil" (i.e the string)
                                ;; it will be delq'ed on top.
                                for str = (if (null d) d (helm-stringify d))
                                when (member str cands)
                                do (setq cands (delete d cands))
                                when str collect str)
                       cands))
              (t cands))))

;;;###autoload
(cl-defun helm-comp-read (prompt collection
                          &key
                            test
                            initial-input
                            default
                            preselect
                            (buffer "*Helm Completions*")
                            must-match
                            fuzzy
                            reverse-history
                            (requires-pattern 0)
                            history
                            input-history
                            (case-fold helm-comp-read-case-fold-search)
                            (del-input t)
                            (persistent-action nil)
                            (persistent-help "DoNothing")
                            (mode-line helm-comp-read-mode-line)
                            help-message
                            (keymap helm-comp-read-map)
                            (name "Helm Completions")
                            header-name
                            candidates-in-buffer
                            match-part
                            match-dynamic
                            exec-when-only-one
                            quit-when-no-cand
                            (volatile t)
                            sort
                            fc-transformer
                            hist-fc-transformer
                            (marked-candidates helm-comp-read-use-marked)
                            nomark
                            (alistp t)
                            (candidate-number-limit helm-candidate-number-limit)
                            multiline
                            allow-nest
                            coerce
                            (group 'helm))
  "Read a string in the minibuffer, with helm completion.

It is helm `completing-read' equivalent.

- PROMPT is the prompt name to use.

- COLLECTION can be a list, vector, obarray or hash-table.
  It can be also a function that receives three arguments:
  the values string, predicate and t. See `all-completions' for more details.

Keys description:

- TEST: A predicate called with one arg i.e candidate.

- INITIAL-INPUT: Same as input arg in `helm'.

- PRESELECT: See preselect arg of `helm'.

- DEFAULT: This option is used only for compatibility with regular
  Emacs `completing-read' (Same as DEFAULT arg of `completing-read').

- BUFFER: Name of helm-buffer.

- MUST-MATCH: Candidate selected must be one of COLLECTION.

- FUZZY: Enable fuzzy matching.

- REVERSE-HISTORY: When non--nil display history source after current
  source completion.

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. The minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- CASE-FOLD: Same as `helm-case-fold-search'.

- DEL-INPUT: Boolean, when non--nil (default) remove the partial
  minibuffer input from HISTORY is present.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  Default is `helm-comp-read-mode-line'.

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (the keymap will be shared with history source)

- NAME: The name related to this local source.

- HEADER-NAME: A function to alter NAME, see `helm'.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute.

- SORT: A predicate to give to `sort' e.g `string-lessp'
  Use this only on small data as it is inefficient.
  If you want to sort faster add a sort function to
  FC-TRANSFORMER.
  Note that FUZZY when enabled is already providing a sort function.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function
  or a list of functions.

- HIST-FC-TRANSFORMER: A `filtered-candidate-transformer'
  function for the history source.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: (default is non--nil) See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-source-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

- MATCH-PART: Allow matching only one part of candidate.
  See match-part documentation in `helm-source'.

- MATCH-DYNAMIC: See match-dynamic in `helm-source-sync'
  It has no effect when used with CANDIDATES-IN-BUFFER.

- ALLOW-NEST: Allow nesting this `helm-comp-read' in a helm session.
  See `helm'.

- MULTILINE: See multiline in `helm-source'.

- COERCE: See coerce in `helm-source'.

- GROUP: See group in `helm-source'.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That means you can pass prefix args before or after calling a command
that use `helm-comp-read'.  See `helm-M-x' for example."

  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (let ((action-fn `(("Sole action (Identity)"
                      . (lambda (candidate)
                          (if ,marked-candidates
                              (helm-marked-candidates)
                              (identity candidate)))))))
    (let* ((minibuffer-completion-predicate test)
           (minibuffer-completion-table collection)
           (helm-read-file-name-mode-line-string
            (replace-regexp-in-string "helm-maybe-exit-minibuffer"
                                      "helm-confirm-and-exit-minibuffer"
                                      helm-read-file-name-mode-line-string))
           (get-candidates
            (lambda ()
              (let ((cands (helm-comp-read-get-candidates
                            ;; If `helm-pattern' is passed as INPUT
                            ;; and :alistp is nil INPUT is passed to
                            ;; `all-completions' which defeat helm
                            ;; matching functions (multi match, fuzzy
                            ;; etc...) issue #2134.
                            collection test sort alistp
                            (if (and match-dynamic (null candidates-in-buffer))
                                helm-pattern ""))))
                (helm-cr-default default cands))))
           (history-get-candidates
            (lambda ()
              (let ((cands (helm-comp-read-get-candidates
                            history test nil alistp)))
                (when cands
                  (delete "" (helm-cr-default default cands))))))
           (src-hist (helm-build-sync-source (format "%s History" name)
                       :candidates history-get-candidates
                       :fuzzy-match fuzzy
                       :multiline multiline
                       :match-part match-part
                       :filtered-candidate-transformer
                       (append '((lambda (candidates sources)
                                   (cl-loop for i in candidates
                                            ;; Input is added to history in completing-read's
                                            ;; and may be regexp-quoted, so unquote it
                                            ;; but check if cand is a string (it may be at this stage
                                            ;; a symbol or nil) Issue #1553.
                                            when (stringp i)
                                            collect (replace-regexp-in-string "\\s\\" "" i))))
                               (and hist-fc-transformer (helm-mklist hist-fc-transformer)))
                       :persistent-action persistent-action
                       :persistent-help persistent-help
                       :keymap keymap
                       :must-match must-match
                       :group group
                       :coerce coerce
                       :mode-line mode-line
                       :help-message help-message
                       :action action-fn))
           (src (helm-build-sync-source name
                  :candidates get-candidates
                  :match-part match-part
                  :multiline multiline
                  :header-name header-name
                  :filtered-candidate-transformer
                  (let ((transformers (helm-mklist fc-transformer)))
                    (append transformers
                            (unless (member 'helm-cr-default-transformer transformers)
                              '(helm-cr-default-transformer))))
                  :requires-pattern requires-pattern
                  :persistent-action persistent-action
                  :persistent-help persistent-help
                  :fuzzy-match fuzzy
                  :keymap keymap
                  :must-match must-match
                  :group group
                  :coerce coerce
                  :mode-line mode-line
                  :match-dynamic match-dynamic
                  :help-message help-message
                  :action action-fn
                  :volatile volatile))
           (src-1 (helm-build-in-buffer-source name
                    :data get-candidates
                    :match-part match-part
                    :multiline multiline
                    :header-name header-name
                    :filtered-candidate-transformer
                    (append (helm-mklist fc-transformer)
                            '(helm-cr-default-transformer))
                    :requires-pattern requires-pattern
                    :persistent-action persistent-action
                    :fuzzy-match fuzzy
                    :keymap keymap
                    :must-match must-match
                    :group group
                    :coerce coerce
                    :persistent-help persistent-help
                    :mode-line mode-line
                    :help-message help-message
                    :action action-fn))
           (src-list (list src-hist
                           (if candidates-in-buffer
                               src-1 src)))
           (helm-execute-action-at-once-if-one exec-when-only-one)
           (helm-quit-if-no-candidate quit-when-no-cand)
           result)
      (when nomark
        (setq src-list (cl-loop for src in src-list
                             collect (cons '(nomark) src))))
      (when reverse-history (setq src-list (nreverse src-list)))
      (add-hook 'helm-after-update-hook 'helm-comp-read--move-to-first-real-candidate)
      (unwind-protect
           (setq result (helm
                         :sources src-list
                         :input initial-input
                         :default default
                         :preselect preselect
                         :prompt prompt
                         :resume 'noresume
                         :keymap keymap ;; Needed with empty collection.
                         :allow-nest allow-nest
                         :candidate-number-limit candidate-number-limit
                         :case-fold-search case-fold
                         :history (and (symbolp input-history) input-history)
                         :buffer buffer))
        (remove-hook 'helm-after-update-hook 'helm-comp-read--move-to-first-real-candidate))
      ;; Avoid adding an incomplete input to history.
      (when (and result history del-input)
        (cond ((and (symbolp history) ; History is a symbol.
                    (not (symbolp (symbol-value history)))) ; Fix Issue #324.
               ;; Be sure history is not a symbol with a nil value.
               (helm-aif (symbol-value history) (setcar it result)))
              ((consp history) ; A list with a non--nil value.
               (setcar history result))
              (t ; Possibly a symbol with a nil value.
               (set history (list result)))))
      (or result (helm-mode--keyboard-quit)))))


;; Generic completing-read
;;
;; Support also function as collection.
;; e.g M-x man is supported.
;; Support hash-table and vectors as collection.
;; NOTE:
;; Some crap emacs functions may not be supported
;; like ffap-alternate-file (bad use of completing-read)
;; and maybe others.
;; Provide a mode `helm-mode' which turn on
;; helm in all `completing-read' and `read-file-name' in Emacs.
;;
(defvar helm-completion-mode-string " Helm")

(defvar helm-completion-mode-quit-message
  "Helm completion disabled")

(defvar helm-completion-mode-start-message
  "Helm completion enabled")

;;; Specialized handlers
;;
;;
(defun helm-completing-read-symbols
    (prompt _collection test _require-match init
     hist default _inherit-input-method name buffer)
  "Specialized function for fast symbols completion in `helm-mode'."
  (require 'helm-elisp)
  (or
   (helm
    :sources (helm-build-in-buffer-source name
               :init (lambda ()
                       (helm-apropos-init (lambda (x)
                                            (and (funcall test x)
                                                 (not (keywordp x))))
                                          (or (car-safe default) default)))
               :filtered-candidate-transformer 'helm-apropos-default-sort-fn
               :help-message #'helm-comp-read-help-message
               :fuzzy-match helm-mode-fuzzy-match
               :persistent-action
               (lambda (candidate)
                 (helm-lisp-completion-persistent-action
                  candidate name))
               :persistent-help (helm-lisp-completion-persistent-help))
    :prompt prompt
    :buffer buffer
    :input init
    :history hist
    :resume 'noresume
    :default (or default ""))
     (helm-mode--keyboard-quit)))


;;; Generic completing read
;;
;;
(defun helm-completing-read-default-1
    (prompt collection test require-match
     init hist default _inherit-input-method
     name buffer &optional cands-in-buffer exec-when-only-one)
  "Call `helm-comp-read' with same args as `completing-read'.
Extra optional arg CANDS-IN-BUFFER means use `candidates-in-buffer'
method which is faster.
It should be used when candidate list doesn't need to be rebuilt dynamically."
  (let ((history (or (car-safe hist) hist))
        (initial-input (helm-aif (pcase init
                                   ((pred (stringp)) init)
                                   ;; INIT is a cons cell.
                                   (`(,l . ,_ll) l))
                           it)))
    (helm-comp-read
     prompt collection
     :test test
     :history history
     :reverse-history helm-mode-reverse-history
     :input-history history
     :must-match require-match
     :alistp nil
     :help-message #'helm-comp-read-help-message
     :name name
     :requires-pattern (if (and (stringp default)
                                (string= default "")
                                (or (eq require-match 'confirm)
                                    (eq require-match
                                        'confirm-after-completion)))
                           1 0)
     :nomark (null helm-comp-read-use-marked)
     :candidates-in-buffer cands-in-buffer
     :exec-when-only-one exec-when-only-one
     :fuzzy helm-mode-fuzzy-match
     :buffer buffer
     ;; If DEF is not provided, fallback to empty string
     ;; to avoid `thing-at-point' to be appended on top of list
     :default (or default "")
     ;; Fail with special characters (e.g in gnus "nnimap+gmail:")
     ;; if regexp-quote is not used.
     ;; when init is added to history, it will be unquoted by
     ;; helm-comp-read.
     :initial-input initial-input)))

(defun helm-completing-read-default-2
    (prompt collection predicate require-match
     init hist default _inherit-input-method
     name buffer &optional _cands-in-buffer exec-when-only-one)
  "Call `helm-comp-read' with same args as `completing-read'.

This handler uses dynamic matching which allows honouring `completion-styles'."
  (let* ((history (or (car-safe hist) hist))
         (input (pcase init
                  ((pred (stringp)) init)
                  ;; INIT is a cons cell.
                  (`(,l . ,_ll) l)))
         (completion-flex-nospace t)
         (completion-styles
          (helm--prepare-completion-styles 'nomode))
         (metadata (or (completion-metadata (or input "") collection predicate)
                       '(metadata)))
         (afun (or (plist-get completion-extra-properties :annotation-function)
                   (completion-metadata-get metadata 'annotation-function)))
         (file-comp-p (eq (completion-metadata-get metadata 'category) 'file))
         (compfn (lambda (str _predicate _action)
                   (let* ((completion-ignore-case (helm-set-case-fold-search))
                          (comps
                           (completion-all-completions
                            str         ; This is helm-pattern
                            collection
                            predicate
                            (length str)
                            metadata))
                          (last-data (last comps))
                          ;; Helm syle sort fn is added to
                          ;; metadata only in emacs-27, so in
                          ;; emacs-26 use helm-generic-sort-fn
                          ;; which handle both helm and
                          ;; helm-flex styles. When
                          ;; helm-completion-style is helm or
                          ;; helm-fuzzy, sorting will be done
                          ;; later in FCT.
                          (sort-fn
                           (and (eq helm-completion-style 'emacs)
                                (or
                                 ;; Emacs-27
                                 (completion-metadata-get
                                  metadata 'display-sort-function)
                                 ;; Emacs-26
                                 (lambda (candidates)
                                   (sort candidates #'helm-generic-sort-fn)))))
                          all)
                     (when (cdr last-data)
                       ;; Remove the last element of
                       ;; comps by side-effect.
                       (setcdr last-data nil))
                     (setq helm-completion--sorting-done (and sort-fn t))
                     (setq all (copy-sequence comps))
                     ;; Default is passed here only with helm
                     ;; h-c-styles, otherwise with emacs style it is
                     ;; passed with the :default arg of helm-comp-read
                     ;; and computed in its get-candidates function.
                     (append (and default
                                  (memq helm-completion-style '(helm helm-fuzzy))
                                  (list default))
                             (helm-completion-in-region--initial-filter
                              (if (and sort-fn (> (length str) 0))
                                  (funcall sort-fn all)
                                all)
                              afun file-comp-p)))))
         (data (if (memq helm-completion-style '(helm helm-fuzzy))
                   (funcall compfn (or input "") nil nil)
                 compfn))
         (helm-completion-in-region-default-sort-fn
          (lambda (candidates _source)
            (if (or helm-completion--sorting-done
                    (string= helm-pattern ""))
                candidates
              (sort candidates 'helm-generic-sort-fn)))))
    (unwind-protect
        (helm-comp-read
         ;; Completion-at-point and friends have no prompt.
         prompt
         data
         :name name
         :initial-input input
         :buffer buffer
         :history history
         :nomark (null helm-comp-read-use-marked)
         :reverse-history helm-mode-reverse-history
         ;; In helm h-c-styles default is passed directly in
         ;; candidates.
         :default (and (eq helm-completion-style 'emacs) default)
         :fc-transformer
         ;; Ensure sort fn is at the end.
         (append '(helm-cr-default-transformer)
                 (and helm-completion-in-region-default-sort-fn
                      (list helm-completion-in-region-default-sort-fn)))
         :match-dynamic (eq helm-completion-style 'emacs)
         :fuzzy (eq helm-completion-style 'helm-fuzzy)
         :exec-when-only-one exec-when-only-one
         :must-match require-match)
      (setq helm-completion--sorting-done nil))))

(defun helm-completing-read-default-find-tag
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Specialized `helm-mode' handler for `find-tag'."
  ;; Some commands like find-tag may use `read-file-name' from inside
  ;; the calculation of collection. in this case it clash with
  ;; candidates-in-buffer that reuse precedent data (files) which is wrong.
  ;; So (re)calculate collection outside of main helm-session.
  (let* ((cands (helm-comp-read-get-candidates
                 collection test nil nil)))
    (helm-completing-read-default-1 prompt cands test require-match
                                    init hist default inherit-input-method
                                    name buffer t)))

(defun helm-completing-read-sync-default-handler
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "`helm-mode' handler using sync source as backend."
  (helm-completing-read-default-1 prompt collection test require-match
                                  init hist default inherit-input-method
                                  name buffer))

(defun helm-completing-read-inbuffer-default-handler
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "`helm-mode' handler using inbuffer source as backend."
  (helm-completing-read-default-1 prompt collection test require-match
                                  init hist default inherit-input-method
                                  name buffer t))

(defun helm-completing-read-default-handler
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Default `helm-mode' handler for all `completing-read'."
  (let* ((standard (memq helm-completion-style '(helm helm-fuzzy)))
         (fn (if standard
                 #'helm-completing-read-default-1
               #'helm-completing-read-default-2))
         (helm-mode-fuzzy-match (eq helm-completion-style 'helm-fuzzy)))
    (apply fn
           prompt collection test require-match
           init hist default inherit-input-method
           name buffer standard)))

(defun helm-mode--read-buffer-to-switch (prompt)
  "[INTERNAL] This is used to advice `read-buffer-to-switch'.
Don't use it directly."
  ;; `read-buffer-to-switch' is passing `minibuffer-completion-table'
  ;; to `read-buffer' through `minibuffer-setup-hook' which is too
  ;; late to be known by `read-buffer-function', in our case
  ;; `helm--generic-read-buffer'.  It should let bind it to allow us
  ;; using it. 
  (let ((minibuffer-completion-table (internal-complete-buffer-except)))
    (read-buffer prompt (other-buffer (current-buffer))
                 (confirm-nonexistent-file-or-buffer))))

(defun helm--generic-read-buffer (prompt &optional default require-match predicate)
  "The `read-buffer-function' for `helm-mode'.
Affects `switch-to-buffer' `kill-buffer' and related."
  (helm--completing-read-default
   prompt (or minibuffer-completion-table
              (internal-complete-buffer "" nil t))
   predicate require-match nil nil default))

(cl-defun helm--completing-read-default
    (prompt collection &optional
                         predicate require-match
                         initial-input hist def
                         inherit-input-method)
  "An helm replacement of `completing-read'.
This function should be used only as a `completing-read-function'.

Don't use it directly, use instead `helm-comp-read' in your programs.

See documentation of `completing-read' and `all-completions' for details."
  (let* ((current-command (or (helm-this-command) this-command))
         (str-command     (helm-symbol-name current-command))
         (buf-name        (format "*helm-mode-%s*" str-command))
         (entry           (assq current-command
                                helm-completing-read-handlers-alist))
         (def-com         (cdr-safe entry))
         (str-defcom      (and def-com (helm-symbol-name def-com)))
         (def-args        (list prompt collection predicate require-match
                                initial-input hist def inherit-input-method))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in helm specialized functions.
         (others-args        (append def-args (list str-command buf-name)))
         helm-completion-mode-start-message ; Be quiet
         helm-completion-mode-quit-message
         ;; Be sure this pesty *completion* buffer doesn't popup.
         ;; Note: `minibuffer-with-setup-hook' may setup a lambda
         ;; calling `minibuffer-completion-help' or other minibuffer
         ;; functions we DONT WANT here, in these cases removing the hook
         ;; (a symbol) have no effect. Issue #448.
         ;; Because `minibuffer-completion-table' and
         ;; `minibuffer-completion-predicate' are not bound
         ;; anymore here, these functions should have no effect now,
         ;; except in some rare cases like in `woman-file-name',
         ;; so remove all incompatible functions
         ;; from `minibuffer-setup-hook' (Issue #1205, #1240).
         ;; otherwise helm have not the time to close its initial session.
         (minibuffer-setup-hook
          (cl-loop for h in minibuffer-setup-hook
                   unless (or (consp h) ; a lambda.
                              (byte-code-function-p h)
                              (helm-subr-native-elisp-p h)
                              (memq h helm-mode-minibuffer-setup-hook-black-list))
                   collect h))
         ;; Disable hack that could be used before `completing-read'.
         ;; i.e (push ?\t unread-command-events).
         unread-command-events
         (default-handler
           ;; If nothing is found in
           ;; helm-completing-read-handlers-alist use default
           ;; handler.
           #'helm-completing-read-default-handler))
    (when (eq def-com 'ido) (setq def-com 'ido-completing-read))
    (unless (or (not entry) def-com)
      ;; An entry in *read-handlers-alist exists but have
      ;; a nil value, so we exit from here, disable `helm-mode'
      ;; and run the command again with it original behavior.
      ;; `helm-mode' will be restored on exit.
      (cl-return-from helm--completing-read-default
        (unwind-protect
             (progn
               (helm-mode -1)
               (apply completing-read-function def-args))
          (helm-mode 1))))
    ;; If we use now `completing-read' we MUST turn off `helm-mode'
    ;; to avoid infinite recursion and CRASH. It will be reenabled on exit.
    (when (or (eq def-com 'completing-read)
              ;; All specialized functions are prefixed by "helm"
              (and (stringp str-defcom)
                   (not (string-match "^helm" str-defcom))))
      (helm-mode -1))
    (unwind-protect
         (cond (;; An helm specialized function exists, run it.
                (and def-com helm-mode)
                (apply def-com others-args))
               (;; Try to handle `ido-completing-read' everywhere.
                (and def-com (eq def-com 'ido-completing-read))
                (setcar (memq collection def-args)
                        (all-completions "" collection predicate))
                (apply def-com def-args))
               (;; User set explicitely `completing-read' or something similar
                ;; in *read-handlers-alist, use this with exactly the same
                ;; args as in `completing-read'.
                ;; If we are here `helm-mode' is now disabled.
                def-com
                (apply def-com def-args))
               (;; Use by default a in-buffer handler unless
                ;; COLLECTION is a function.
                t
                (funcall default-handler
                         prompt collection predicate require-match
                         initial-input hist def inherit-input-method
                         str-command buf-name)))
      (helm-mode 1)
      ;; When exiting minibuffer, `this-command' is set to
      ;; `helm-exit-minibuffer', which is unwanted when starting
      ;; on another `completing-read', so restore `this-command' to
      ;; initial value when exiting.
      (setq this-command current-command))))

;;; Generic read-file-name
;;
;;
;;;###autoload
(cl-defun helm-read-file-name
    (prompt
     &key
       (name "Read File Name")
       (initial-input default-directory)
       (buffer "*Helm file completions*")
       test
       noret
       (case-fold helm-file-name-case-fold-search)
       preselect
       history
       must-match
       (fuzzy t)
       default
       marked-candidates
       (candidate-number-limit helm-ff-candidate-number-limit)
       nomark
       (alistp t)
       (persistent-action-if 'helm-find-files-persistent-action-if)
       (persistent-help "Hit1 Expand Candidate, Hit2 or (C-u) Find file")
       (mode-line helm-read-file-name-mode-line-string))
  "Read a file name with helm completion.
It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start reading file name, default to `default-directory'.

- BUFFER: `helm-buffer' name, defaults to \"*Helm Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- NORET: Allow disabling helm-ff-RET (have no effect if helm-ff-RET
                                      isn't bound to RET).

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- FUZZY: Enable fuzzy matching when non-nil (Enabled by default).

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- NOMARK: When non--nil don't allow marking candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION-IF: a persistent if action function.

- PERSISTENT-HELP: persistent help message.

- MODE-LINE: A mode line message, default is `helm-read-file-name-mode-line-string'."
  (require 'tramp)
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (mapc (lambda (hook)
          (add-hook 'helm-after-update-hook hook))
        '(helm-ff-move-to-first-real-candidate
          helm-ff-update-when-only-one-matched
          helm-ff-auto-expand-to-home-or-root))
  (let* ((action-fn `(("Sole action (Identity)"
                       . (lambda (candidate)
                           (if ,marked-candidates
                               (helm-marked-candidates :with-wildcard t)
                             (identity candidate))))))
         ;; Be sure we don't erase the underlying minibuffer if some.
         (helm-ff-auto-update-initial-value
          (and helm-ff-auto-update-initial-value
               (not (minibuffer-window-active-p (minibuffer-window)))))
         helm-follow-mode-persistent
         (helm-ff-fuzzy-matching
          (and fuzzy
               (not (memq helm-mm-matching-method '(multi1 multi3p)))))
         (hist (and history (helm-comp-read-get-candidates
                             history nil nil alistp)))
         (helm-ff--RET-disabled noret)
         (minibuffer-completion-predicate test)
         (minibuffer-completing-file-name t)
         (helm--completing-file-name t)
         (helm-read-file-name-mode-line-string
          (replace-regexp-in-string "helm-maybe-exit-minibuffer"
                                    "helm-confirm-and-exit-minibuffer"
                                    helm-read-file-name-mode-line-string))
         (src-list
          (list
           ;; History source.
           (helm-build-sync-source (format "%s History" name)
             :header-name (lambda (name)
                            (concat name (substitute-command-keys
                                          helm-find-files-doc-header)))
             :mode-line mode-line
             :candidates hist
             :nohighlight t
             :fuzzy-match fuzzy
             :persistent-action-if persistent-action-if
             :persistent-help persistent-help
             :keymap helm-read-file-map
             :must-match must-match
             :nomark nomark
             :action action-fn)
           ;; Other source.
           (helm-build-sync-source name
             :header-name (lambda (name)
                            (concat name (substitute-command-keys
                                          helm-find-files-doc-header)))
             :init (lambda ()
                     (setq helm-ff-auto-update-flag
                           helm-ff-auto-update-initial-value)
                     (setq helm-ff--auto-update-state
                           helm-ff-auto-update-flag))
             :mode-line mode-line
             :help-message 'helm-read-file-name-help-message
             :nohighlight t
             :candidates
             (lambda ()
               (if test
                   (append (and (not (file-exists-p helm-pattern))
                                (not (helm-ff--invalid-tramp-name-p helm-pattern))
                                (list (helm-ff-filter-candidate-one-by-one
                                       helm-pattern nil t)))
                           (cl-loop with hn = (helm-ff--tramp-hostnames)
                                    ;; helm-find-files-get-candidates is
                                    ;; returning a list of cons cells.
                                    for (d . r) in (helm-find-files-get-candidates
                                                    must-match)
                                    when (or (member r hn) ; A tramp host
                                             (funcall test r)) ; Test ok
                                    collect (cons d r)))
                 (helm-find-files-get-candidates must-match)))
             :update (lambda ()
                       (remhash helm-ff-default-directory
                                helm-ff--list-directory-cache))
             :match-on-real t
             :filtered-candidate-transformer '(helm-ff-fct
                                               helm-ff-sort-candidates)
             :persistent-action-if persistent-action-if
             :persistent-help persistent-help
             :volatile t
             :keymap helm-read-file-map
             :must-match must-match
             :cleanup 'helm-find-files-cleanup
             :nomark nomark
             :action action-fn)))
         ;; Helm result.
         (result (helm
                  :sources (if helm-mode-reverse-history
                               (reverse src-list) src-list)
                  :input (expand-file-name initial-input)
                  :prompt prompt
                  :candidate-number-limit candidate-number-limit
                  :resume 'noresume
                  :case-fold-search case-fold
                  :default default
                  :buffer buffer
                  :full-frame nil
                  :preselect preselect)))
    (or
     (cond ((and result (stringp result)
                 (string= result "") ""))
           ((and result
                 (stringp result)
                 (file-equal-p result initial-input)
                 default)
            (if (listp default) (car default) default))
           ((and result (listp result))
            (mapcar #'expand-file-name result))
           ((and result (file-directory-p result))
            (file-name-as-directory (expand-file-name result)))
           (result (expand-file-name result)))
     (helm-mode--keyboard-quit))))

(defun helm-mode--default-filename (fname dir initial)
  (unless dir (setq dir default-directory))
  (unless (file-name-absolute-p dir)
    (setq dir (expand-file-name dir)))
  (unless (or fname (consp fname))
    (setq fname (expand-file-name
                 (or initial buffer-file-name dir)
                 dir)))
  (if (and fname (consp fname))
      (setq fname (cl-loop for f in fname
                           collect (expand-file-name f dir)))
      (if (file-name-absolute-p fname)
          fname (expand-file-name fname dir))))

(cl-defun helm--generic-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "Generic helm replacement of `read-file-name'.
Don't use it directly, use instead `helm-read-file-name' in your programs."
  (let* ((init (or initial dir default-directory))
         (current-command (or (helm-this-command) this-command))
         (str-command (helm-symbol-name current-command))
         (helm--file-completion-sources
          (cons str-command
                (remove str-command helm--file-completion-sources)))
         (buf-name (format "*helm-mode-%s*" str-command))
         (entry (assq current-command
                      helm-completing-read-handlers-alist))
         (def-com  (cdr-safe entry))
         (str-defcom (and def-com (helm-symbol-name def-com)))
         ;; Don't modify the original args list for emacs generic functions.
         (def-args (list prompt dir default-filename mustmatch initial predicate))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in helm specialized functions.
         (others-args (append def-args (list str-command buf-name)))
         (reading-directory (eq predicate 'file-directory-p))
         (use-dialog (and (next-read-file-uses-dialog-p)
                          ;; Graphical file dialogs can't handle
                          ;; remote files.
                          (not (file-remote-p init))
                          use-file-dialog))
         helm-completion-mode-start-message ; Be quiet
         helm-completion-mode-quit-message  ; Same here
         add-to-history fname)
    ;; Build `default-filename' with `dir'+`initial' when
    ;; `default-filename' is not specified.
    ;; See `read-file-name' docstring for more infos.
    (setq default-filename (helm-mode--default-filename
                            default-filename dir initial))
    ;; Some functions that normally call `completing-read' can switch
    ;; brutally to `read-file-name' (e.g find-tag), in this case
    ;; the helm specialized function will fail because it is build
    ;; for `completing-read', so set it to 'incompatible to be sure
    ;; we switch to `helm-read-file-name' and don't try to call it
    ;; with wrong number of args.
    (when (eq def-com 'ido)
      (setq def-com 'ido-read-file-name))
    (when (and def-com (> (length (help-function-arglist def-com)) 8))
      (setq def-com 'incompatible))
    (unless (or (not entry) def-com)
      (cl-return-from helm--generic-read-file-name
        (unwind-protect
             (progn
               (helm-mode -1)
               (apply read-file-name-function def-args))
          (helm-mode 1))))
    ;; If we use now `read-file-name' or dialog we MUST turn off `helm-mode'
    ;; to avoid infinite recursion and CRASH. It will be reenabled on exit.
    (when (or (memq def-com '(read-file-name ido-read-file-name))
              use-dialog
              (and (stringp str-defcom)
                   (not (string-match "^helm" str-defcom))))
      (helm-mode -1))
    (unwind-protect
         (setq fname
               (cond (use-dialog
                      (let ((dialog-mustmatch
                             (not (memq mustmatch
                                        '(nil confirm confirm-after-completion)))))
                        ;; Dialogs don't support a list of default fnames.
                        (when (and default-filename (consp default-filename))
                          (setq default-filename
                                (expand-file-name (car default-filename) init)))
                        (setq add-to-history t)
                        (x-file-dialog prompt init default-filename
                                       dialog-mustmatch
                                       reading-directory)))
                     ;; A specialized function exists, run it
                     ;; with the two extra args specific to helm.
                     ;; Note that the helm handler should ensure
                     ;; :initial-input is not nil i.e. Use init
                     ;; which fallback to default-directory instead
                     ;; of INITIAL.
                     ((and def-com helm-mode
                           (not (eq def-com 'ido-read-file-name))
                           (not (eq def-com 'incompatible)))
                      (apply def-com others-args))
                     (;; Def-com value is `ido-read-file-name'
                      ;; run it with default args.
                      (and def-com (eq def-com 'ido-read-file-name))
                      (ido-mode 1)
                      (apply def-com def-args))
                     (;; Def-com value is `read-file-name'
                      ;; run it with default args.
                      (eq def-com 'read-file-name)
                      (apply def-com def-args))
                     (t  ; Fall back to classic `helm-read-file-name'.
                      (helm-read-file-name
                       prompt
                       :name str-command
                       :buffer buf-name
                       :default default-filename
                       ;; Helm handlers should always have a non nil INITIAL arg.
                       :initial-input (expand-file-name init dir)
                       :alistp nil
                       :must-match mustmatch
                       :test predicate
                       :noret reading-directory))))
      (and ido-mode (ido-mode -1))
      (helm-mode 1)
      ;; Same comment as in `helm--completing-read-default'.
      (setq this-command current-command))
    (when add-to-history
      (add-to-history 'file-name-history
                      (minibuffer-maybe-quote-filename fname)))
    (if (and
         ;; Using `read-directory-name'.
         reading-directory
         ;; `file-name-as-directory' return "./" when FNAME is
         ;; empty string.
         (not (string= fname "")))
        (file-name-as-directory fname)
      fname)))

;; Read file name handler with history (issue #1652)
(defun helm-read-file-name-handler-1 (prompt dir default-filename
                                      mustmatch initial predicate
                                      name buffer)
  "A `read-file-name' handler with history.
Can be added to `helm-completing-read-handlers-alist' for functions
that need a `read-file-name' function with directory history.
The `helm-find-files' history `helm-ff-history' is used here."
  (let ((helm-always-two-windows t)
        (helm-split-window-default-side
         (if (eq helm-split-window-default-side 'same)
             'below helm-split-window-default-side))
        helm-split-window-inside-p
        helm-reuse-last-window-split-state
        ;; Helm handlers should always have a non nil INITIAL arg.
        (init (or initial dir default-directory)))
    (helm-read-file-name
     prompt
     :name name
     :history helm-ff-history
     :buffer buffer
     :default default-filename
     :initial-input (expand-file-name init dir)
     :alistp nil
     :must-match mustmatch
     :test predicate)))


;;; Completion in region and Helm style
;;
(defun helm-mode--advice-lisp--local-variables (old--fn &rest args)
  (ignore-errors
    (apply old--fn args)))

(defvar helm-completion--sorting-done nil
  "Flag that notifies the FCT if sorting has been done in completion function.")
(defun helm-completion-in-region-sort-fn (candidates _source)
  "Default sort function for completion-in-region."
  (if helm-completion--sorting-done
      candidates
    (sort candidates 'helm-generic-sort-fn)))

(defun helm-mode--completion-in-region-initial-input (str)
  "Highlight prefix in helm and helm-fuzzy `helm-completion-styles'."
  (if (memq helm-completion-style '(helm helm-fuzzy))
      (propertize str 'read-only t 'face 'helm-mode-prefix 'rear-nonsticky t)
    str))

(defun helm-completion-in-region--initial-filter (comps afun file-comp-p)
  "Add annotations at end of candidates and filter out dot files."
  (if file-comp-p
      ;; Filter out dot files in file completion.
      (cl-loop for f in comps unless
               (string-match "\\`\\.\\{1,2\\}/\\'" f)
               collect f)
    (if afun
        ;; Add annotation at end of
        ;; candidate if needed, e.g. foo<f>, this happen when
        ;; completing against a quoted symbol.
        (mapcar (lambda (s)
                  (let ((ann (funcall afun s)))
                    (if ann
                        (cons
                         (concat
                          s
                          (propertize
                           " " 'display
                           (propertize
                            ann
                            'face 'completions-annotations)))
                         s)
                      s)))
                comps)
      comps)))

;; Helm multi matching style

(defun helm-completion-try-completion (string table pred point)
  "The try completion function for `completing-styles-alist'.
Actually does nothing."
  ;; AFAIU the try-completions style functions
  ;; are here to check if what is at point is suitable for TABLE but
  ;; there is no way to pass a multiple pattern from what is at point
  ;; apart sending STRING in a minibuffer like helm does.  Perhaps
  ;; minibuffer-complete should benefit of this but for now just do
  ;; nothing as this is used nowhere.  It is anyway not clear what the
  ;; try-completions functions do in emacs so just do nothing for now.
  (ignore string table pred point))

(defun helm-completion-all-completions (string table pred point)
  "The all completions function for `completing-styles-alist'."
  ;; FIXME: No need to bind all these value.
  (cl-multiple-value-bind (all _pattern prefix _suffix _carbounds)
      (helm-completion--multi-all-completions string table pred point)
    (when all (nconc all (length prefix)))))

(defun helm-completion--multi-all-completions-1 (string collection &optional predicate)
  "Allow `all-completions' multi matching on its candidates."
  ;; Doing an initial call of all-completions on the first element of
  ;; STRING speedup completion and fix file completion when CAPF
  ;; returns relative paths to initial pattern (eshell and shell).
  (let* ((split (helm-mm-split-pattern string))
         (fpat (or (car split) ""))
         (file-comp-p (or minibuffer-completing-file-name
                          (eq
                           (completion-metadata-get
                            (completion-metadata string collection predicate)
                            'category)
                           'file)))
         (all (and file-comp-p
                   (or (cdr split)
                       (and (not (cdr split))
                            ;; Kickin when STRING is a simple string.
                            ;; Handle as well "foo " (space at end).
                            (not (string= fpat "")))
                       (string= string ""))
                   (not (string-match "\\`!" fpat))
                   ;; all-completions should return nil if FPAT is a
                   ;; regexp, it is what we expect.
                   (all-completions fpat collection
                                    (lambda (x &optional _y)
                                      (let ((elm (if (listp x) (car x) x)))
                                        (funcall (or predicate #'identity) elm))))))
         (pattern (helm-aand all (string-match " " string)
                             ;; Returns the part of STRING after space
                             ;; e.g. "foo bar baz" => "bar baz".
                             (substring string (1+ it)))))
    (if (or (and all (not (cdr split)))
            (equal pattern "")) ; e.g. STRING == "foo ".
        all
      (all-completions "" (or all collection)
                       (lambda (x &optional _y)
                         ;; Second arg _y is needed when
                         ;; COLLECTION is a hash-table issue
                         ;; #2231 (C-x 8 RET).
                         ;; Elements of COLLECTION may be
                         ;; lists or alists, in this case consider the
                         ;; car of element (issue #2219 org-refile).
                         (let ((elm (if (listp x) (car x) x)))
                           ;; PREDICATE have been already called in
                           ;; initial all-completions, no need to call
                           ;; it a second time, thus ALL is now a list
                           ;; of strings maybe not supported by
                           ;; PREDICATE (e.g. symbols vs strings).
                           (if (and predicate (null all))
                               (and (funcall predicate elm)
                                    ;; ALL is nil so use whole STRING
                                    ;; against COLLECTION.
                                    (helm-mm-match (helm-stringify elm) string))
                             (helm-mm-match (helm-stringify elm)
                                            (or (and all pattern) string)))))))))

(defun helm-completion--multi-all-completions (string table pred point)
  "Collect completions from TABLE for helm completion style."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (prefix (substring beforepoint 0 (car bounds)))
         (suffix (substring afterpoint (cdr bounds)))
         (all (helm-completion--multi-all-completions-1
               (regexp-quote string) table pred)))
    (list all string prefix suffix point)))

;; The adjust-metadata functions run only in emacs-27, they are NOT
;; used otherwise.
(defun helm-completion--adjust-metadata (metadata)
  (if (memq helm-completion-style '(helm helm-fuzzy))
      metadata
    (let ((compose-helm-sort-fn
           (lambda (candidates)
             (sort candidates #'helm-generic-sort-fn))))
      `(metadata
        (display-sort-function
         . ,compose-helm-sort-fn)
        (cycle-sort-function
         . ,compose-helm-sort-fn)
        ,@(cdr metadata)))))
(put 'helm 'completion--adjust-metadata 'helm-completion--adjust-metadata)

;; Helm-flex style.
;; This is more or less the same as emacs-27 flex style.
(defun helm-flex-completion-try-completion (string table pred point)
  "The try completion function for `completing-styles-alist'."
  ;; It is needed here to make minibuffer-complete work in emacs-26,
  ;; e.g. with regular M-x.
  (unless (string-match-p " " string)
    (cl-multiple-value-bind (all pattern prefix suffix _carbounds)
        (helm-completion--flex-all-completions string table pred point)
      (when minibuffer-completing-file-name
        (setq all (completion-pcm--filename-try-filter all)))
      (completion-pcm--merge-try pattern all prefix suffix))))

(defun helm-flex-completion-all-completions (string table pred point)
  "The all completions function for `completing-styles-alist'."
  ;; FIXME: No need to bind all these value.
  (unless (string-match-p " " string)
    (cl-multiple-value-bind (all pattern prefix _suffix _carbounds)
        (helm-completion--flex-all-completions
         string table pred point
         #'helm-completion--flex-transform-pattern)
      (let ((regexp (completion-pcm--pattern->regex pattern 'group)))
        (when all (nconc (helm-flex-add-score-as-prop all regexp)
                         (length prefix)))))))

(defun helm-flex-add-score-as-prop (candidates regexp)
  (cl-loop with case-fold-search = (helm-set-case-fold-search)
           for cand in candidates
           collect (helm-flex--style-score cand regexp)))

(defun helm-completion--flex-transform-pattern (pattern)
  ;; "fob" => '(prefix "f" any "o" any "b" any point)
  (cl-loop for p in pattern
           if (stringp p) nconc
           (cl-loop for str across p
                    nconc (list (string str) 'any))
           else nconc (list p)))

;; Same as emacs-27 completion-substring--all-completions.
(defun helm-completion--flex-all-completions
    (string table pred point &optional transform-pattern-fn)
  "Match the presumed substring STRING to the entries in TABLE.
Respect PRED and POINT.  The pattern used is a PCM-style substring
pattern, but it will be massaged by TRANSFORM-PATTERN-FN, if that
is non-nil."
  (let* ((beforepoint (substring string 0 point))
         (afterpoint (substring string point))
         (bounds (completion-boundaries beforepoint table pred afterpoint))
         (suffix (substring afterpoint (cdr bounds)))
         (prefix (substring beforepoint 0 (car bounds)))
         (basic-pattern (completion-basic--pattern
                         beforepoint afterpoint bounds))
         (pattern (if (not (stringp (car basic-pattern)))
                      basic-pattern
                    (cons 'prefix basic-pattern)))
         (pattern (if transform-pattern-fn
                       (funcall transform-pattern-fn pattern)
                     pattern))
         (all (completion-pcm--all-completions prefix pattern table pred)))
    (list all pattern prefix suffix (car bounds))))

(defun helm-completion-in-region--selection ()
  (with-helm-buffer
    (setq helm-saved-selection (helm-get-selection nil 'withprop))))

;; Completion-in-region-function

(defun helm--completion-in-region (origfun start end collection &optional predicate)
  "Helm replacement of `completion--in-region'.

Can be used for `completion-in-region-function' by advicing it with an
:around advice to allow passing the old
`completion-in-region-function' value in ORIGFUN."
  (cl-declare (special require-match prompt))
  (if (memq major-mode helm-mode-no-completion-in-region-in-modes)
      (funcall origfun start end collection predicate)
    (advice-add
     'lisp--local-variables
     :around #'helm-mode--advice-lisp--local-variables)
    (let ((old--helm-completion-style helm-completion-style)
          (exit-fun (plist-get completion-extra-properties :exit-function))
          ;; Always start with prefix to allow completing without
          ;; the need of inserting a space after cursor or
          ;; relaying on crap old completion-styles emacs22 which
          ;; add suffix after prefix. e.g. def|else.
          (initial-input (buffer-substring-no-properties start (point)))
          string)
      (helm-aif (cdr (assq major-mode helm-completion-styles-alist))
          (customize-set-variable 'helm-completion-style
                                  (if (cdr-safe it) (car it) it)))
      ;; This hook force usage of the display part of candidate with
      ;; its properties, this is needed for lsp-mode in its
      ;; :exit-function see issue #2265.
      (add-hook 'helm-before-action-hook 'helm-completion-in-region--selection)
      (unwind-protect
          (let* ((enable-recursive-minibuffers t)
                 (completion-flex-nospace t)
                 (completion-styles (helm--prepare-completion-styles))
                 (input (buffer-substring-no-properties start end))
                 (prefix (and (eq helm-completion-style 'emacs) initial-input))
                 (point (point))
                 (current-command (or (helm-this-command)
                                      this-command
                                      ;; Some backends are async and
                                      ;; use a callback, in those
                                      ;; cases, we can't retrieve from
                                      ;; frames the last interactive
                                      ;; command, so fallback to
                                      ;; `last-command' which may be
                                      ;; the one that called the callback.
                                      last-command))
                 (crm (eq current-command 'crm-complete))
                 (str-command (helm-symbol-name current-command))
                 (buf-name (format "*helm-mode-%s*" str-command))
                 (require-match (or (and (boundp 'require-match) require-match)
                                    minibuffer-completion-confirm
                                    ;; If prompt have not been propagated here, that's
                                    ;; probably mean we have no prompt and we are in
                                    ;; completion-at-point or friend, so use a non--nil
                                    ;; value for require-match.
                                    (not (boundp 'prompt))))
                 (metadata (completion-metadata input collection predicate))
                 ;; `completion-extra-properties' is let-bounded in `completion-at-point'.
                 ;; `afun' is a closure to call against each string in `data'.
                 ;; it provide the annotation info for each string.
                 ;; e.g "foo" => "foo <f>" where foo is a function.
                 ;; See Issue #407.
                 (afun (or (plist-get completion-extra-properties :annotation-function)
                           (completion-metadata-get metadata 'annotation-function)))
                 (init-space-suffix (unless (or (memq helm-completion-style '(helm-fuzzy emacs))
                                                (string-suffix-p " " input)
                                                (string= input ""))
                                      " "))
                 (file-comp-p (or (eq (completion-metadata-get metadata 'category) 'file)
                                  (helm-mode--in-file-completion-p)
                                  ;; Assume that when `afun' and `predicate' are null
                                  ;; we are in filename completion.
                                  (and (null afun) (null predicate))))
                 ;; `completion-all-completions' store the base-size in the last `cdr',
                 ;; so data looks like this: '(a b c d . 0) and (last data) == (d . 0).
                 base-size
                 (compfn (lambda (str _predicate _action)
                           (let* ((completion-ignore-case (helm-set-case-fold-search))
                                  (comps
                                   (completion-all-completions
                                    str ; This is helm-pattern
                                    collection
                                    predicate
                                    ;; Use prefix length at first call to
                                    ;; allow styles matching
                                    ;; "prefix*suffix" to kick in.
                                    (length (or prefix str))
                                    metadata))
                                  (last-data (last comps))
                                  (bs (helm-aif (cdr last-data)
                                          (prog1 it
                                            ;; Remove the last element of
                                            ;; comps by side-effect.
                                            (setcdr last-data nil))
                                        0))
                                  ;; Helm syle sort fn is added to
                                  ;; metadata only in emacs-27, so in
                                  ;; emacs-26 use helm-generic-sort-fn
                                  ;; which handle both helm and
                                  ;; helm-flex styles. When
                                  ;; helm-completion-style is helm or
                                  ;; helm-fuzzy, sorting will be done
                                  ;; later in FCT.
                                  (sort-fn
                                   (and (eq helm-completion-style 'emacs)
                                        (or
                                         ;; Emacs-27
                                         (completion-metadata-get
                                          metadata 'display-sort-function)
                                         ;; Emacs-26
                                         (lambda (candidates)
                                           (sort candidates #'helm-generic-sort-fn)))))
                                  all)
                             ;; Reset prefix to allow using length of
                             ;; helm-pattern on next calls (this avoid
                             ;; args-out-of-range error).
                             (and prefix (setq prefix nil))
                             ;; base-size needs to be set only once at
                             ;; first call.
                             (unless base-size (setq base-size bs))
                             (setq helm-completion--sorting-done (and sort-fn t))
                             (setq all (copy-sequence comps))
                             (helm-completion-in-region--initial-filter
                              (if (and sort-fn (> (length str) 0))
                                  (funcall sort-fn all)
                                all)
                              afun file-comp-p))))
                 (data (if (memq helm-completion-style '(helm helm-fuzzy))
                           (funcall compfn input nil nil)
                         compfn))
                 (result (if (stringp data)
                             data
                           (helm-comp-read
                            ;; Completion-at-point and friends have no prompt.
                            (or (and (boundp 'prompt) prompt) "Pattern: ")
                            data
                            :name str-command
                            :nomark (null crm)
                            :marked-candidates crm
                            :initial-input
                            (cond ((and file-comp-p
                                        (not (string-match "/\\'" initial-input)))
                                   (concat (helm-mode--completion-in-region-initial-input
                                            (if (memq helm-completion-style '(helm helm-fuzzy))
                                                (helm-basename initial-input)
                                              initial-input))
                                           init-space-suffix))
                                  ((string-match "/\\'" initial-input)
                                   (and (eq helm-completion-style 'emacs) initial-input))
                                  ((or (null require-match)
                                       (stringp require-match))
                                   (helm-mode--completion-in-region-initial-input initial-input))
                                  (t (concat (helm-mode--completion-in-region-initial-input initial-input)
                                             init-space-suffix)))
                            :buffer buf-name
                            :fc-transformer
                            ;; Ensure sort fn is at the end.
                            (append '(helm-cr-default-transformer)
                                    (and helm-completion-in-region-default-sort-fn
                                         (list helm-completion-in-region-default-sort-fn)))
                            :match-dynamic (eq helm-completion-style 'emacs)
                            :fuzzy (eq helm-completion-style 'helm-fuzzy)
                            :exec-when-only-one t
                            :quit-when-no-cand
                            (lambda ()
                              ;; Delay message to overwrite "Quit".
                              (run-with-timer
                               0.01 nil
                               (lambda ()
                                 (message "[No matches]")))
                              t)        ; exit minibuffer immediately.
                            :must-match require-match))))
            ;; `helm-completion-in-region--insert-result' is stripping
            ;; out properties on RESULT by side-effect (perhaps
            ;; `choose-completion-string'?) so make a copy of STRING
            ;; to not loose props.
            (setq string (copy-sequence result))
            (helm-completion-in-region--insert-result
             result start point end base-size))
        ;; Allow running extra property `:exit-function' (Issues #2265,
        ;; #2356). Function is called with 'exact if for a unique
        ;; match which is exact, the return value of `try-completion'
        ;; is t, otherwise it is called with 'finished.
        (when (and (stringp string) exit-fun)
          (funcall exit-fun string
                   (if (eq (try-completion initial-input collection) t)
                       'exact 'finished)))
        (remove-hook 'helm-before-action-hook 'helm-completion-in-region--selection)
        (customize-set-variable 'helm-completion-style old--helm-completion-style)
        (setq helm-completion--sorting-done nil)
        (advice-remove 'lisp--local-variables
                       #'helm-mode--advice-lisp--local-variables)))))

(defvar helm-crm-default-separator ","
  "Default separator for `completing-read-multiple'.

`crm-separator' will take precedence on this when it is a string composed
of a single character.
If used globally, it is a string composed of a single character,
if let-bounded, it can be also nil or a symbol which mean no
separator.  Don't set this to a string composed of more than one
character.
Be sure to know what you are doing when modifying this.")
(defun helm-completion-in-region--insert-result (result start point end base-size)
  (cond ((stringp result)
         ;; When RESULT have annotation, annotation is displayed
         ;; in it with a display property attached to a space
         ;; added at end of string, take care of removing this
         ;; space (issue #2360). However keep RESULT intact to
         ;; pass it to `:exit-function' i.e. Don't store the
         ;; modified string in STRING.
         (choose-completion-string
          (replace-regexp-in-string " \\'" "" result)
          (current-buffer)
          (list (+ start base-size) point)
          completion-list-insert-choice-function)
         (when helm-completion-mark-suffix
           (run-with-idle-timer 0.01 nil
                                (lambda ()
                                  (helm-aand
                                   (+ (- (point) point) end)
                                   (and (> it (point)) it)
                                   (push-mark  it t t))))))
        ((consp result)                 ; crm.
         (let ((beg (+ start base-size))
               (sep (or (and
                         ;; If `crm-separator' is a string of length 1
                         ;; assume it can be used as separator (#2298),
                         ;; otherwise it is a regexp and use the value
                         ;; it matches or default to "," if no match.
                         (eq (length crm-separator) 1)
                         crm-separator)
                        helm-crm-default-separator)))
           ;; Try to find a default separator. If `crm-separator' is a
           ;; regexp use the string the regexp is matching.
           ;; If SEP is not a string, it have been probably bound to a
           ;; symbol or nil through `helm-crm-default-separator' that serve
           ;; as a flag to say "Please no separator" (Issue #2353 with
           ;; `magit-completing-read-multiple').
           (if (stringp sep)
               (save-excursion
                 (goto-char beg)
                 (when (looking-back crm-separator (1- (point)))
                   (setq sep (match-string 0))))
             (setq sep nil))
           (funcall completion-list-insert-choice-function
                    beg end (mapconcat 'identity (append result '("")) sep))))
        (t nil)))

(defun helm-mode--in-file-completion-p ()
  (with-helm-current-buffer
    (run-hook-with-args-until-success 'file-name-at-point-functions)))

(defun helm-mode--disable-ido-maybe (&optional from-hook)
  (when (and (boundp 'ido-everywhere) ido-everywhere)
    (remove-function read-file-name-function #'ido-read-file-name)
    (remove-function read-buffer-function #'ido-read-buffer)
    (setq ido-everywhere nil)
    (if from-hook
        (user-error "Unable to turn on Ido-everywhere while Helm-mode is enabled")
      (user-error "Helm-mode enabled (Ido-everywhere is incompatible with Helm-mode, disabling it)"))))

(defun helm-mode--ido-everywhere-hook ()
  ;; Called only when user calls directly ido-everywhere
  ;; and helm-mode is enabled.
  (when helm-mode
    (helm-mode--disable-ido-maybe t)))

;;;###autoload
(define-minor-mode helm-mode
    "Toggle generic helm completion.

All functions in Emacs that use `completing-read',
`read-file-name', `completion-in-region' and friends will use helm
interface when this mode is turned on.

However you can modify this behavior for functions of your choice
with `helm-completing-read-handlers-alist'.

Called with a positive arg, turn on unconditionally, with a
negative arg turn off.
You can toggle it with M-x `helm-mode'.

About `ido-mode':
DO NOT enable `ido-everywhere' when using `helm-mode'.  Instead of
using `ido-mode', add the commands where you want to use ido to
`helm-completing-read-handlers-alist' with `ido' as value.

Note: This mode is incompatible with Emacs23."
  :group 'helm-mode
  :global t
  :lighter helm-completion-mode-string
  (cl-assert (boundp 'completing-read-function) nil
             "`helm-mode' not available, upgrade to Emacs-24")
  (if helm-mode
      (progn
        (add-function :override completing-read-function
                      #'helm--completing-read-default)
        (add-function :override read-file-name-function
                      #'helm--generic-read-file-name)
        (add-function :override read-buffer-function
                      #'helm--generic-read-buffer)
        (when helm-mode-handle-completion-in-region
          (add-function :around completion-in-region-function
                        #'helm--completion-in-region))
        ;; If user have enabled ido-everywhere BEFORE enabling
        ;; helm-mode disable it and warn user about its
        ;; incompatibility with helm-mode (issue #2085).
        (helm-mode--disable-ido-maybe)
        ;; If ido-everywhere is not enabled yet anticipate and
        ;; disable it if user attempt to enable it while helm-mode
        ;; is running (issue #2085).
        (add-hook 'ido-everywhere-hook #'helm-mode--ido-everywhere-hook)
        (when (fboundp 'ffap-read-file-or-url-internal)
          ;; `ffap-read-file-or-url-internal' have been removed in
          ;; emacs-27 and `ffap-read-file-or-url' is fixed, so no need
          ;; to advice it.
          (advice-add 'ffap-read-file-or-url :override #'helm-advice--ffap-read-file-or-url))
        (advice-add 'read-buffer-to-switch :override #'helm-mode--read-buffer-to-switch))
    (progn
      (remove-function completing-read-function #'helm--completing-read-default)
      (remove-function read-file-name-function #'helm--generic-read-file-name)
      (remove-function read-buffer-function #'helm--generic-read-buffer)
      (remove-function completion-in-region-function #'helm--completion-in-region)
      (remove-hook 'ido-everywhere-hook #'helm-mode--ido-everywhere-hook)
      (when (fboundp 'ffap-read-file-or-url-internal)
        (advice-remove 'ffap-read-file-or-url #'helm-advice--ffap-read-file-or-url))
      (advice-remove 'read-buffer-to-switch #'helm-mode--read-buffer-to-switch))))

(provide 'helm-mode)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-mode.el ends here
