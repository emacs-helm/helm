;;; helm-mode.el --- Enable helm completion everywhere.

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
(require 'helm-files)


(defgroup helm-mode nil
  "Enable helm completion."
  :group 'helm)

(defcustom helm-completing-read-handlers-alist
  '((describe-function . helm-completing-read-symbols)
    (describe-variable . helm-completing-read-symbols)
    (debug-on-entry . helm-completing-read-symbols)
    (find-function . helm-completing-read-symbols)
    (trace-function . helm-completing-read-symbols)
    (trace-function-background . helm-completing-read-symbols)
    (find-tag . helm-completing-read-with-cands-in-buffer)
    (ffap-alternate-file . nil))
  "Alist of handlers to replace `completing-read', `read-file-name' in `helm-mode'.
Each entry is a cons cell like \(emacs_command . completing-read_handler\)
where key and value are symbols.

Each key is an Emacs command that use originaly `completing-read'.

Each value maybe an helm function that take same arguments as
`completing-read' plus NAME and BUFFER, where NAME is the name of the new
helm source and BUFFER the name of the buffer we will use.
This function prefix name must start by \"helm\".

See `helm-completing-read-symbols' for example.

If the value of an entry is nil completion will fall back to
emacs vanilla behavior.
e.g If you want to disable helm completion for `describe-function':
\(describe-function . nil\).

Ido is also supported, you can use `ido-completing-read' and
`ido-read-file-name' as value of an entry or just 'ido.
e.g ido completion for `find-file':
\(find-file . ido\)
same as
\(find-file . ido-read-file-name\)
Note that you don't need to enable `ido-mode' for this to work."
  :group 'helm-mode
  :type '(alist :key-type symbol :value-type symbol))

(defcustom helm-comp-read-case-fold-search helm-case-fold-search
  "Default Local setting of `helm-case-fold-search' for `helm-comp-read'.
See `helm-case-fold-search' for more info."
  :group 'helm-mode
  :type 'symbol)


(defvar helm-comp-read-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "<C-return>") 'helm-cr-empty-string)
    map)
  "Keymap for `helm-comp-read'.")


;;; Internal
;;
;;
;; Flag to know if `helm-pattern' have been added
;; to candidate list in `helm-comp-read'.
(defvar helm-cr-unknow-pattern-flag nil)


;;; Helm `completing-read' replacement
;;
;;
;;;###autoload
(defun helm-cr-empty-string ()
  "Return empty string."
  (interactive)
  (helm-c-quit-and-execute-action
   #'(lambda (_candidate)
       (identity ""))))

(defun helm-comp-read-get-candidates (collection &optional test sort-fn alistp)
  "Convert COLLECTION to list removing elements that don't match TEST.
See `helm-comp-read' about supported COLLECTION arguments.

SORT-FN is a predicate to sort COLLECTION.

ALISTP when non--nil will not use `all-completions' to collect
candidates because it doesn't handle alists correctly for helm.
i.e In `all-completions' the car of each pair is used as value.
In helm we want to use the cdr instead like \(display . real\),
so we return the alist as it is with no transformation by all-completions.

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

If COLLECTION is an `obarray', a TEST should be needed. See `obarray'."
  (let ((cands
         (cond ((eq collection obarray)
                (all-completions "" collection test))
               ((and (vectorp collection) test)
                (loop for i across collection when (funcall test i) collect i))
               ((vectorp collection)
                (loop for i across collection collect i))
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
                (let ((predicate `(lambda (elm)
                                    (if (or (eq (quote ,test) 'commandp)
                                            (eq (quote ,test) 'functionp)
                                            (eq (quote ,test) 'symbolp))
                                        (funcall (quote ,test) (intern elm))
                                        (funcall (quote ,test) elm)))))
                  (all-completions "" (symbol-value collection) predicate)))
               ((and (symbolp collection) (boundp collection))
                (all-completions "" (symbol-value collection)))
               ((and alistp test)
                (loop for i in collection when (funcall test i) collect i))
               (alistp collection)
               (t (all-completions "" collection test)))))
    (if sort-fn (sort cands sort-fn) cands)))

(defun helm-cr-default-transformer (candidates source)
  "Default filter candidate function for `helm-comp-read'."
  (loop for cand in candidates
        if (and (equal cand helm-pattern)
                helm-cr-unknow-pattern-flag)
        collect
        (cons (concat (propertize " " 'display (propertize
                                                "[?]" 'face 'helm-ff-prefix))
                      cand) cand)
        else collect cand))

;;;###autoload
(defun* helm-comp-read (prompt collection
                               &key
                               test
                               initial-input
                               default
                               preselect
                               (buffer "*Helm Completions*")
                               must-match
                               (requires-pattern 0)
                               (history nil)
                               input-history
                               (case-fold helm-comp-read-case-fold-search)
                               (del-input t)
                               (persistent-action nil)
                               (persistent-help "DoNothing")
                               (mode-line helm-comp-read-mode-line)
                               (keymap helm-comp-read-map)
                               (name "Helm Completions")
                               candidates-in-buffer
                               exec-when-only-one
                               (volatile t)
                               sort
                               (fc-transformer 'helm-cr-default-transformer)
                               (marked-candidates nil)
                               (alistp t))
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

- REQUIRES-PATTERN: Same as helm attribute, default is 0.

- HISTORY: A list containing specific history, default is nil.
  When it is non--nil, all elements of HISTORY are displayed in
  a special source before COLLECTION.

- INPUT-HISTORY: A symbol. the minibuffer input history will be
  stored there, if nil or not provided, `minibuffer-history'
  will be used instead.

- CASE-FOLD: Same as `helm-case-fold-search'.

- DEL-INPUT: Boolean, when non--nil (default) remove the partial
  minibuffer input from HISTORY is present.

- PERSISTENT-ACTION: A function called with one arg i.e candidate.

- PERSISTENT-HELP: A string to document PERSISTENT-ACTION.

- MODE-LINE: A string or list to display in mode line.
  (See `helm-mode-line-string')

- KEYMAP: A keymap to use in this `helm-comp-read'.
  (The keymap will be shared with history source)

- NAME: The name related to this local source.

- EXEC-WHEN-ONLY-ONE: Bound `helm-execute-action-at-once-if-one'
  to non--nil. (possibles values are t or nil).

- VOLATILE: Use volatile attribute \(enabled by default\).

- SORT: A predicate to give to `sort' e.g `string-lessp'.

- FC-TRANSFORMER: A `filtered-candidate-transformer' function.

- MARKED-CANDIDATES: If non--nil return candidate or marked candidates as a list.

- ALISTP: \(default is non--nil\) See `helm-comp-read-get-candidates'.

- CANDIDATES-IN-BUFFER: when non--nil use a source build with
  `helm-candidates-in-buffer' which is much faster.
  Argument VOLATILE have no effect when CANDIDATES-IN-BUFFER is non--nil.

Any prefix args passed during `helm-comp-read' invocation will be recorded
in `helm-current-prefix-arg', otherwise if prefix args were given before
`helm-comp-read' invocation, the value of `current-prefix-arg' will be used.
That's mean you can pass prefix args before or after calling a command
that use `helm-comp-read' See `helm-M-x' for example."
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))
  (let ((action-fn '(("Sole action (Identity)"
                      . (lambda (candidate)
                          (if marked-candidates
                              (helm-marked-candidates)
                              (identity candidate)))))))
    ;; Assume completion have been already required,
    ;; so always use 'confirm.
    (when (eq must-match 'confirm-after-completion)
      (setq must-match 'confirm))
    (let* ((minibuffer-completion-confirm must-match)
           (must-match-map (when must-match
                             (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "RET")
                                 'helm-confirm-and-exit-minibuffer)
                               map)))
           (loc-map (if must-match-map
                        (make-composed-keymap
                         must-match-map (or keymap helm-map))
                        (or keymap helm-map)))
           (helm-read-file-name-mode-line-string
            (replace-regexp-in-string "helm-exit-minibuffer"
                          "helm-confirm-and-exit-minibuffer"
                          helm-read-file-name-mode-line-string))
           (src-hist `((name . ,(format "%s History" name))
                       (candidates
                        . (lambda ()
                            (let ((all (helm-comp-read-get-candidates
                                        history test nil ,alistp)))
                              (delete
                               ""
                               (helm-fast-remove-dups
                                (if (and default (not (string= default "")))
                                    (delq nil (cons default
                                                    (delete default all)))
                                    all)
                                :test 'equal)))))
                       (filtered-candidate-transformer
                        . (lambda (candidates sources)
                            (loop for i in candidates
                                  do (set-text-properties 0 (length i) nil i)
                                  collect i)))
                       (persistent-action . ,persistent-action)
                       (persistent-help . ,persistent-help)
                       (mode-line . ,mode-line)
                       (action . ,action-fn)))
           (src `((name . ,name)
                  (candidates
                   . (lambda ()
                       (let ((cands (helm-comp-read-get-candidates
                                     collection test sort alistp)))
                         (setq helm-cr-unknow-pattern-flag nil)
                         (unless (or (eq must-match t) (string= helm-pattern "")
                                     (assoc helm-pattern cands)
                                     (assoc (intern helm-pattern) cands)
                                     (member helm-pattern cands))
                           (setq cands (append (list helm-pattern) cands))
                           (setq helm-cr-unknow-pattern-flag t))
                         (if (and default (not (string= default "")))
                             (delq nil (cons default (delete default cands)))
                             cands))))
                  (filtered-candidate-transformer . ,fc-transformer)
                  (requires-pattern . ,requires-pattern)
                  (persistent-action . ,persistent-action)
                  (persistent-help . ,persistent-help)
                  (mode-line . ,mode-line)
                  (action . ,action-fn)))
           (src-1 `((name . ,name)
                    (init
                     . (lambda ()
                         (let ((cands (helm-comp-read-get-candidates
                                       collection test sort alistp)))
                           (unless (or (eq must-match t) (string= helm-pattern "")
                                       (assoc helm-pattern cands)
                                       (assoc (intern helm-pattern) cands)
                                       (member helm-pattern cands))
                             (setq cands (append (list helm-pattern) cands))
                             (setq helm-cr-unknow-pattern-flag t))
                           (with-current-buffer (helm-candidate-buffer 'global)
                             (loop for i in
                                   (if (and default (not (string= default "")))
                                       (delq nil (cons default (delete default cands)))
                                       cands)
                                   do (insert (concat i "\n")))))))
                    (candidates-in-buffer)
                    (filtered-candidate-transformer . ,fc-transformer)
                    (requires-pattern . ,requires-pattern)
                    (persistent-action . ,persistent-action)
                    (persistent-help . ,persistent-help)
                    (mode-line . ,mode-line)
                    (action . ,action-fn)))
           (src-list (list src-hist
                           (if candidates-in-buffer
                               src-1
                               (if volatile
                                   (append src '((volatile)))
                                   src))))
           (helm-execute-action-at-once-if-one exec-when-only-one)
           result)
      (setq result (helm
                    :sources src-list
                    :input initial-input
                    :default default
                    :preselect preselect
                    :prompt prompt
                    :resume 'noresume
                    :case-fold-search case-fold
                    :keymap loc-map
                    :history (and (symbolp input-history) input-history)
                    :buffer buffer))
      ;; Avoid adding an incomplete input to history.
      (when (and result history del-input)
        (cond (;; Be sure history is not a symbol with a nil value.
               (and (symbolp history)
                    (symbol-value history))
               (setcar (eval history) result))
              (;; A list with a non--nil value.
               (consp history) (setcar history result))
              (t ; Possibly a symbol with a nil value.
               (set history (list result)))))
      (or
       result
       (when (and (eq helm-exit-status 0)
                  (eq must-match 'confirm))
         ;; Return empty string only if it is the DEFAULT
         ;; value and helm-pattern is empty.
         ;; otherwise return helm-pattern
         (if (and (string= helm-pattern "") default)
             default (identity helm-pattern)))
       (unless (or (eq helm-exit-status 1)
                   must-match)  ; FIXME this should not be needed now.
         default)
       (keyboard-quit)))))

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
    (prompt collection test require-match init
     hist default inherit-input-method name buffer)
  "Specialized function for fast symbols completion in `helm-mode'."
  (or
   (helm
    :sources `((name . ,name)
               (init . (lambda ()
                         (with-current-buffer (helm-candidate-buffer 'global)
                           (goto-char (point-min))
                           (when (and default (stringp default)
                                      ;; Some defaults args result as
                                      ;; (symbol-name nil) == "nil".
                                      ;; e.g debug-on-entry.
                                      (not (string= default "nil"))
                                      (not (string= default "")))
                             (insert (concat default "\n")))
                           (loop with all = (all-completions "" collection test)
                                 for sym in all
                                 unless (and default (eq sym default))
                                 do (insert (concat sym "\n"))))))
               (persistent-action . helm-lisp-completion-persistent-action)
               (persistent-help . "Show brief doc in mode-line")
               (candidates-in-buffer)
               (action . identity))
    :prompt prompt
    :buffer buffer
    :input init
    :history hist
    :resume 'noresume
    :default (or default ""))
   (keyboard-quit)))


;;; Generic completing read
;;
;;
(defun helm-completing-read-default-1
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer &optional cands-in-buffer exec-when-only-one)
  "Call `helm-comp-read' with same args as `completing-read'.
Extra optional arg CANDS-IN-BUFFER mean use `candidates-in-buffer'
method which is faster.
It should be used when candidate list don't need to rebuild dynamically."
  (let ((history (or (car-safe hist) hist)))
    (when (and default (listp default))
      ;; When DEFAULT is a list move the list on head of COLLECTION
      ;; and set it to its car. #bugfix `grep-read-files'.
      (setq collection (if (listp collection)
                           (append default collection)
                           ;; Else COLLECTION is maybe a function or a table.
                           (append default (all-completions "" collection))))
      (setq default (car default)))
    (helm-comp-read
     prompt collection
     :test test
     :history history
     :input-history history
     :must-match require-match
     :alistp nil ; Be sure `all-completions' is used.
     :name name
     :requires-pattern (if (and (string= default "")
                                (or (eq require-match 'confirm)
                                    (eq require-match
                                        'confirm-after-completion)))
                           1 0)
     :candidates-in-buffer cands-in-buffer
     :exec-when-only-one exec-when-only-one
     :buffer buffer
     ;; If DEF is not provided, fallback to empty string
     ;; to avoid `thing-at-point' to be appended on top of list
     :default (or default "")
     ;; Use `regexp-quote' to fix initial input
     ;; with special characters (e.g nnimap+gmail:)
     :initial-input (and (stringp init) (regexp-quote init)))))

(defun helm-completing-read-with-cands-in-buffer
    (prompt collection test require-match
     init hist default inherit-input-method
     name buffer)
  "Same as `helm-completing-read-default-1' but use candidates-in-buffer."
  ;; Some commands like find-tag may use `read-file-name' from inside
  ;; the calculation of collection. in this case it clash with
  ;; candidates-in-buffer that reuse precedent data (files) which is wrong.
  ;; So (re)calculate collection outside of main helm-session.
  (let ((cands (all-completions "" collection)))
    (helm-completing-read-default-1 prompt cands test require-match
                                    init hist default inherit-input-method
                                    name buffer t)))

(defun* helm-completing-read-default
    (prompt collection &optional
            predicate require-match
            initial-input hist def
            inherit-input-method)
  "An helm replacement of `completing-read'.
This function should be used only as a `completing-read-function'.

Don't use it directly, use instead `helm-comp-read' in your programs.

See documentation of `completing-read' and `all-completions' for details."
  (declare (special helm-mode))
  (let* ((current-command this-command)
         (str-command     (if (consp current-command) ; Maybe a lambda.
                              "Anonymous"
                              (symbol-name current-command)))
         (buf-name        (format "*helm-mode-%s*" str-command))
         (entry           (assq current-command
                                helm-completing-read-handlers-alist))
         (def-com         (cdr-safe entry))
         (str-defcom      (and def-com (symbol-name def-com)))
         (def-args        (list prompt collection predicate require-match
                                initial-input hist def inherit-input-method))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in helm specialized functions.
         (any-args        (append def-args (list str-command buf-name)))
         helm-completion-mode-start-message ; Be quiet
         helm-completion-mode-quit-message
         (minibuffer-completion-table collection)
         (minibuffer-completion-predicate predicate)
         ;; Be sure this pesty *completion* buffer doesn't popup.
         (minibuffer-setup-hook (remove 'minibuffer-completion-help
                                        minibuffer-setup-hook))
         ;; Disable hack that could be used before `completing-read'.
         ;; i.e (push ?\t unread-command-events).
         unread-command-events)
    (when (eq def-com 'ido) (setq def-com 'ido-completing-read))
    (unless (or (not entry) def-com)
      ;; An entry in *read-handlers-alist exists but have
      ;; a nil value, so we exit from here, disable `helm-mode'
      ;; and run the command again with it original behavior.
      ;; `helm-mode' will be restored on exit.
      (return-from helm-completing-read-default
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
                (apply def-com any-args))
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
               (t ; Fall back to classic `helm-comp-read'.
                (helm-completing-read-default-1
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
(defun* helm-c-read-file-name
    (prompt
     &key
     (name "Read File Name")
     (initial-input (expand-file-name default-directory))
     (buffer "*Helm Completions*")
     test
     (case-fold helm-file-name-case-fold-search)
     (preselect nil)
     (history nil)
     must-match
     (marked-candidates nil)
     (alistp t)
     (persistent-action 'helm-find-files-persistent-action)
     (persistent-help "Hit1 Expand Candidate, Hit2 or (C-u) Find file"))
  "Read a file name with helm completion.
It is helm `read-file-name' emulation.

Argument PROMPT is the default prompt to use.

Keys description:

- NAME: Source name, default to \"Read File Name\".

- INITIAL-INPUT: Where to start read file name, default to `default-directory'.

- BUFFER: `helm-buffer' name default to \"*Helm Completions*\".

- TEST: A predicate called with one arg 'candidate'.

- CASE-FOLD: Same as `helm-case-fold-search'.

- PRESELECT: helm preselection.

- HISTORY: Display HISTORY in a special source.

- MUST-MATCH: Can be 'confirm, nil, or t.

- MARKED-CANDIDATES: When non--nil return a list of marked candidates.

- ALISTP: Don't use `all-completions' in history (take effect only on history).

- PERSISTENT-ACTION: a persistent action function.

- PERSISTENT-HELP: persistent help message."
  (when (get-buffer helm-action-buffer)
    (kill-buffer helm-action-buffer))

  ;; Assume completion have been already required,
  ;; so always use 'confirm.
  (when (eq must-match 'confirm-after-completion)
    (setq must-match 'confirm))

  (let ((action-fn '(("Sole action (Identity)"
                      . (lambda (candidate)
                          (if marked-candidates
                              (helm-marked-candidates)
                              (identity candidate)))))))
    (let* ((helm-mp-highlight-delay nil)
           ;; Be sure we don't erase the underlying minibuffer if some.
           (helm-ff-auto-update-initial-value
            (and helm-ff-auto-update-initial-value
                 (not (minibuffer-window-active-p (minibuffer-window)))))
           helm-same-window
           (hist (and history (helm-comp-read-get-candidates
                               history nil nil alistp)))
           (minibuffer-completion-confirm must-match)
           (must-match-map (when must-match
                             (let ((map (make-sparse-keymap)))
                               (define-key map (kbd "RET")
                                 'helm-confirm-and-exit-minibuffer)
                               map)))
           (helm-map (if must-match-map
                         (make-composed-keymap
                          must-match-map helm-c-read-file-map)
                         helm-c-read-file-map))
           (helm-read-file-name-mode-line-string
            (replace-regexp-in-string "helm-exit-minibuffer"
                          "helm-confirm-and-exit-minibuffer"
                          helm-read-file-name-mode-line-string))
           (result (helm
                    :sources
                    `(((name . ,(format "%s History" name))
                       (header-name . (lambda (name)
                                        (concat name
                                                helm-c-find-files-doc-header)))
                       (mode-line . helm-read-file-name-mode-line-string)
                       (candidates . hist)
                       (persistent-action . ,persistent-action)
                       (persistent-help . ,persistent-help)
                       (action . ,action-fn))
                      ((name . ,name)
                       (header-name . (lambda (name)
                                        (concat name
                                                helm-c-find-files-doc-header)))
                       (init . (lambda ()
                                 (setq helm-ff-auto-update-flag
                                       helm-ff-auto-update-initial-value)))
                       (mode-line . helm-read-file-name-mode-line-string)
                       (candidates
                        . (lambda ()
                            ;; Don't run `file-exists-p' if `helm-pattern' is remote.
                            (append (and (not (file-remote-p helm-pattern))
                                         (not (file-exists-p helm-pattern))
                                         (list helm-pattern))
                                    (if test
                                        (loop with hn = (helm-ff-tramp-hostnames)
                                              for i in (helm-find-files-get-candidates
                                                        must-match)
                                              when (or (member i hn) ; A tramp host
                                                       (funcall test i)) ; Test ok
                                              collect i)
                                        (helm-find-files-get-candidates must-match)))))
                       (filtered-candidate-transformer
                        helm-c-find-files-transformer)
                       (persistent-action . ,persistent-action)
                       (candidate-number-limit . 9999)
                       (persistent-help . ,persistent-help)
                       (volatile)
                       (action . ,action-fn)))
                    :input initial-input
                    :prompt prompt
                    :resume 'noresume
                    :case-fold-search case-fold
                    :keymap helm-map
                    :buffer buffer
                    :preselect preselect)))
      (or
       (cond ((and result (stringp result))
              (expand-file-name result))
             ((and result (listp result))
              (mapcar #'expand-file-name result))
             (t result))
       (when (and (not (string= helm-pattern ""))
                  (eq helm-exit-status 0)
                  (eq must-match 'confirm))
         (identity helm-pattern))
       (keyboard-quit)))))

(defun* helm-generic-read-file-name
    (prompt &optional dir default-filename mustmatch initial predicate)
  "An helm replacement of `read-file-name'."
  (declare (special helm-mode))
  (let* ((default (and default-filename
                       (if (listp default-filename)
                           (car default-filename)
                           default-filename)))
         (init (or default initial dir default-directory))
         (ini-input (and init (expand-file-name init)))
         (current-command this-command)
         (str-command (symbol-name current-command))
         (helm-file-completion-sources
          (cons str-command
                (remove str-command helm-file-completion-sources)))
         (buf-name (format "*helm-mode-%s*" str-command))
         (entry (assq current-command
                      helm-completing-read-handlers-alist))
         (def-com  (cdr-safe entry))
         (str-defcom (symbol-name def-com))
         (def-args (list prompt dir default-filename mustmatch initial predicate))
         ;; Append the two extra args needed to set the buffer and source name
         ;; in helm specialized functions.
         (any-args (append def-args (list str-command buf-name)))
         (ido-state ido-mode)
         helm-completion-mode-start-message ; Be quiet
         helm-completion-mode-quit-message  ; Same here
         fname)
    ;; Some functions that normally call `completing-read' can switch
    ;; brutally to `read-file-name' (e.g find-tag), in this case
    ;; the helm specialized function will fail because it is build
    ;; for `completing-read', so set it to 'incompatible to be sure
    ;; we switch to `helm-c-read-file-name' and don't try to call it
    ;; with wrong number of args.
    (when (eq def-com 'ido)
      (setq def-com 'ido-read-file-name) (ido-mode 1))
    (when (and def-com (> (length (help-function-arglist def-com)) 8))
      (setq def-com 'incompatible))
    (unless (or (not entry) def-com)
      (return-from helm-generic-read-file-name
        (unwind-protect
             (progn
               (helm-mode -1)
               (apply read-file-name-function def-args))
          (helm-mode 1))))
    ;; If we use now `read-file-name' we MUST turn off `helm-mode'
    ;; to avoid infinite recursion and CRASH. It will be reenabled on exit.
    (when (or (eq def-com 'read-file-name)
              (eq def-com 'ido-read-file-name)
              (and (stringp str-defcom)
                   (not (string-match "^helm" str-defcom))))
      (helm-mode -1))
    (unwind-protect
         (setq fname
               (cond (;; A specialized function exists, run it
                      ;; with the two extra args specific to helm..
                      (and def-com helm-mode
                           (not (eq def-com 'ido-read-file-name))
                           (not (eq def-com 'incompatible)))
                      (apply def-com any-args))
                     (;; Def-com value is `ido-read-file-name'
                      ;; run it with default args.
                      (and def-com (eq def-com 'ido-read-file-name))
                      (ido-mode 1)
                      (apply def-com def-args))
                     (;; Def-com value is `read-file-name'
                      ;; run it with default args.
                      (eq def-com 'read-file-name)
                      (apply def-com def-args))
                     (t ; Fall back to classic `helm-c-read-file-name'.
                      (helm-c-read-file-name
                       prompt
                       :name str-command
                       :buffer buf-name
                       :initial-input (expand-file-name init dir)
                       :alistp nil
                       :must-match mustmatch
                       :test predicate))))
      (helm-mode 1)
      (ido-mode (if ido-state 1 -1))
      ;; Same comment as in `helm-completing-read-default'.
      (setq this-command current-command))
    fname))

;;;###autoload
(define-minor-mode helm-mode
    "Toggle generic helm completion.

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

Note: This mode will work only partially on Emacs23."
  :group 'helm-mode
  :global t
  :lighter helm-completion-mode-string
  (declare (special completing-read-function))
  (if helm-mode
      (progn
        (setq completing-read-function 'helm-completing-read-default
              read-file-name-function  'helm-generic-read-file-name)
        (message helm-completion-mode-start-message))
      (setq completing-read-function (and (fboundp 'completing-read-default)
                                          'completing-read-default)
            read-file-name-function  (and (fboundp 'read-file-name-default)
                                          'read-file-name-default))
      (message helm-completion-mode-quit-message)))

(provide 'helm-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-mode.el ends here
