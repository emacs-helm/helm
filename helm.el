;;; helm.el --- Emacs incremental and narrowing framework

;; Copyright (C) 2007         Tamas Patrovics
;;               2008 ~ 2011  rubikitch <rubikitch@ruby-lang.org>
;;               2011 ~ 2012  Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This is a fork of anything.el wrote by Tamas Patrovics.

;; Authors of anything.el: Tamas Patrovics
;;                         rubikitch <rubikitch@ruby-lang.org>
;;                         Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; URL: http://github.com/emacs-helm/helm

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

(require 'cl)

;;; Keymap
;;
;;
(defvar helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "<down>")     'helm-next-line)
    (define-key map (kbd "<up>")       'helm-previous-line)
    (define-key map (kbd "C-n")        'helm-next-line)
    (define-key map (kbd "C-p")        'helm-previous-line)
    (define-key map (kbd "<prior>")    'helm-previous-page)
    (define-key map (kbd "<next>")     'helm-next-page)
    (define-key map (kbd "M-v")        'helm-previous-page)
    (define-key map (kbd "C-v")        'helm-next-page)
    (define-key map (kbd "M-<")        'helm-beginning-of-buffer)
    (define-key map (kbd "M->")        'helm-end-of-buffer)
    (define-key map (kbd "C-g")        'helm-keyboard-quit)
    (define-key map (kbd "<right>")    'helm-next-source)
    (define-key map (kbd "<left>")     'helm-previous-source)
    (define-key map (kbd "<RET>")      'helm-exit-minibuffer)
    (define-key map (kbd "C-i")        'helm-select-action)
    (define-key map (kbd "C-z")        'helm-execute-persistent-action)
    (define-key map (kbd "C-e")        'helm-select-2nd-action-or-end-of-line)
    (define-key map (kbd "C-j")        'helm-select-3rd-action)
    (define-key map (kbd "C-o")        'helm-next-source)
    (define-key map (kbd "C-M-v")      'helm-scroll-other-window)
    (define-key map (kbd "M-<next>")   'helm-scroll-other-window)
    (define-key map (kbd "C-M-y")      'helm-scroll-other-window-down)
    (define-key map (kbd "C-M-S-v")    'helm-scroll-other-window-down)
    (define-key map (kbd "M-<prior>")  'helm-scroll-other-window-down)
    (define-key map (kbd "<C-M-down>") 'helm-scroll-other-window)
    (define-key map (kbd "<C-M-up>")   'helm-scroll-other-window-down)
    (define-key map (kbd "C-SPC")      'helm-toggle-visible-mark)
    (define-key map (kbd "M-SPC")      'helm-toggle-visible-mark)
    (define-key map (kbd "M-[")        'helm-prev-visible-mark)
    (define-key map (kbd "M-]")        'helm-next-visible-mark)
    (define-key map (kbd "C-k")        'helm-delete-minibuffer-contents)
    (define-key map (kbd "C-x C-f")    'helm-quit-and-find-file)
    (define-key map (kbd "M-m")        'helm-toggle-all-marks)
    (define-key map (kbd "C-w")        'helm-yank-text-at-point)
    (define-key map (kbd "C-M-a")      'helm-show-all-in-this-source-only)
    (define-key map (kbd "C-r")        'undefined)
    (define-key map (kbd "C-t")        'helm-toggle-resplit-window)
    (define-key map (kbd "C-}")        'helm-narrow-window)
    (define-key map (kbd "C-{")        'helm-enlarge-window)
    (define-key map (kbd "C-c C-d")    'helm-delete-current-selection)
    (define-key map (kbd "C-c C-y")    'helm-yank-selection)
    (define-key map (kbd "C-c C-k")    'helm-kill-selection-and-quit)
    (define-key map (kbd "C-c C-f")    'helm-follow-mode)
    (define-key map (kbd "C-c C-u")    'helm-force-update)
    (define-key map (kbd "M-p")        'previous-history-element)
    (define-key map (kbd "M-n")        'next-history-element)
    ;; Debugging command
    (define-key map "\C-c\C-x\C-d"     'helm-debug-output)
    (define-key map "\C-c\C-x\C-m"     'helm-display-all-visible-marks)
    (define-key map "\C-c\C-x\C-b"     'helm-send-bug-report-from-helm)
    ;; Use `describe-mode' key in `global-map'.
    (define-key map [f1] nil) ; Allow to eval keymap without errors.
    (dolist (k (where-is-internal 'describe-mode global-map))
      (define-key map k 'helm-help))
    map)
  "Keymap for helm.")


(defgroup helm nil
  "Open helm."
  :prefix "helm-" :group 'convenience)

;;; Faces
;;
;;
(defface helm-source-header
    '((t (:background "#22083397778B"
          :foreground "white"
          :underline t)))
  "Face for source header in the helm buffer."
  :group 'helm)

(defface helm-visible-mark
    '((((min-colors 88) (background dark))
       (:background "green1" :foreground "black"))
      (((background dark))
       (:background "green" :foreground "black"))
      (((min-colors 88))
       (:background "green1"))
      (t (:background "green")))
  "Face for visible mark."
  :group 'helm)

(defface helm-header
    '((t (:inherit header-line)))
  "Face for header lines in the helm buffer."
  :group 'helm)

(defface helm-candidate-number
    '((t (:background "Yellow" :foreground "black")))
  "Face for candidate number in mode-line." :group 'helm)

(defface helm-selection
    '((t (:background "ForestGreen" :underline t)))
  "Face for currently selected item in the helm buffer."
  :group 'helm)

;;; Variables
;;
;;
(defvar helm-type-attributes nil
  "It's a list of \(TYPE ATTRIBUTES ...\).
ATTRIBUTES are the same as attributes for `helm-sources'.
TYPE connects the value to the appropriate sources.
Don't set this directly, use instead `define-helm-type-attribute'.

This allows specifying common attributes for several sources.
For example, sources which provide files can specify
common attributes with a `file' type.")

(defvar helm-display-source-at-screen-top t
  "*Display candidates at the top of screen.
This happen when using `helm-next-source' and `helm-previous-source'.")

(defvar helm-candidate-number-limit 50
  "*Limit candidate number globally.
Do not show more candidates than this limit from individual sources.
It is usually pointless to show hundreds of matches
when the pattern is empty, because it is much simpler to type a
few characters to narrow down the list of potential candidates.

Set it to nil if you don't want this limit.")

(defvar helm-idle-delay 0.3
  "*Be idle for this many seconds, before updating in delayed sources.
This is useful for sources involving heavy operations
\(like launching external programs\), so that candidates
from the source are not retrieved unnecessarily if the user keeps typing.

It also can be used to declutter the results helm displays,
so that results from certain sources are not shown with every
character typed, only if the user hesitates a bit.")

(defvar helm-input-idle-delay 0.3
  "Be idle for this many seconds, before updating.

Unlike `helm-idle-delay', it is also effective for non-delayed sources.
If nil, candidates are collected immediately.

Note:  If this value is too low compared to `helm-idle-delay',
you may have duplicated sources when using multiples sources.
Safe value is always >= `helm-idle-delay'.
Default settings are equal value for both.")

(defvar helm-samewindow nil
  "Use current window to show the candidates.
If t then Helm doesn't pop up a new window.")

(defvar helm-source-filter nil
  "A list of source names to be displayed.
Other sources won't appear in the search results.
If nil then there is no filtering.
See also `helm-set-source-filter'.")

(defvar helm-action-buffer "*helm action*"
  "Buffer showing actions.")

(defvar helm-selection-overlay nil
  "Overlay used to highlight the currently selected item.")

(defvar helm-async-processes nil
  "List of information about asynchronous processes managed by helm.")

(defvar helm-before-initialize-hook nil
  "Run before helm initialization.
This hook is run before init functions in `helm-sources'.")

(defvar helm-after-initialize-hook nil
  "Run after helm initialization.
Global variables are initialized and the helm buffer is created.
But the helm buffer has no contents.")

(defvar helm-update-hook nil
  "Run after the helm buffer was updated according the new input pattern.
This hook is run at the beginning of buffer.
The first candidate is selected after running this hook.
See also `helm-after-update-hook'.")

(defvar helm-after-update-hook nil
  "Run after the helm buffer was updated according the new input pattern.
This is very similar to `helm-update-hook' but selection is not moved.
It is useful to select a particular object instead of the first one.")

(defvar helm-cleanup-hook nil
  "Run after helm minibuffer is closed.
IOW this hook is executed BEFORE performing action.")

(defvar helm-select-action-hook nil
  "Run when opening the action buffer.")

(defvar helm-before-action-hook nil
  "Run before executing action.
Contrarily to `helm-cleanup-hook',
this hook run before helm minibuffer is closed
and before performing action.")

(defvar helm-after-action-hook nil
  "Run after executing action.")

(defvar helm-after-persistent-action-hook nil
  "Run after executing persistent action.")

(defvar helm-move-selection-before-hook nil
  "Run before moving selection in `helm-buffer'.")

(defvar helm-move-selection-after-hook nil
  "Run after moving selection in `helm-buffer'.")

(defvar helm-restored-variables
  '(helm-candidate-number-limit
    helm-source-filter
    helm-source-in-each-line-flag
    helm-map
    helm-sources)
  "Variables which are restored after `helm' invocation.")

(defvar helm-saved-selection nil
  "Value of the currently selected object when the action list is shown.")

(defvar helm-current-prefix-arg nil
  "Record `current-prefix-arg' when exiting minibuffer.")

(defvar helm-candidate-separator
  "--------------------"
  "Candidates separator of `multiline' source.")

(defvar helm-saved-action nil
  "Saved value of the currently selected action by key.")

(defvar helm-last-sources nil
  "OBSOLETE!! Sources of previously invoked `helm'.")

(defvar helm-saved-current-source nil
  "Value of the current source when the action list is shown.")

(defvar helm-compiled-sources nil
  "Compiled version of `helm-sources'.")

(defvar helm-in-persistent-action nil
  "Flag whether in persistent-action or not.")

(defvar helm-quick-update nil
  "If non-nil, suppress displaying sources which are out of screen at first.
They are treated as delayed sources at this input.
This flag makes `helm' a bit faster with many sources.")

(defvar helm-last-sources-local nil
  "Buffer local value of `helm-sources'.")

(defvar helm-last-buffer nil
  "`helm-buffer' of previously `helm' session.")

(defvar helm-save-configuration-functions
  '(set-window-configuration . current-window-configuration)
  "The functions used to restore/save window or frame configurations.
It is a pair where the car is the function to restore window or frame config,
and the cdr is the function to save the window or frame config.

If you want to save and restore frame configuration, set this variable to
 '\(set-frame-configuration . current-frame-configuration\)

Older version saves/restores frame configuration, but the default is changed now
because flickering can occur in some environment. ")

(defvar helm-persistent-action-use-special-display nil
  "If non-nil, use `special-display-function' in persistent action.")

(defvar helm-execute-action-at-once-if-one nil
  "Execute default action and exit when only one candidate is remaining.
It is useful for `helm' applications.")

(defvar helm-quit-if-no-candidate nil
  "Quit when there is no candidates when non--nil.
This variable accepts a function, which is executed if no candidate.

It is useful for `helm' applications.")

(defvar helm-scroll-amount nil
  "Scroll amount when scrolling other window in an helm session.
It is used by `helm-scroll-other-window'
and `helm-scroll-other-window-down'.

If you prefer scrolling line by line, set this value to 1.")

(defvar helm-display-function 'helm-default-display-buffer
  "Function to display *helm* buffer.
It is `helm-default-display-buffer' by default,
which affects `helm-samewindow'.")

(defvar helm-source-in-each-line-flag nil
  "Non-nil means add helm-source text-property in each candidate.
experimental feature.")

(defvaralias 'helm-debug-variables 'helm-debug-forms)

(defvar helm-debug-forms nil
  "Forms to show in `helm-debug-output'.
Otherwise all variables started with `helm-' are shown.
It is useful for debug.")

(defvar helm-debug nil
  "If non-nil, write log message into *Helm Log* buffer.
If `debug-on-error' is non-nil, write log message regardless of this variable.
It is disabled by default because *Helm Log* grows quickly.")


;;; Internal Variables
;;
;;
(defvar helm-sources nil)
(defvar helm-delayed-init-executed nil)
(defvar helm-buffer "*helm*"
  "Buffer showing completions.")
(defvar helm-current-buffer nil
  "Current buffer when `helm' is invoked.")
(defvar helm-buffer-file-name nil
  "Variable `buffer-file-name' when `helm' is invoked.")
(defvar helm-candidate-cache nil
  "Holds the available candidate withing a single helm invocation.")
(defvar helm-pattern ""
  "The input pattern used to update the helm buffer.")
(defvar helm-input ""
  "The input typed in the candidates panel.")
(defvar helm-source-name nil)
(defvar helm-candidate-buffer-alist nil)
(defvar helm-check-minibuffer-input-timer nil)
(defvar helm-match-hash (make-hash-table :test 'equal))
(defvar helm-cib-hash (make-hash-table :test 'equal))
(defvar helm-tick-hash (make-hash-table :test 'equal))
(defvar helm-issued-errors nil)
(defvar helm-once-called-functions nil)
(defvar helm-follow-mode nil)
(defvar helm-let-variables nil)
(defvar helm-split-window-state nil)
(defvar helm-selection-point nil)
(defvar helm-alive-p nil)
(defvar helm-visible-mark-overlays nil)
(defconst helm-default-match-functions
  (list (lambda (candidate)
          (string-match helm-pattern candidate)))
  "Default functions to match candidates according to `helm-pattern'.")


;; Utility: logging
(defun helm-log (format-string &rest args)
  "Log message if `debug-on-error' or `helm-debug' is non-nil.
Messages are written to the *Helm Log* buffer.

Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'."
  (when (or debug-on-error helm-debug)
    (with-current-buffer (get-buffer-create "*Helm Log*")
      (buffer-disable-undo)
      (set (make-local-variable 'inhibit-read-only) t)
      (goto-char (point-max))
      (insert (let ((tm (current-time)))
                (format "%s.%06d (%s) %s\n"
                        (format-time-string "%H:%M:%S" tm)
                        (nth 2 tm)
                        (helm-log-get-current-function)
                        (apply #'format (cons format-string args))))))))

(defmacro helm-log-eval (&rest exprs)
  "Write each EXPRS evaluation result to the *Helm Log* buffer."
  `(helm-log-eval-internal ',exprs))

(defun helm-log-run-hook (hook)
  "Run HOOK like `run-hooks' but write these actions to helm log buffer."
  (helm-log "executing %s" hook)
  (when (boundp hook)
    (helm-log-eval (symbol-value hook))
    (helm-log-eval (default-value hook)))
  (run-hooks hook)
  (helm-log "executed %s" hook))

(defun helm-log-eval-internal (exprs)
  "Eval EXPRS and write results to helm log buffer."
  (dolist (expr exprs)
    (condition-case err
        (helm-log "%S = %S" expr (eval expr))
      (error (helm-log "%S = ERROR!" expr)))))

(defun helm-log-get-current-function ()
  "Get function name calling `helm-log'.
The original idea is from `tramp-debug-message'."
  (loop with exclude-func-re = "^helm-\\(?:interpret\\|log\\|.*funcall\\)"
        for btn from 1 to 40            ;avoid inf-loop
        for btf = (second (backtrace-frame btn))
        for fn  = (if (symbolp btf) (symbol-name btf) "")
        if (and (string-match "^helm" fn)
                (not (string-match exclude-func-re fn)))
        return fn))

(defun helm-log-error (&rest args)
  "Accumulate error messages into `helm-issued-errors'.
ARGS are args given to `format'."
  (apply 'helm-log (concat "ERROR: " (car args)) (cdr args))
  (let ((msg (apply 'format args)))
    (unless (member msg helm-issued-errors)
      (add-to-list 'helm-issued-errors msg))))

(defvar helm-debug-root-directory nil
  "When non--nil, save helm log to `helm-last-log-file'.
Be aware that if you set that, you will end up with a huge directory
of log files, so use that only for debugging purpose.
See `helm-log-save-maybe' for more info.")
;; Internal
(defvar helm-last-log-file nil
  "The name of the last helm session log file.")

(defun helm-log-save-maybe ()
  "May be save log buffer to `helm-last-log-file'.
If `helm-debug-root-directory' is non--nil and a valid directory,
a directory named 'helm-debug-<date of today>'
will be created there and the log recorded in a file named
at the date and time of today in this directory."
  (when (and (stringp helm-debug-root-directory)
             (file-directory-p helm-debug-root-directory)
             (or debug-on-error helm-debug))
    (let ((logdir (expand-file-name (concat "helm-debug-"
                                            (format-time-string "%Y%m%d"))
                                    helm-debug-root-directory)))
      (make-directory logdir t)
      (with-current-buffer (get-buffer-create "*Helm Log*")
        (write-region (point-min) (point-max)
                      (setq helm-last-log-file
                            (expand-file-name
                             (format-time-string "%Y%m%d-%H%M%S")
                             logdir))
                      nil 'silent)
        (kill-buffer)))))

;;;###autoload
(defun helm-open-last-log ()
  "Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to \"*Helm Log*\" buffer."
  (interactive)
  (if helm-last-log-file
      (view-file helm-last-log-file)
      (switch-to-buffer "*Helm Log*")))

(defun helm-print-error-messages ()
  "Print error messages in `helm-issued-errors'."
  (message "%s" (mapconcat 'identity (reverse helm-issued-errors) "\n")))



;; Programming Tools
(defmacro helm-aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defun helm-mklist (obj)
  "If OBJ is a list \(but not lambda\), return itself.
Otherwise make a list with one element."
  (if (and (listp obj) (not (functionp obj)))
      obj
      (list obj)))


;; Helm API

(defun helm-buffer-get ()
  "Return `helm-action-buffer' if shown otherwise `helm-buffer'."
  (if (helm-action-window)
      helm-action-buffer
      helm-buffer))

(defun helm-window ()
  "Window of `helm-buffer'."
  (get-buffer-window (helm-buffer-get) 'visible))

(defun helm-action-window ()
  "Window of `helm-action-buffer'."
  (get-buffer-window helm-action-buffer 'visible))

(defmacro with-helm-window (&rest body)
  "Be sure BODY is excuted in the helm window."
  (declare (indent 0) (debug t))
  `(with-selected-window (helm-window)
    ,@body))

(defmacro with-helm-current-buffer (&rest body)
  "Eval BODY inside `helm-current-buffer'."
  (declare (indent 0) (debug t))
  `(with-current-buffer helm-current-buffer
     ,@body))

(defmacro with-helm-restore-variables(&rest body)
  "Restore `helm-restored-variables' after executing BODY.
`post-command-hook' is handled specially."
  (declare (indent 0) (debug t))
  `(let ((--orig-vars (mapcar (lambda (v)
                                (cons v (symbol-value v)))
                              helm-restored-variables))
         (--post-command-hook-pair (cons post-command-hook
                                         (default-value 'post-command-hook))))
     (setq post-command-hook '(t))
     (setq-default post-command-hook nil)
     (unwind-protect (progn ,@body)
       (loop for (var . value) in --orig-vars
             do (set var value))
       (setq post-command-hook (car --post-command-hook-pair))
       (setq-default post-command-hook (cdr --post-command-hook-pair))
       (helm-log "restore variables"))))

(defun* helm-attr (attribute-name
                   &optional (src (helm-get-current-source)) compute)
  "Get the value of ATTRIBUTE-NAME of SRC.
If SRC is omitted, use current source.
If COMPUTE is non--nil compute value of ATTRIBUTE-NAME
with `helm-interpret-value'."
  (helm-aif (assq attribute-name src)
      (if compute (helm-interpret-value (cdr it)) (cdr it))))

(defun* helm-attr-defined (attribute-name
                           &optional (src (helm-get-current-source)))
  "Return non-nil if ATTRIBUTE-NAME of SRC is defined.
if SRC is omitted, use current source."
  (and (assq attribute-name src) t))

(defun* helm-attrset (attribute-name value
                                     &optional
                                     (src (helm-get-current-source)))
  "Set the value of ATTRIBUTE-NAME of source SRC to VALUE.
If ATTRIBUTE-NAME doesn't exists in source it is created with value VALUE.
If SRC is omitted, use current source."
  (helm-aif (assq attribute-name src)
      (setcdr it value)
    (setcdr src (cons (cons attribute-name value) (cdr src))))
  value)

(defun helm-set-source-filter (sources)
  "Set the value of `helm-source-filter' to SOURCES and update.

This function sets a filter for helm sources and it may be
called while helm is running. It can be used to toggle
displaying of sources dinamically. For example, additional keys
can be bound into `helm-map' to display only the file-related
results if there are too many matches from other sources and
you're after files only:

Shift+F shows only file results from some sources:

\(define-key helm-map \"F\" 'helm-my-show-files-only)

\(defun helm-my-show-files-only ()
  (interactive)
  (helm-set-source-filter '(\"File Name History\"
                                  \"Files from Current Directory\")))

Shift+A shows all results:

\(define-key helm-map \"A\" 'helm-my-show-all)

\(defun helm-my-show-all ()
  (interactive)
  (helm-set-source-filter nil))

The -my- part is added to avoid collisions with
existing Helm function names."
  (unless (and (listp sources)
               (loop for name in sources always (stringp name)))
    (error "Invalid data in `helm-set-source-filter': %S" sources))
  (let ((cur-disp-sel (with-current-buffer helm-buffer
                        (helm-get-selection nil t))))
    (setq helm-source-filter sources)
    (helm-log-eval helm-source-filter)
    (helm-update cur-disp-sel)))

(defun helm-set-sources (sources &optional no-init no-update)
  "Set SOURCES during `helm' invocation.
If NO-INIT is non-nil, skip executing init functions of SOURCES.
If NO-UPDATE is non-nil, skip executing `helm-update'."
  (with-current-buffer helm-buffer
    (setq helm-compiled-sources nil
          helm-sources sources
          helm-last-sources-local sources)
    (helm-log-eval helm-compiled-sources helm-sources))
  (unless no-init (helm-funcall-foreach 'init))
  (unless no-update (helm-update)))

(defvar helm-compile-source-functions
  '(helm-compile-source--type
    helm-compile-source--dummy
    helm-compile-source--candidates-in-buffer)
  "Functions to compile elements of `helm-sources' (plug-in).")

(defun helm-get-sources ()
  "Return compiled `helm-sources', which is memoized.

Attributes:

- type
  `helm-type-attributes' are merged in.
- candidates-buffer
  candidates, volatile and match attribute are created."
  (cond
    ;; action
    ((helm-action-window)
     helm-sources)
    ;; memoized
    (helm-compiled-sources)
    ;; first time
    (t
     (prog1
         (setq helm-compiled-sources
               (helm-compile-sources
                helm-sources helm-compile-source-functions))
       (helm-log-eval helm-compiled-sources)))))

(defun* helm-get-selection (&optional (buffer nil buffer-s)
                                      force-display-part)
  "Return the currently selected item or nil.
if BUFFER is nil or unspecified, use helm-buffer as default value.
If FORCE-DISPLAY-PART is non-nil, return the display string.
If FORCE-DISPLAY-PART value is 'withprop the display string is returned
with its properties."
  (setq buffer (if (and buffer buffer-s) buffer helm-buffer))
  (unless (helm-empty-buffer-p buffer)
    (with-current-buffer buffer
      (let* ((disp-fn (if (eq force-display-part 'withprop)
                          'buffer-substring
                          'buffer-substring-no-properties))
             (selection
              (or (and (not force-display-part)
                       (get-text-property (overlay-start
                                           helm-selection-overlay)
                                          'helm-realvalue))
                  ;; It is needed to return properties of DISP in some case,
                  ;; e.g for `helm-confirm-and-exit-minibuffer',
                  ;; so use `buffer-substring' here when 'withprop is specified.
                  (let ((disp (funcall
                               disp-fn
                               (overlay-start helm-selection-overlay)
                               (1- (overlay-end helm-selection-overlay))))
                        (source (helm-get-current-source)))
                    (helm-aif (and (not force-display-part)
                                   (assoc-default 'display-to-real source))
                        (helm-funcall-with-source source it disp)
                      disp)))))
        (unless (equal selection "")
          (helm-log-eval selection)
          selection)))))

(defun helm-get-action ()
  "Return the associated action for the selected candidate.
It is a function symbol \(sole action\) or list
of \(action-display . function\)."
  (unless (helm-empty-buffer-p (helm-buffer-get))
    (helm-aif (helm-attr 'action-transformer)
        (helm-composed-funcall-with-source
         (helm-get-current-source) it
         (helm-attr 'action) (helm-get-selection))
      (helm-attr 'action))))

(defun helm-get-current-source ()
  "Return the source for the current selection.
Use it in init, candidates, action, candidate-transformer,
filtered-candidate-transformer functions."
  (declare (special source))
  ;; The name `helm-get-current-source' should be used in init function etc.
  (if (and (boundp 'helm-source-name) (stringp helm-source-name))
      source
      (with-current-buffer (helm-buffer-get)
        (or
         ;; This happen only when `helm-source-in-each-line-flag'
         ;; is non--nil and there is candidates in buffer.
         (get-text-property (point) 'helm-source)
         ;; Return nil when no--candidates.
         (block exit
           ;; This goto-char shouldn't be necessary, but point is moved to
           ;; point-min somewhere else which shouldn't happen.
           (goto-char (overlay-start helm-selection-overlay))
           (let* ((header-pos (or (helm-get-previous-header-pos)
                                  (helm-get-next-header-pos)))
                  (source-name
                   (save-excursion
                     (unless header-pos
                       (return-from exit nil))
                     (goto-char header-pos)
                     (helm-current-line-contents))))
             (loop for source in (helm-get-sources) thereis
                   (and (equal (assoc-default 'name source) source-name)
                        source))))))))

(defun helm-buffer-is-modified (buffer)
  "Return non-nil when BUFFER is modified since `helm' was invoked."
  (let* ((b (get-buffer buffer))
         (key (concat (buffer-name b) "/" (helm-attr 'name)))
         (source-tick (or (gethash key helm-tick-hash) 0))
         (buffer-tick (buffer-chars-modified-tick b))
         (modifiedp (/= source-tick buffer-tick)))
    (puthash key buffer-tick helm-tick-hash)
    (helm-log-eval buffer modifiedp)
    modifiedp))

(defun helm-current-buffer-is-modified ()
  "Check if `helm-current-buffer' is modified since `helm' was invoked."
  (helm-buffer-is-modified helm-current-buffer))

(defvar helm-quit nil)
(defun helm-run-after-quit (function &rest args)
  "Perform an action after quitting `helm'.
The action is to call FUNCTION with arguments ARGS."
  (setq helm-quit t)
  (helm-kill-async-processes)
  (helm-log-eval function args)
  (apply 'run-with-idle-timer 0 nil function args)
  (helm-exit-minibuffer))


(defun define-helm-type-attribute (type definition &optional doc)
  "Register type attribute of TYPE as DEFINITION with DOC.
DOC is displayed in `helm-type-attributes' docstring.

Use this function is better than setting `helm-type-attributes' directly."
  (loop for i in definition do
        ;; without `ignore-errors', error at emacs22
        (ignore-errors (setf i (delete nil i))))
  (helm-add-type-attribute type definition)
  (and doc (helm-document-type-attribute type doc))
  nil)

(defvaralias 'helm-attributes 'helm-additional-attributes)
(defvar helm-additional-attributes nil
  "List of all `helm' attributes.")

(defun helm-document-attribute (attribute short-doc &optional long-doc)
  "Register ATTRIBUTE documentation introduced by plug-in.
SHORT-DOC is displayed beside attribute name.
LONG-DOC is displayed below attribute name and short documentation."
  (if long-doc
      (setq short-doc (concat "(" short-doc ")"))
      (setq long-doc short-doc
            short-doc ""))
  (add-to-list 'helm-additional-attributes attribute t)
  (put attribute 'helm-attrdoc
       (concat "- " (symbol-name attribute)
               " " short-doc "\n\n" long-doc "\n")))

(put 'helm-document-attribute 'lisp-indent-function 2)

(defun helm-interpret-value (value &optional source)
  "Interpret VALUE as variable, function or literal and return it.
If VALUE is a function, call it with no arguments and return the value.
If SOURCE compute VALUE for this source.
If VALUE is a variable, return the value.
If VALUE is a symbol, but it is not a function or a variable, cause an error.
Otherwise, return VALUE itself."
  (cond ((and source (functionp value))
         (helm-funcall-with-source source value))
        ((functionp value)
         (funcall value))
        ((and (symbolp value) (boundp value))
         (symbol-value value))
        ((symbolp value)
         (error
          "helm-interpret-value: Symbol must be a function or a variable"))
        (t
         value)))

(defun helm-once (function &rest args)
  "Ensure FUNCTION with ARGS to be called once in `helm' session."
  (let ((spec (cons function args)))
    (unless (member spec helm-once-called-functions)
      (apply function args)
      (push spec helm-once-called-functions))))


;; Core: API helper
(defun* helm-empty-buffer-p (&optional (buffer helm-buffer))
  "Check if BUFFER have candidates.
Default value for BUFFER is `helm-buffer'."
  (zerop (buffer-size (and buffer (get-buffer buffer)))))

(defun helm-let-eval-varlist (varlist)
  "Return the list of pairs VARLIST with each cdr of pair evaluated.
If VARLIST contain single elements, those are returned
as a list of one element."
  (mapcar (lambda (pair)
            (if (listp pair)
                (cons (car pair) (eval (cadr pair)))
                (cons pair nil)))
          varlist))

(defun helm-let-internal (binding bodyfunc)
  "Set BINDING to helm buffer-local variables and Evaluate BODYFUNC.
BINDING is a list of \(VARNAME . VALUE\) pair."
  (setq helm-let-variables binding)
  (unwind-protect
       (funcall bodyfunc)
    (setq helm-let-variables nil)))


;; Core: tools
(defun helm-current-line-contents ()
  "Current line string without properties."
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun helm-funcall-with-source (source func &rest args)
  "Call from SOURCE FUNC list or single function FUNC with ARGS.
FUNC can be a symbol or a list of functions.
Return the result of last function call."
  (let ((helm-source-name (assoc-default 'name source))
        result)
    (helm-log-eval helm-source-name func args)
    (dolist (func (if (functionp func) (list func) func) result)
      (setq result (apply func args)))))

(defun helm-funcall-foreach (sym)
  "Call the function SYM for each source if any."
  (dolist (source (helm-get-sources))
    (helm-aif (assoc-default sym source)
        (helm-funcall-with-source source it))))

(defun helm-normalize-sources (sources)
  "If SOURCES is only one source, make a list of one element."
  (cond ((or (and sources
                  (symbolp sources))
             (and (listp sources) (assq 'name sources)))
         (list sources))
        (sources)
        (t helm-sources)))

(defun helm-approximate-candidate-number (&optional in-current-source)
  "Return approximate number of candidates in `helm-buffer'.
If IN-CURRENT-SOURCE is provided return number of candidates
in the source where point is.
It is used to check if candidate number is 0, 1, or 2+."
  (with-current-buffer helm-buffer
    (save-excursion
      (if in-current-source
          (goto-char (helm-get-previous-header-pos))
          (goto-char (point-min)))
      (forward-line 1)
      (let ((count-multi 1))
        (if (helm-pos-multiline-p)
            (save-excursion
              (loop while (and (not (if in-current-source
                                        (save-excursion
                                          (forward-line 2)
                                          (or (helm-pos-header-line-p) (eobp)))
                                        (eobp)))
                               (search-forward helm-candidate-separator nil t))
                    do (incf count-multi)
                    finally return count-multi))
            (save-excursion
              (loop with ln = 0
                    while (not (if in-current-source
                                   (or (helm-pos-header-line-p) (eobp))
                                   (eobp)))
                    unless (helm-pos-header-line-p)
                    do (incf ln)
                    do (forward-line 1) finally return ln)))))))

(defmacro with-helm-quittable (&rest body)
  "If an error occur in execution of BODY, quit helm safely."
  (declare (indent 0) (debug t))
  `(let (inhibit-quit)
     (condition-case v
         (progn ,@body)
       (quit (setq helm-quit t)
             (exit-minibuffer)
             (keyboard-quit)))))

(defun helm-compose (arg-lst func-lst)
  "Apply arguments specified in ARG-LST with each function of FUNC-LST.
The result of each function will be the new `car' of ARG-LST.

This function allows easy sequencing of transformer functions."
  (dolist (func func-lst)
    (setcar arg-lst (apply func arg-lst)))
  (car arg-lst))

(defun helm-composed-funcall-with-source (source funcs &rest args)
  "With SOURCE apply `helm-funcall-with-source' with each FUNCS and ARGS.
This is used in transformers to modify candidates list."
  (if (functionp funcs)
      (apply 'helm-funcall-with-source source funcs args)
      (apply 'helm-funcall-with-source source
             (lambda (&rest args)
               (helm-compose args funcs))
             args)))

(defun helm-new-timer (variable timer)
  "Give VARIABLE value to TIMER and cancel old timer."
  (helm-aif (symbol-value variable)
      (cancel-timer it))
  (set variable timer))


;; Core: entry point
(defconst helm-argument-keys
  '(:sources :input :prompt :resume
    :preselect :buffer :keymap :default :history))

(defun helm (&rest plist)
  "Main function to execute helm sources.

Keywords supported:
:sources :input :prompt :resume :preselect :buffer :keymap :default :history
Extra keywords are supported and can be added, see below.

When call interactively with no arguments deprecated `helm-sources'
will be used if non--nil.

PLIST is a list like \(:key1 val1 :key2 val2 ...\) or
\(&optional sources input prompt resume
            preselect buffer keymap default history\).

Basic keywords are the following:

\:sources

Temporary value of `helm-sources'.  It also accepts a
symbol, interpreted as a variable of an helm source.  It
also accepts an alist representing an helm source, which is
detected by \(assq 'name ANY-SOURCES\)

\:input

Temporary value of `helm-pattern', ie. initial input of minibuffer.

\:prompt

Prompt other than \"pattern: \".

\:resume

If t, Resurrect previously instance of `helm'.  Skip the initialization.
If 'noresume, this instance of `helm' cannot be resumed.

\:preselect

Initially selected candidate.  Specified by exact candidate or a regexp.

\:buffer

`helm-buffer' instead of *helm*.

\:keymap

`helm-map' for current `helm' session.

\:default

A default argument that will be inserted in minibuffer \
with \\<minibuffer-local-map>\\[next-history-element].
When nil of not present `thing-at-point' will be used instead.

\:history

By default all minibuffer input is pushed to `minibuffer-history',
if an argument HISTORY is provided, input will be pushed to HISTORY.
History element should be a symbol.

Of course, conventional arguments are supported, the two are same.

\(helm :sources sources :input input :prompt prompt :resume resume
           :preselect preselect :buffer buffer :keymap keymap :default default
           :history history\)
\(helm sources input prompt resume preselect buffer keymap default history\)

Other keywords are interpreted as local variables of this helm session.
The `helm-' prefix can be omitted.  For example,

\(helm :sources 'helm-c-source-buffers
           :buffer \"*buffers*\" :candidate-number-limit 10\)

means starting helm session with `helm-c-source-buffers'
source in *buffers* buffer and set variable `helm-candidate-number-limit'
to 10 as session local variable."
  (if (keywordp (car plist))
      (helm-let-internal
       (helm-parse-keys plist)
       (lambda ()
         (apply 'helm
                (mapcar (lambda (key) (plist-get plist key))
                        helm-argument-keys))))
      (apply 'helm-internal plist)))

(defun helm-parse-keys (keys)
  "Parse the KEYS arguments of `helm'.
Return only the keys that are not in `helm-argument-keys'.
It is used to set local variables via `helm-let-internal'.
This allow to add arguments that are not part of `helm-argument-keys',
but are valid helm attributes.
i.e :candidate-number-limit will be bound to `helm-candidate-number-limit'
in source."
  ;; (helm-parse-keys '(:sources ((name . "test")
  ;;                                  (candidates . (a b c)))
  ;;                        :buffer "toto"
  ;;                        :candidate-number-limit 4))
  ;; ==> ((helm-candidate-number-limit . 4))
  (loop for (key value) on keys by #'cddr
        for symname = (substring (symbol-name key) 1)
        for sym = (intern (if (string-match "^helm-" symname)
                              symname
                              (concat "helm-" symname)))
        unless (memq key helm-argument-keys)
        collect (cons sym value)))

;;; Core: entry point helper
(defun helm-internal (&optional
                        any-sources any-input
                        any-prompt any-resume
                        any-preselect any-buffer
                        any-keymap any-default any-history)
  "The internal helm function called by `helm'.
For ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT ANY-BUFFER and
ANY-KEYMAP ANY-DEFAULT ANY-HISTORY See `helm'."
  (helm-log (concat "[Start session] " (make-string 41 ?+)))
  (helm-log-eval any-prompt any-preselect
                 any-buffer any-keymap any-default)
  (let ((old-overridding-local-map overriding-local-map))
    (unwind-protect
         (condition-case v
             (let ( ;; It is needed because `helm-source-name' is non-nil
                   ;; when `helm' is invoked by action. Awful global scope.
                   helm-source-name
                   helm-in-persistent-action
                   helm-quit
                   (case-fold-search t)
                   (helm-buffer (or any-buffer helm-buffer))
                   ;; cua-mode ; avoid error when region is selected
                   )
               (with-helm-restore-variables
                 (helm-initialize any-resume any-input any-sources)
                 (helm-display-buffer helm-buffer)
                 (helm-log "show prompt")
                 (unwind-protect
                      (helm-read-pattern-maybe
                       any-prompt any-input any-preselect
                       any-resume any-keymap any-default
                       (when (and any-history (symbolp any-history))
                         any-history))
                   (helm-cleanup)))
               (prog1 (unless helm-quit
                        (helm-execute-selection-action-1))
                 (helm-log (concat "[End session] " (make-string 41 ?-)))))
           (quit
            (helm-restore-position-on-quit)
            (helm-log (concat "[End session (quit)] " (make-string 34 ?-)))
            nil))
      (setq overriding-local-map old-overridding-local-map)
      (helm-log-save-maybe))))


;;; Helm resume
;;
;;
;;;###autoload
(defun* helm-resume (&optional
                     (any-buffer helm-last-buffer)
                     buffer-pattern (any-resume t))
  "Resurrect previously invoked `helm'.
Called with a prefix arg, allow choosing among all existing
helm buffers.  i.e choose among various helm sessions."
  (interactive)
  (when (or current-prefix-arg buffer-pattern)
    (setq any-buffer (helm-resume-select-buffer buffer-pattern)))
  (setq helm-compiled-sources nil)
  (helm
   :sources (or (buffer-local-value
                 'helm-last-sources-local (get-buffer any-buffer))
                helm-last-sources helm-sources)
   :input (buffer-local-value 'helm-input-local (get-buffer any-buffer))
   :resume any-resume
   :buffer any-buffer))

;;;###autoload
(defun* helm-resume-window-only (&optional
                                 (any-buffer helm-last-buffer)
                                 buffer-pattern)
  (interactive)
  (helm-resume any-buffer buffer-pattern 'window-only))

(defun helm-resume-p (any-resume)
  "Whether current helm session is resumed or not.
Just check if ANY-RESUME value is t or window-only."
  (memq any-resume '(t window-only)))

(defun helm-resume-select-buffer (input)
  "Resume precedent helm session with initial input INPUT."
  (or (helm :sources '(((name . "Resume helm buffer")
                        (candidates . helm-buffers)
                        (action . identity)))
            :input  input
            :resume 'noresume
            :buffer "*helm resume*")
      (keyboard-quit)))


;;;###autoload
(defun helm-at-point (&optional
                        any-sources any-input
                        any-prompt any-resume
                        any-preselect any-buffer)
  "Call helm with symbol at point as initial input.
ANY-SOURCES ANY-INPUT ANY-PROMPT ANY-RESUME ANY-PRESELECT and ANY-BUFFER
are same args as in `helm'."
  (interactive)
  (helm :sources any-sources
        :input (if current-prefix-arg
                   (concat "\\b" (thing-at-point 'symbol) "\\b"
                           (if (featurep 'helm-match-plugin) " " ""))
                   any-input)
        :prompt any-prompt
        :resume any-resume
        :preselect any-preselect
        :buffer any-buffer))

;;;###autoload
(defun helm-other-buffer (any-sources any-buffer)
  "Simplified interface of `helm' with other `helm-buffer'.
Call `helm' with only ANY-SOURCES and ANY-BUFFER as args."
  (helm :sources any-sources :buffer any-buffer))

(defun helm-nest (&rest same-as-helm)
  "Allow calling `helm' whithin a running helm session."
  (with-helm-window
    (let (helm-current-position
          helm-current-buffer
          (orig-helm-current-buffer helm-current-buffer)
          (orig-helm-buffer helm-buffer)
          (orig-helm-last-frame-or-window-configuration
           helm-last-frame-or-window-configuration)
          helm-pattern
          (helm-buffer (or (getf same-as-helm :buffer)
                           (nth 5 same-as-helm)
                           "*Helm*"))
          helm-sources
          helm-compiled-sources
          (helm-samewindow t)
          (enable-recursive-minibuffers t))
      (unwind-protect
           (apply #'helm same-as-helm)
        (with-current-buffer orig-helm-buffer
          (helm-initialize-overlays orig-helm-buffer)
          (setq helm-buffer (current-buffer))
          (helm-mark-current-line)
          (setq helm-last-frame-or-window-configuration
                orig-helm-last-frame-or-window-configuration)
          (setq cursor-type t)
          (setq helm-current-buffer orig-helm-current-buffer))))))


;;; Initialize
;;
;;
(defvar helm-buffers nil
  "All of `helm-buffer' in most recently used order.")
(defun helm-initialize (any-resume any-input any-sources)
  "Start initialization of `helm' session.
For ANY-RESUME ANY-INPUT and ANY-SOURCES See `helm'."
  (helm-log "start initialization: any-resume=%S any-input=%S"
            any-resume any-input)
  (helm-frame-or-window-configuration 'save)
  (setq helm-sources (helm-normalize-sources any-sources))
  (helm-log "sources = %S" helm-sources)
  (helm-hooks 'setup)
  (helm-current-position 'save)
  (if (helm-resume-p any-resume)
      (helm-initialize-overlays (helm-buffer-get))
      (helm-initial-setup))
  (unless (eq any-resume 'noresume)
    (helm-recent-push helm-buffer 'helm-buffers)
    (setq helm-last-buffer helm-buffer))
  (when any-input (setq helm-input any-input helm-pattern any-input))
  (and (helm-resume-p any-resume) (helm-funcall-foreach 'resume))
  (helm-log "end initialization"))

(defun helm-execute-selection-action-1 ()
  "Execute current action."
  (helm-log-run-hook 'helm-before-action-hook)
  (unwind-protect
       (helm-execute-selection-action)
    (helm-aif (get-buffer helm-action-buffer)
        (kill-buffer it))
    (helm-log-run-hook 'helm-after-action-hook)))

(defun helm-restore-position-on-quit ()
  "Restore position in `helm-current-buffer' when quitting."
  (helm-current-position 'restore))

(defun helm-recent-push (elt list-var)
  "Add ELT to the value of LIST-VAR as most recently used value."
  (let ((m (member elt (symbol-value list-var))))
    (and m (set list-var (delq (car m) (symbol-value list-var))))
    (push elt (symbol-value list-var))))


;;; Core: Accessors
;;; rubikitch: I love to create functions to control variables.
(defvar helm-current-position nil
  "Cons of \(point . window-start\)  when `helm' is invoked.
It is needed to restore position in `helm-current-buffer'
when `helm' is keyboard-quitted.")
(defun helm-current-position (save-or-restore)
  "Restore or save current position in `helm-current-buffer'.
Argument SAVE-OR-RESTORE is one of save or restore."
  (case save-or-restore
    (save
     (helm-log "Save position at %S" (cons (point) (window-start)))
     (setq helm-current-position (cons (point) (window-start))))
    (restore
     (helm-log "Restore position at  %S in buffer %s"
               helm-current-position
               (buffer-name (current-buffer)))
     (goto-char (car helm-current-position))
     ;; Fix this position with the NOFORCE arg of `set-window-start'
     ;; otherwise, if there is some other buffer than `helm-current-buffer'
     ;; one, position will be lost.
     (set-window-start (selected-window) (cdr helm-current-position) t))))


;; Internal.
(defvar helm-last-frame-or-window-configuration nil
  "Used to store window or frame configuration when helm start.")
(defun helm-frame-or-window-configuration (save-or-restore)
  "Save or restore last frame or window configuration.
Possible value of SAVE-OR-RESTORE are 'save and 'restore.
window or frame configuration is saved/restored according to values of
`helm-save-configuration-functions'."
  (helm-log-eval helm-save-configuration-functions)
  (case save-or-restore
    (save    (setq helm-last-frame-or-window-configuration
                   (funcall (cdr helm-save-configuration-functions))))
    (restore (funcall (car helm-save-configuration-functions)
                      helm-last-frame-or-window-configuration)
             ;; Restore frame focus.
             (let ((frame
                    (and (listp helm-last-frame-or-window-configuration)
                         (caadr helm-last-frame-or-window-configuration))))
               ;; If `helm-save-configuration-functions' are window functions
               ;; frame should be nil, use current frame.
               (unless (framep frame)
                 (setq frame (selected-frame)))
               (select-frame-set-input-focus frame)))))


;; Core: Display *helm* buffer
(defun helm-display-buffer (buf)
  "Display *helm* buffer BUF."
  (let (pop-up-frames)
    (funcall (with-current-buffer buf helm-display-function) buf)))

(defun helm-default-display-buffer (buf)
  "Default function to display BUF.
Where BUF is generally `helm-buffer'.
It use `switch-to-buffer' or `pop-to-buffer' depending of value of
`helm-samewindow'."
  (funcall (if helm-samewindow 'switch-to-buffer 'pop-to-buffer) buf))


;; Core: initialize
(defun helm-initial-setup ()
  "Initialize helm settings and set up the helm buffer."
  (helm-log-run-hook 'helm-before-initialize-hook)
  (setq helm-current-prefix-arg nil)
  (setq helm-once-called-functions nil)
  (setq helm-delayed-init-executed nil)
  (setq helm-alive-p t)
  (setq helm-current-buffer
        (if (minibuffer-window-active-p (minibuffer-window))
            ;; If minibuffer is active be sure to use it's buffer
            ;; as `helm-current-buffer'.
            (window-buffer (active-minibuffer-window))
            (current-buffer)))
  (setq helm-buffer-file-name buffer-file-name)
  (setq helm-issued-errors nil)
  (setq helm-compiled-sources nil)
  (setq helm-saved-current-source nil)
  (if (or (not split-width-threshold)
          (and (integerp split-width-threshold)
               (>= split-width-threshold (+ (frame-width) 4))))
      (setq helm-split-window-state 'vertical)
      (setq helm-split-window-state 'horizontal))
  ;; Call the init function for sources where appropriate
  (helm-funcall-foreach 'init)
  (setq helm-pattern "")
  (setq helm-input "")
  (setq helm-candidate-cache nil)
  (setq helm-last-sources helm-sources)
  (helm-create-helm-buffer)
  (helm-log-run-hook 'helm-after-initialize-hook))

(defvar helm-reading-pattern nil
  "Whether in `read-string' in helm or not.")

(defun helm-read-pattern-maybe (any-prompt any-input
                                any-preselect any-resume any-keymap
                                any-default any-history)
  "Read pattern with prompt ANY-PROMPT and initial input ANY-INPUT.
For ANY-PRESELECT ANY-RESUME ANY-KEYMAP, See `helm'."
  (if (helm-resume-p any-resume)
      (helm-mark-current-line t)
      (helm-update any-preselect))
  (with-current-buffer (helm-buffer-get)
    (let ((src-keymap (assoc-default 'keymap (helm-get-current-source))))
      ;; Startup with the first keymap found either in current source
      ;; or helm arg, otherwise use global value of `helm-map'.
      ;; This map will be used as a `minibuffer-local-map'.
      ;; Maybe it will be overriden when changing source
      ;; by `helm-maybe-update-keymap'.
      (set (make-local-variable 'helm-map)
           (or src-keymap any-keymap helm-map))
      (helm-log-eval (helm-approximate-candidate-number)
                     helm-execute-action-at-once-if-one
                     helm-quit-if-no-candidate)
      (cond ((and helm-execute-action-at-once-if-one
                  (= (helm-approximate-candidate-number) 1))
             (ignore))
            ((and helm-quit-if-no-candidate
                  (= (helm-approximate-candidate-number) 0))
             (setq helm-quit t)
             (and (functionp helm-quit-if-no-candidate)
                  (funcall helm-quit-if-no-candidate)))
            (t
             (let ((helm-reading-pattern t)
                   (tap (or any-default
                            (with-helm-current-buffer
                              (thing-at-point 'symbol)))))
               (read-from-minibuffer (or any-prompt "pattern: ")
                                     any-input helm-map
                                     nil any-history tap)))))))

(defun helm-maybe-update-keymap ()
  "Handle differents keymaps in multiples sources.
This function is meant to be run in `helm-move-selection-after-hook'.
It will override `helm-map' with the keymap attribute of current source
if some when multiples sources are present."
  (with-helm-window
    (let ((kmap (assoc-default 'keymap (helm-get-current-source))))
      (when kmap (setq overriding-local-map kmap)))))
(add-hook 'helm-move-selection-after-hook 'helm-maybe-update-keymap)

(defun helm-create-helm-buffer ()
  "Create and setup `helm-buffer'."
  (with-current-buffer (get-buffer-create helm-buffer)
    (helm-log "kill local variables: %S" (buffer-local-variables))
    (kill-all-local-variables)
    (set (make-local-variable 'inhibit-read-only) t)
    (buffer-disable-undo)
    (erase-buffer)
    (set (make-local-variable 'inhibit-read-only) t)
    (set (make-local-variable 'helm-last-sources-local) helm-sources)
    (set (make-local-variable 'helm-follow-mode) nil)
    (set (make-local-variable 'helm-display-function) helm-display-function)
    (set (make-local-variable 'helm-selection-point) nil)
    (helm-initialize-persistent-action)
    (helm-log-eval helm-display-function helm-let-variables)
    (loop for (var . val) in helm-let-variables
          do (set (make-local-variable var) val))
    (setq cursor-type nil)
    (setq mode-name "Helm"))
  (helm-initialize-overlays helm-buffer)
  (get-buffer helm-buffer))

(defun helm-initialize-overlays (buffer)
  "Initialize helm overlays in BUFFER."
  (helm-log "overlay setup")
  (if helm-selection-overlay
      ;; make sure the overlay belongs to the helm buffer if
      ;; it's newly created
      (move-overlay helm-selection-overlay (point-min) (point-min)
                    (get-buffer buffer))

      (setq helm-selection-overlay
            (make-overlay (point-min) (point-min) (get-buffer buffer)))
      (overlay-put helm-selection-overlay 'face 'helm-selection)))

(defun helm-hooks (setup-or-cleanup)
  "Add or remove hooks according to SETUP-OR-CLEANUP value.
if SETUP-OR-CLEANUP value is setup add hooks, any other value
will remove hooks.
hooks concerned are `post-command-hook' and `minibuffer-setup-hook'."
  (let ((hooks '((post-command-hook helm-check-minibuffer-input)
                 (minibuffer-setup-hook helm-print-error-messages))))
    (if (eq setup-or-cleanup 'setup)
        (dolist (args hooks) (apply 'add-hook args))
        (dolist (args (reverse hooks)) (apply 'remove-hook args)))))


;; Core: clean up

(defun helm-cleanup ()
  "Clean up the mess when helm exit or quit."
  (helm-log "start cleanup")
  (with-current-buffer helm-buffer
    ;; If we end up for any reasons (error etc...)
    ;; with an helm-buffer staying around (visible),
    ;; we will have no cursor in this buffer when switching to it.
    (setq cursor-type t)
    ;; Call burry-buffer whithout arg
    ;; to be sure helm-buffer is removed from window.
    (bury-buffer)
    ;; Be sure we call this from helm-buffer.
    (helm-funcall-foreach 'cleanup))
  (helm-new-timer 'helm-check-minibuffer-input-timer nil)
  (helm-kill-async-processes)
  (helm-log-run-hook 'helm-cleanup-hook)
  (helm-hooks 'cleanup)
  (helm-frame-or-window-configuration 'restore)
  (setq helm-alive-p nil)
  ;; This is needed in some cases where last input
  ;; is yielded infinitely in minibuffer after helm session.
  (helm-clean-up-minibuffer))

(defun helm-clean-up-minibuffer ()
  "Remove contents of minibuffer."
  (let ((miniwin (minibuffer-window)))
    ;; Clean only current minibuffer used by helm.
    ;; i.e The precedent one is active.
    (unless (minibuffer-window-active-p miniwin)
      (with-current-buffer (window-buffer miniwin)
        (delete-minibuffer-contents)))))


;; Core: input handling
(defun helm-check-minibuffer-input ()
  "Extract input string from the minibuffer and use it maybe."
  (let ((delay (with-current-buffer helm-buffer
                 (and helm-input-idle-delay
                      (max helm-input-idle-delay 0.1)))))
    (if (or (not delay) (helm-action-window))
        (helm-check-minibuffer-input-1)
        (helm-new-timer
         'helm-check-minibuffer-input-timer
         (run-with-idle-timer delay nil 'helm-check-minibuffer-input-1)))))

(defun helm-check-minibuffer-input-1 ()
  "Check minibuffer content."
  (with-helm-quittable
    (with-selected-window (or (active-minibuffer-window)
                              (minibuffer-window))
      (helm-check-new-input (minibuffer-contents)))))

(defun helm-check-new-input (input)
  "Check INPUT string and update the helm buffer if necessary."
  (unless (equal input helm-pattern)
    (setq helm-pattern input)
    (unless (helm-action-window)
      (setq helm-input helm-pattern))
    (helm-log-eval helm-pattern helm-input)
    (helm-update)))


;; Core: source compiler
(defvar helm-compile-source-functions-default helm-compile-source-functions
  "Plug-ins this file provides.")
(defun helm-compile-sources (sources funcs)
  "Compile SOURCES with FUNCS.
See `helm-compile-source-functions'.
Helm plug-ins are realized by this function."
  (mapcar
   (lambda (source)
     (loop with source = (if (listp source) source (symbol-value source))
           for f in funcs
           do (setq source (funcall f source))
           finally (return source)))
   sources))


;; Core: plug-in attribute documentation hack

;; `helm-document-attribute' is public API.
(defadvice documentation-property (after helm-document-attribute activate)
  "Display plug-in attributes' documentation as `helm-sources' docstring."
  (when (eq (ad-get-arg 0) 'helm-sources)
    (setq ad-return-value
          (concat ad-return-value "\n"
                  (mapconcat (lambda (sym) (get sym 'helm-attrdoc))
                             helm-additional-attributes
                             "\n")))))


;; Core: all candidates
(defun helm-process-delayed-init (source)
  "Initialize delayed SOURCE."
  (let ((name (assoc-default 'name source)))
    (unless (member name helm-delayed-init-executed)
      (helm-aif (assoc-default 'delayed-init source)
          (with-current-buffer helm-current-buffer
            (helm-funcall-with-source source it)
            (dolist (f (if (functionp it) (list it) it))
              (add-to-list 'helm-delayed-init-executed name)))))))

(defun helm-get-candidates (source)
  "Retrieve and return the list of candidates from SOURCE."
  (helm-process-delayed-init source)
  (let* ((candidate-source (assoc-default 'candidates source))
         (type-error (lambda ()
                       (error (concat "Candidates must either be a function, "
                                      " a variable or a list: %s")
                              candidate-source)))
         (candidates (condition-case err
                         (helm-interpret-value candidate-source source)
                       (error (funcall type-error)))))
    (cond ((processp candidates) candidates)
          ((listp candidates) (helm-transform-candidates candidates source))
          (t (funcall type-error)))))


(defun helm-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is not yet a cached value."
  (let* ((name (assoc-default 'name source))
         (candidate-cache (assoc name helm-candidate-cache)))
    (cond (candidate-cache
           (helm-log "use cached candidates")
           (cdr candidate-cache))
          (t
           (helm-log "calculate candidates")
           (let ((candidates (helm-get-candidates source)))
             (cond ((processp candidates)
                    (push (cons candidates
                                (append source
                                        (list (cons 'item-count 0)
                                              (cons 'incomplete-line ""))))
                          helm-async-processes)
                    (set-process-filter candidates 'helm-output-filter)
                    (setq candidates nil))
                   ((not (assoc 'volatile source))
                    (setq candidate-cache (cons name candidates))
                    (push candidate-cache helm-candidate-cache)))
             candidates)))))


;;; Core: candidate transformers
(defun helm-transform-mapcar (function args)
  "`mapcar' for candidate-transformer.

ARGS is (cand1 cand2 ...) or ((disp1 . real1) (disp2 . real2) ...)

\(helm-transform-mapcar 'upcase '(\"foo\" \"bar\"))
=> (\"FOO\" \"BAR\")
\(helm-transform-mapcar 'upcase '((\"1st\" . \"foo\") (\"2nd\" . \"bar\")))
=> ((\"1st\" . \"FOO\") (\"2nd\" . \"BAR\"))
"
  (loop for arg in args
        if (consp arg)
        collect (cons (car arg) (funcall function (cdr arg)))
        else
        collect (funcall function arg)))

(defun helm-process-candidate-transformer (candidates source)
  "Execute candidate-transformer function on all CANDIDATES of SOURCE."
  (helm-aif (assoc-default 'candidate-transformer source)
      (helm-composed-funcall-with-source source it candidates)
    candidates))

(defun helm-process-filtered-candidate-transformer (candidates source)
  "Execute filtered-candidate-transformer function on all CANDIDATES of SOURCE."
  (helm-aif (assoc-default 'filtered-candidate-transformer source)
      (helm-composed-funcall-with-source source it candidates source)
    candidates))

(defun helm-process-filtered-candidate-transformer-maybe
    (candidates source process-p)
  "Execute filtered-candidate-transformer function on all CANDIDATES of SOURCE.
This happen if PROCESS-P is non-nil."
  (if process-p
      (helm-process-filtered-candidate-transformer candidates source)
      candidates))

(defun helm-process-real-to-display (candidates source)
  "Execute real-to-display function on all CANDIDATES of SOURCE."
  (helm-aif (assoc-default 'real-to-display source)
      (setq candidates (helm-funcall-with-source
                        source 'mapcar
                        (lambda (cand_)
                          (if (consp cand_)
                              ;; override DISPLAY from candidate-transformer
                              (cons (funcall it (cdr cand_)) (cdr cand_))
                              (cons (funcall it cand_) cand_)))
                        candidates))
    candidates))

(defun helm-transform-candidates (candidates source &optional process-p)
  "Transform CANDIDATES of SOURCE according to candidate transformers.
This happen if PROCESS-P is non-nil."
  (helm-process-real-to-display
   (helm-process-filtered-candidate-transformer-maybe
    (helm-process-candidate-transformer candidates source) source process-p)
   source))


;; Core: narrowing candidates
(defun helm-candidate-number-limit (source)
  "Apply candidate-number-limit attribute value.
This overhide variable `helm-candidate-number-limit'.

e.g:
If \(candidate-number-limit\) is in SOURCE, show all candidates in SOURCE.
If \(candidate-number-limit . 123\) is in SOURCE limit candidate to 123."
  (helm-aif (assq 'candidate-number-limit source)
      (or (cdr it) 99999999)
    (or helm-candidate-number-limit 99999999)))

(defun helm-compute-matches (source)
  "Compute matched results from SOURCE according to its settings."
  (if debug-on-error
      (helm-compute-matches-internal source)
      (condition-case v
          (helm-compute-matches-internal source)
        (error (helm-log-error
                "helm-compute-matches: error when processing source: %s"
                (assoc-default 'name source))
               nil))))

(defun helm-candidate-get-display (candidate)
  "Get searched display part from CANDIDATE.
CANDIDATE is a string, a symbol, or \(DISPLAY . REAL\) cons cell."
  (format "%s" (or (car-safe candidate) candidate)))

(defun helm-process-pattern-transformer (pattern source)
  "Execute pattern-transformer attribute PATTERN function in SOURCE."
  (helm-aif (assoc-default 'pattern-transformer source)
      (helm-composed-funcall-with-source source it pattern)
    pattern))

(defun helm-match-functions (source)
  (or (assoc-default 'match source)
      helm-default-match-functions))

(defmacro helm-accumulate-candidates-internal (cand newmatches
                                               hash item-count limit)
  "Internal, add CAND into NEWMATCHES.
Use HASH to uniq NEWMATCHES.
Argument ITEM-COUNT count the matches.
if ITEM-COUNT reaches LIMIT, exit from inner loop."
  `(unless (gethash ,cand ,hash)
     (puthash ,cand t ,hash)
     (push ,cand ,newmatches)
     (incf ,item-count)
     (when (= ,item-count ,limit) (return))))

(defun helm-take-first-elements (seq n)
  (if (> (length seq) n)
      (setq seq (subseq seq 0 n))
      seq))

(defun helm-match-from-candidates (cands matchfns limit)
  (let (matches)
    (condition-case nil
        (let ((item-count 0) exit)
          (clrhash helm-match-hash)
          (dolist (match matchfns)
            (let (newmatches)
              (dolist (candidate cands)
                (when (funcall match (helm-candidate-get-display candidate))
                  (helm-accumulate-candidates-internal
                   candidate newmatches helm-match-hash item-count limit)))
              (setq matches (append matches (reverse newmatches))))))
      (invalid-regexp (setq matches nil)))
    matches))

(defun helm-compute-matches-internal (source)
  (save-current-buffer
    (let ((matchfns (helm-match-functions source))
          (helm-source-name (assoc-default 'name source))
          (limit (helm-candidate-number-limit source))
          (helm-pattern (helm-process-pattern-transformer
                         helm-pattern source)))
      (helm-process-filtered-candidate-transformer
       (if (or (equal helm-pattern "") (equal matchfns '(identity)))
           (helm-take-first-elements
            (helm-get-cached-candidates source) limit)
           (helm-match-from-candidates
            (helm-get-cached-candidates source) matchfns limit))
       source))))

(defun helm-process-source (source)
  "Display matched results from SOURCE according to its settings."
  (helm-log-eval (assoc-default 'name source))
  (if (assq 'direct-insert-match source) ;experimental
      (helm-process-source--direct-insert-match source)
      (let ((matches (helm-compute-matches source)))
        (when matches
          (helm-insert-header-from-source source)
          (if (not (assq 'multiline source))
              (mapc #'(lambda (m)
                        (helm-insert-match m 'insert source))
                    matches)
              (let ((start (point)) separate)
                (dolist (match matches)
                  (if separate
                      (helm-insert-candidate-separator)
                      (setq separate t))
                  (helm-insert-match match 'insert source))
                (put-text-property start (point) 'helm-multiline t)))))))

(defun helm-process-source--direct-insert-match (source)
  "[EXPERIMENTAL] Insert candidates from `helm-candidate-buffer' in SOURCE."
  (helm-log-eval (assoc-default 'name source))
  (let ((helm-source-name (assoc-default 'name source))
        content-buf)
    (funcall (assoc-default 'candidates source))
    (setq content-buf (helm-candidate-buffer))
    (unless (helm-empty-buffer-p content-buf)
      (helm-insert-header-from-source source)
      (insert-buffer-substring content-buf))))

(defun helm-process-delayed-sources (delayed-sources &optional preselect)
  "Process helm DELAYED-SOURCES.
Move selection to string or regexp PRESELECT if non--nil.
This function is called in `helm-process-delayed-sources-timer'
when emacs is idle for `helm-idle-delay'."
  (with-helm-quittable
    (helm-log-eval (mapcar (lambda (s)
                             (assoc-default 'name s))
                           delayed-sources))
    (with-current-buffer helm-buffer
      (save-excursion
        (goto-char (point-max))
        (mapc 'helm-process-source delayed-sources)
        (when (and (not (helm-empty-buffer-p))
                   ;; No selection yet.
                   (= (overlay-start helm-selection-overlay)
                      (overlay-end helm-selection-overlay)))
          (helm-update-move-first-line 'without-hook)))
      (when preselect (helm-preselect preselect))
      (save-excursion
        (goto-char (point-min))
        (helm-log-run-hook 'helm-update-hook))
      (helm-log-run-hook 'helm-after-update-hook))))


;; Core: *helm* buffer contents
(defvar helm-input-local nil)
(defvar helm-process-delayed-sources-timer nil)
(defun helm-update (&optional preselect)
  "Update candidates list in `helm-buffer' according to `helm-pattern'.
Argument PRESELECT is a string or regexp used to move selection to a particular
place once updating is done.  It should be used on single source because search
is done on whole `helm-buffer' and not on current source."
  (helm-log "start update")
  (helm-kill-async-processes)
  (with-current-buffer (helm-buffer-get)
    (set (make-local-variable 'helm-input-local) helm-pattern)
    (erase-buffer)
    (let (delayed-sources
          normal-sources)
      (unwind-protect ; Process normal sources and store delayed one's.
           (setq delayed-sources
                 (loop for source in (remove-if-not 'helm-update-source-p
                                                    (helm-get-sources))
                       if (helm-delayed-source-p source)
                       collect source
                       else do (progn (push source normal-sources)
                                      (helm-process-source source))))
        (helm-log-eval
         (mapcar (lambda (s) (assoc-default 'name s)) delayed-sources))
        (cond ((and preselect delayed-sources normal-sources)
               ;; Preselection run here when there is
               ;; normal AND delayed sources.
               (helm-log "Update preselect candidate %s" preselect)
               (helm-preselect preselect))
              (delayed-sources ; Preselection and hooks will run later.
               (helm-update-move-first-line 'without-hook))
              (t              ; No delayed sources, run the hooks now.
               (helm-update-move-first-line)
               (helm-log-run-hook 'helm-after-update-hook)
               (when preselect
                 (helm-log "Update preselect candidate %s" preselect)
                 (helm-preselect preselect))))
        (when delayed-sources
          (helm-new-timer
           'helm-process-delayed-sources-timer
           (run-with-idle-timer
            ;; Be sure helm-idle-delay is >
            ;; to helm-input-idle-delay
            ;; otherwise use value of helm-input-idle-delay
            ;; or 0.1 if == to 0.
            (max helm-idle-delay helm-input-idle-delay 0.1) nil
            'helm-process-delayed-sources delayed-sources preselect)))
        (helm-log "end update")))))

(defun helm-update-source-p (source)
  "Wheter SOURCE need updating or not."
  (and (or (not helm-source-filter)
           (member (assoc-default 'name source) helm-source-filter))
       (>= (length helm-pattern)
           (helm-aif (assoc 'requires-pattern source)
               (or (cdr it) 1)
             0))))

(defun helm-delayed-source-p (source)
  "Wheter SOURCE is a delayed source or not."
  (or (assoc 'delayed source)
      (and helm-quick-update
           (< (window-height (get-buffer-window (current-buffer)))
              (line-number-at-pos (point-max))))))

(defun helm-update-move-first-line (&optional without-hook)
  "Goto first line of `helm-buffer'."
  (goto-char (point-min))
  (unless without-hook
    (save-excursion (helm-log-run-hook 'helm-update-hook)))
  (helm-next-line))

;;;###autoload
(defun helm-force-update (&optional preselect)
  "Force recalculation and update of candidates.
If current source has `update' attribute, a function without argument,
call it before update."
  (interactive)
  (let ((source    (helm-get-current-source))
        (selection (helm-get-selection nil t)))
    (when source
      (mapc 'helm-force-update--reinit
            (helm-get-sources)))
    (helm-update preselect)
    ;; If preselect arg exists, `helm-update' should
    ;; have moved to selection, otherwise do it now.
    (unless preselect
      (helm-keep-selection (assoc-default 'name source) selection))
    (with-helm-window (recenter))))

(defun helm-force-update--reinit (source)
  "Reinit SOURCE by calling his update and/or init functions."
  (helm-aif (helm-funcall-with-source
             source 'helm-candidate-buffer)
      (kill-buffer it))
  (dolist (attr '(update init))
    (helm-aif (assoc-default attr source)
        (helm-funcall-with-source source it)))
  (helm-remove-candidate-cache source))

(defun helm-keep-selection (source selection)
  "Switch to SOURCE and goto SELECTION."
  (when (and source selection)
    (with-helm-window
      (helm-goto-source source)
      (forward-char -1)
      (if (search-forward selection nil t)
          (forward-line 0)
          (goto-char (point-min))
          (forward-line 1))
      (helm-mark-current-line))))

(defun helm-remove-candidate-cache (source)
  "Remove SOURCE from `helm-candidate-cache'."
  (setq helm-candidate-cache
        (delete (assoc (assoc-default 'name source)
                       helm-candidate-cache)
                helm-candidate-cache)))

(defun helm-insert-match (match insert-function source)
  "Insert MATCH into `helm-buffer' with INSERT-FUNCTION for SOURCE.
If MATCH is a list then insert the string intended to appear on the display
and store the real value in a text property."
  (let ((start     (point-at-bol (point)))
        (dispvalue (or (car-safe match) match))
        (realvalue (cdr-safe match)))
    (setq dispvalue
          (cond ((symbolp dispvalue) (symbol-name dispvalue))
                ((numberp dispvalue) (number-to-string dispvalue))
                (t dispvalue)))
    (when (stringp dispvalue)
      (funcall insert-function dispvalue)
      ;; Some sources with candidates-in-buffer have already added
      ;; 'helm-realvalue property when creating candidate buffer.
      (unless (get-text-property start 'helm-realvalue)
        (and realvalue
             (put-text-property start (point-at-eol)
                                'helm-realvalue realvalue)))
      (when helm-source-in-each-line-flag
        (put-text-property start (point-at-eol) 'helm-source source))
      (funcall insert-function "\n"))))

(defun helm-insert-header-from-source (source)
  "Insert SOURCE name in `helm-buffer' header.
Maybe insert by overlay additional info after source name if SOURCE have
header-name attribute."
  (let ((name (assoc-default 'name source)))
    (helm-insert-header
     name
     (helm-aif (assoc-default 'header-name source)
         (helm-funcall-with-source source it name)))))

(defun helm-insert-header (name &optional display-string)
  "Insert header of source NAME into the helm buffer.
If DISPLAY-STRING is non--nil and a string, display this additional info
after the source name by overlay."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (put-text-property start (point) 'helm-header-separator t)))
  (let ((start (point)))
    (insert name)
    (put-text-property (point-at-bol)
                       (point-at-eol) 'helm-header t)
    (when display-string
      (overlay-put (make-overlay (point-at-bol) (point-at-eol))
                   'display display-string))
    (insert "\n")
    (put-text-property start (point) 'face 'helm-source-header)))

(defun helm-insert-candidate-separator ()
  "Insert separator of candidates into the helm buffer."
  (insert (propertize helm-candidate-separator 'face 'helm-separator))
  (put-text-property (point-at-bol)
                     (point-at-eol) 'helm-candidate-separator t)
  (insert "\n"))


;; Core: async process
(defun helm-output-filter (process string)
  "From PROCESS process output STRING."
  (helm-output-filter-1 (assoc process helm-async-processes) string))

(defun helm-output-filter-1 (process-assoc string)
  (helm-log-eval string)
  (with-current-buffer helm-buffer
    (let ((source (cdr process-assoc)))
      (save-excursion
        (helm-aif (assoc-default 'insertion-marker source)
            (goto-char it)
          (goto-char (point-max))
          (helm-insert-header-from-source source)
          (setcdr process-assoc
                  (append source `((insertion-marker . ,(point-marker))))))
        (helm-output-filter--process-source
         (car process-assoc) string source
         (helm-candidate-number-limit source))))
    (helm-output-filter--post-process)))

(defun helm-output-filter--process-source (process string source limit)
  (dolist (candidate (helm-transform-candidates
                      (helm-output-filter--collect-candidates
                       (split-string string "\n")
                       (assoc 'incomplete-line source))
                      source t))
    (if (not (assq 'multiline source))
        (helm-insert-match candidate 'insert-before-markers source)
        (let ((start (point)))
          (helm-insert-candidate-separator)
          (helm-insert-match candidate 'insert-before-markers source)
          (put-text-property start (point) 'helm-multiline t)))
    (incf (cdr (assoc 'item-count source)))
    (when (>= (assoc-default 'item-count source) limit)
      (helm-kill-async-process process)
      (return))))

(defun helm-output-filter--collect-candidates (lines incomplete-line-info)
  (helm-log-eval (cdr incomplete-line-info))
  (butlast
   (loop for line in lines collect
         (if (cdr incomplete-line-info)
             (prog1
                 (concat (cdr incomplete-line-info) line)
               (setcdr incomplete-line-info nil))
             line)
         finally (setcdr incomplete-line-info line))))

(defun helm-output-filter--post-process ()
  (helm-log-run-hook 'helm-update-hook)
  (helm-aif (get-buffer-window helm-buffer 'visible)
      (save-selected-window
        (select-window it)
        (helm-skip-noncandidate-line 'next)
        (helm-mark-current-line))))

(defun helm-kill-async-processes ()
  "Kill all known asynchronous processes of `helm-async-processes'."
  (mapc 'helm-kill-async-process (mapcar 'car helm-async-processes))
  (setq helm-async-processes nil))

(defun helm-kill-async-process (process)
  "Kill PROCESS and detach the associated functions."
  (set-process-filter process nil)
  (delete-process process))


;; Core: action
(defun helm-execute-selection-action (&optional
                                        selection action
                                        preserve-saved-action)
  "If a candidate SELECTION is present then perform the associated ACTION on it.
If PRESERVE-SAVED-ACTION is non-nil don't save action."
  (helm-log "executing action")
  (setq action (helm-get-default-action
                (or action
                    helm-saved-action
                    (if (get-buffer helm-action-buffer)
                        (helm-get-selection helm-action-buffer)
                        (helm-get-action)))))
  (let ((source (or helm-saved-current-source
                    (helm-get-current-source))))
    (setq selection (or selection
                        (helm-get-selection)
                        (and (assoc 'accept-empty source) "")))
    (unless preserve-saved-action (setq helm-saved-action nil))
    (if (and selection action)
        (helm-funcall-with-source
         source action
         (helm-coerce-selection selection source)))))

(defun helm-coerce-selection (selection source)
  "Apply coerce attribute function to SELECTION in SOURCE.
Coerce source with coerce function."
  (helm-aif (assoc-default 'coerce source)
      (helm-funcall-with-source source it selection)
    selection))

(defun helm-get-default-action (action)
  "Get the first ACTION value of action list in source."
  (if (and (listp action) (not (functionp action)))
      (cdar action)
      action))

;;;###autoload
(defun helm-select-action ()
  "Select an action for the currently selected candidate.
If action buffer is selected, back to the helm buffer."
  (interactive)
  (helm-log-run-hook 'helm-select-action-hook)
  (cond ((get-buffer-window helm-action-buffer 'visible)
         (set-window-buffer (get-buffer-window helm-action-buffer)
                            helm-buffer)
         (kill-buffer helm-action-buffer)
         (helm-set-pattern helm-input 'noupdate))
        (t
         (setq helm-saved-selection (helm-get-selection))
         (unless helm-saved-selection
           (error "Nothing is selected"))
         (setq helm-saved-current-source (helm-get-current-source))
         (let ((actions (helm-get-action)))
           (if (functionp actions)
               (message "Sole action: %s" actions)
               (helm-show-action-buffer actions)
               (helm-delete-minibuffer-contents)
               ;; Make `helm-pattern' differs from the previous value.
               (setq helm-pattern 'dummy)
               (helm-check-minibuffer-input))))))

(defun helm-show-action-buffer (actions)
  (with-current-buffer (get-buffer-create helm-action-buffer)
    (erase-buffer)
    (buffer-disable-undo)
    (set-window-buffer (get-buffer-window helm-buffer) helm-action-buffer)
    (set (make-local-variable 'helm-sources)
         `(((name . "Actions")
            (volatile)
            (candidates . ,actions)
            (candidate-number-limit))))
    (set (make-local-variable 'helm-source-filter) nil)
    (set (make-local-variable 'helm-selection-overlay) nil)
    (helm-initialize-overlays helm-action-buffer)))


;; Core: selection
(defun helm-move-selection-common (move-func unit direction)
  "Move the selection marker to a new position wit function MOVE-FUNC.
It is determined by UNIT and DIRECTION."
  (unless (or (helm-empty-buffer-p (helm-buffer-get))
              (not (helm-window)))
    (with-helm-window
      (helm-log-run-hook 'helm-move-selection-before-hook)
      (funcall move-func)
      (helm-skip-noncandidate-line direction)
      (helm-display-source-at-screen-top-maybe unit)
      (when (helm-get-previous-header-pos)
        (helm-mark-current-line))
      (helm-display-mode-line (helm-get-current-source))
      (helm-log-run-hook 'helm-move-selection-after-hook))))

(defun helm-display-source-at-screen-top-maybe (unit)
  (when (and helm-display-source-at-screen-top (eq unit 'source))
    (set-window-start (selected-window)
                      (save-excursion (forward-line -1) (point)))))

(defun helm-skip-noncandidate-line (direction)
  (helm-skip-header-and-separator-line direction)
  (and (bobp) (forward-line 1))     ;skip first header
  (and (eobp) (forward-line -1)))   ;avoid last empty line


(defun helm-skip-header-and-separator-line (direction)
  "Skip source header or candidate separator when going to next/previous line.
Possible value of DIRECTION are 'next or 'previous."
  (while (and (not (bobp))
              (or (helm-pos-header-line-p)
                  (helm-pos-candidate-separator-p)))
    (forward-line (if (and (eq direction 'previous)
                           (not (eq (point-at-bol) (point-min))))
                      -1 1))))

;; Internal
;; The string to display in mode-line.
(defvar helm-mode-line-string-real nil)
(defun helm-display-mode-line (source)
  "Setup mode-line and header-line for `helm-buffer'."
  (set (make-local-variable 'helm-mode-line-string)
       (helm-interpret-value (or (assoc-default 'mode-line source)
                                 (default-value 'helm-mode-line-string))
                             source))
  (if helm-mode-line-string
      (setq mode-line-format
            '(" " mode-line-buffer-identification " "
              (line-number-mode "L%l") " " (helm-follow-mode "(HF) ")
              (:eval (helm-show-candidate-number
                      (when (listp helm-mode-line-string)
                        (car helm-mode-line-string))))
              " " helm-mode-line-string-real " -%-")
            helm-mode-line-string-real
            (substitute-command-keys (if (listp helm-mode-line-string)
                                         (cadr helm-mode-line-string)
                                         helm-mode-line-string)))
      (setq mode-line-format (default-value 'mode-line-format)))
  
  (let* ((hlstr (helm-interpret-value
                 (assoc-default 'header-line source) source))
         (hlend (make-string (- (window-width) (length hlstr)) ? )))
    (setq header-line-format
          (propertize (concat " " hlstr hlend) 'face 'helm-header))))
  
(defun helm-show-candidate-number (&optional name)
  "Used to display candidate number in mode-line.
You can specify NAME of candidates e.g \"Buffers\" otherwise
it is \"Candidate\(s\)\" by default."
  (propertize
   (format "[%s %s]"
           (helm-approximate-candidate-number 'in-current-source)
           (or name "Candidate(s)"))
   'face 'helm-candidate-number))

;;;###autoload
(defun helm-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (helm-move-selection-common
   (lambda ()
     (if (not (helm-pos-multiline-p))
         (forward-line -1)      ;double forward-line is meaningful
         (forward-line -1)        ;because evaluation order is important
         (helm-skip-header-and-separator-line 'previous)
         (let ((header-pos (helm-get-previous-header-pos))
               (separator-pos (helm-get-previous-candidate-separator-pos)))
           (when header-pos
             (goto-char (if (or (null separator-pos)
                                (< separator-pos header-pos))
                            header-pos ; first candidate
                            separator-pos))
             (forward-line 1)))))
   'line 'previous))

;;;###autoload
(defun helm-next-line ()
  "Move selection to the next line."
  (interactive)
  (helm-move-selection-common
   (lambda ()
     (if (not (helm-pos-multiline-p))
         (forward-line 1)
         (let ((header-pos (helm-get-next-header-pos))
               (separator-pos (helm-get-next-candidate-separator-pos)))
           (cond ((and separator-pos
                       (or (null header-pos) (< separator-pos header-pos)))
                  (goto-char separator-pos))
                 (header-pos
                  (goto-char header-pos))))))
   'line 'next))

;;;###autoload
(defun helm-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (helm-move-selection-common
   (lambda ()
     (condition-case nil
         (scroll-down)
       (beginning-of-buffer (goto-char (point-min)))))
   'page 'previous))

;;;###autoload
(defun helm-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (helm-move-selection-common
   (lambda ()
     (condition-case nil
         (scroll-up)
       (end-of-buffer (goto-char (point-max)))))
   'page 'next))

;;;###autoload
(defun helm-beginning-of-buffer ()
  "Move selection at the top."
  (interactive)
  (helm-move-selection-common
   (lambda () (goto-char (point-min)))
   'edge 'previous))

;;;###autoload
(defun helm-end-of-buffer ()
  "Move selection at the bottom."
  (interactive)
  (helm-move-selection-common
   (lambda () (goto-char (point-max)))
   'edge 'next))

;;;###autoload
(defun helm-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (helm-move-selection-common
   (lambda ()
     (forward-line -1)
     (if (bobp)
         (goto-char (point-max))
         (helm-skip-header-and-separator-line 'previous))
     (goto-char (helm-get-previous-header-pos))
     (forward-line 1))
   'source 'previous))

;;;###autoload
(defun helm-next-source ()
  "Move selection to the next source."
  (interactive)
  (helm-move-selection-common
   (lambda ()
     (goto-char (or (helm-get-next-header-pos) (point-min))))
   'source 'next))

(defun helm-goto-source (source-or-name)
  "Move the selection to the source SOURCE-OR-NAME."
  (helm-move-selection-common
   (lambda ()
     (goto-char (point-min))
     (let ((name (if (stringp source-or-name) source-or-name
                     (assoc-default 'name source-or-name))))
       (condition-case err
           (while (not (string= name (helm-current-line-contents)))
             (goto-char (helm-get-next-header-pos)))
         (error (message "")))))
   'source 'next))

(defun helm-mark-current-line (&optional resumep)
  "Move `helm-selection-overlay' to current line.
Note that this is not related with visibles marks, which are used
to mark candidates."
  (with-helm-window
    (when resumep
      (goto-char helm-selection-point))
    (move-overlay
     helm-selection-overlay (point-at-bol)
     (if (helm-pos-multiline-p)
         (let ((header-pos (helm-get-next-header-pos))
               (separator-pos (helm-get-next-candidate-separator-pos)))
           (or (and (null header-pos) separator-pos)
               (and header-pos separator-pos (< separator-pos header-pos)
                    separator-pos)
               header-pos
               (point-max)))
         (1+ (point-at-eol))))
    (setq helm-selection-point (overlay-start helm-selection-overlay)))
  (helm-follow-execute-persistent-action-maybe))

(defun helm-this-command-key ()
  (event-basic-type (elt (this-command-keys-vector) 0)))

(defvar helm-exit-status 0
  "Flag to inform whether helm have exited or quitted.
Exit with 0 mean helm have exited executing an action.
Exit with 1 mean helm have quitted with \\[keyboard-quit]
It is useful for example to restore a window config if helm abort
in special cases.
See `helm-exit-minibuffer' and `helm-keyboard-quit'.")

(defvar helm-minibuffer-confirm-state nil)

;;;###autoload
(defun helm-confirm-and-exit-minibuffer ()
  "Maybe ask for confirmation when exiting helm.
It is similar to `minibuffer-complete-and-exit' adapted to helm.
If `minibuffer-completion-confirm' value is 'confirm,
send in minibuffer confirm message and exit on next hit.
If `minibuffer-completion-confirm' value is t,
don't exit and send message 'no match'."
  (interactive)
  (let* ((empty-buffer-p (with-current-buffer helm-buffer
                           (eq (point-min) (point-max))))
         (unknow (and (not empty-buffer-p)
                      (string= (get-text-property
                                0 'display (helm-get-selection nil 'withprop))
                               "[?]"))))
    (cond ((and (or empty-buffer-p unknow)
                (eq minibuffer-completion-confirm 'confirm))
           (setq helm-minibuffer-confirm-state
                 'confirm)
           (setq minibuffer-completion-confirm nil)
           (minibuffer-message " [confirm]"))
          ((and (or empty-buffer-p unknow)
                (eq minibuffer-completion-confirm t))
           (minibuffer-message " [No match]"))
          (t
           (setq helm-minibuffer-confirm-state nil)
           (helm-exit-minibuffer)))))
(add-hook 'helm-after-update-hook 'helm-confirm-and-exit-hook)

(defun helm-confirm-and-exit-hook ()
  "Restore `minibuffer-completion-confirm' when helm update."
  (unless (or (eq minibuffer-completion-confirm t)
              (not helm-minibuffer-confirm-state))
    (setq minibuffer-completion-confirm
          helm-minibuffer-confirm-state)))

;;;###autoload
(defun helm-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (interactive)
  (unless helm-current-prefix-arg
    (setq helm-current-prefix-arg current-prefix-arg))
  (setq helm-exit-status 0)
  (exit-minibuffer))

;;;###autoload
(defun helm-keyboard-quit ()
  "Quit minibuffer in helm.
If action buffer is displayed, kill it."
  (interactive)
  (when (get-buffer-window helm-action-buffer 'visible)
    (kill-buffer helm-action-buffer))
  (setq helm-exit-status 1)
  (abort-recursive-edit))

(defun helm-get-next-header-pos ()
  "Return the position of the next header from point."
  (next-single-property-change (point) 'helm-header))

(defun helm-get-previous-header-pos ()
  "Return the position of the previous header from point."
  (previous-single-property-change (point) 'helm-header))

(defun helm-pos-multiline-p ()
  "Return non-nil if the current position is in the multiline source region."
  (get-text-property (point) 'helm-multiline))

(defun helm-get-next-candidate-separator-pos ()
  "Return the position of the next candidate separator from point."
  (next-single-property-change (point) 'helm-candidate-separator))

(defun helm-get-previous-candidate-separator-pos ()
  "Return the position of the previous candidate separator from point."
  (previous-single-property-change (point) 'helm-candidate-separator))

(defun helm-pos-header-line-p ()
  "Return t if the current line is a header line."
  (or (get-text-property (point-at-bol) 'helm-header)
      (get-text-property (point-at-bol) 'helm-header-separator)))

(defun helm-pos-candidate-separator-p ()
  "Return t if the current line is a candidate separator."
  (get-text-property (point-at-bol) 'helm-candidate-separator))


;;; Debugging
;;
;;
;;;###autoload
(defun helm-debug-output ()
  "Show all helm-related variables at this time."
  (interactive)
  (helm-help-internal " *Helm Debug*" 'helm-debug-output-function))

(defun helm-debug-output-function (&optional vars)
  (message "Calculating all helm-related values...")
  (insert "If you debug some variables or forms, set `helm-debug-forms'
to a list of forms.\n\n")
  (dolist (v (or vars
                 helm-debug-forms
                 (apropos-internal "^helm-" 'boundp)))
    (insert "** "
            (pp-to-string v) "\n"
            (pp-to-string (with-current-buffer helm-buffer (eval v))) "\n"))
  (message "Calculating all helm-related values...Done"))


;; Core: misc
(defun helm-kill-buffer-hook ()
  "Remove tick entry from `helm-tick-hash' when killing a buffer."
  (loop for key being the hash-keys in helm-tick-hash
        if (string-match (format "^%s/" (regexp-quote (buffer-name))) key)
        do (remhash key helm-tick-hash)))
(add-hook 'kill-buffer-hook 'helm-kill-buffer-hook)

(defun helm-preselect (candidate-or-regexp)
  "Move `helm-selection-overlay' to CANDIDATE-OR-REGEXP on startup."
  (with-helm-window
    (when candidate-or-regexp
      (goto-char (point-min))
      ;; go to first candidate of first source
      (forward-line 1)
      (let ((start (point)))
        (or (re-search-forward
             (concat "^" (regexp-quote candidate-or-regexp) "$") nil t)
            (re-search-forward candidate-or-regexp nil t)
            (search-forward candidate-or-regexp nil t)
            (goto-char start))))
    (helm-mark-current-line)))

;;;###autoload
(defun helm-delete-current-selection ()
  "Delete the currently selected item."
  (interactive)
  (with-helm-window
    (cond ((helm-pos-multiline-p)
           (helm-aif (helm-get-next-candidate-separator-pos)
               (delete-region (point-at-bol)
                              (1+ (progn (goto-char it) (point-at-eol))))
             ;; last candidate
             (goto-char (helm-get-previous-candidate-separator-pos))
             (delete-region (point-at-bol) (point-max)))
           (when (helm-end-of-source-p)
             (goto-char (or (helm-get-previous-candidate-separator-pos)
                            (point-min)))
             (forward-line 1)))
          (t
           (delete-region (point-at-bol) (1+ (point-at-eol)))
           (when (helm-end-of-source-p) (forward-line -1))))
    (helm-mark-current-line)))

(defun helm-end-of-source-p ()
  "Return non--nil if we are at eob or end of source."
  (save-excursion
    (forward-line 1)
    (or (eq (point-at-bol) (point-at-eol))
        (helm-pos-header-line-p)
        (eobp))))

(defun helm-edit-current-selection-internal (func)
  (with-helm-window
    (beginning-of-line)
    (let ((realvalue (get-text-property (point) 'helm-realvalue)))
      (funcall func)
      (beginning-of-line)
      (and realvalue
           (put-text-property (point) (point-at-eol)
                              'helm-realvalue realvalue))
      (helm-mark-current-line))))

(defmacro helm-edit-current-selection (&rest forms)
  "Evaluate FORMS at current selection in the helm buffer.
You can edit the line."
  (declare (indent 0) (debug t))
  `(helm-edit-current-selection-internal
    (lambda () ,@forms)))

(defun helm-set-pattern (pattern &optional noupdate)
  "Set minibuffer contents to PATTERN.
if optional NOUPDATE is non-nil, helm buffer is not changed."
  (with-selected-window (or (active-minibuffer-window) (minibuffer-window))
    (delete-minibuffer-contents)
    (insert pattern))
  (when noupdate
    (setq helm-pattern pattern)
    (helm-hooks 'cleanup)
    (run-with-idle-timer 0 nil 'helm-hooks 'setup)))

;;;###autoload
(defun helm-delete-minibuffer-contents ()
  "Same as `delete-minibuffer-contents' but this is a command."
  (interactive)
  (helm-set-pattern ""))
(defalias 'helm-delete-minibuffer-content 'helm-delete-minibuffer-contents)


;;; Plugins
;;
;; Built-in plug-in: type
(defun helm-compile-source--type (source)
  (helm-aif (assoc-default 'type source)
      (append source (assoc-default it helm-type-attributes) nil)
    source))

;; `define-helm-type-attribute' is public API.

(defun helm-add-type-attribute (type definition)
  (helm-aif (assq type helm-type-attributes)
      (setq helm-type-attributes (delete it helm-type-attributes)))
  (push (cons type definition) helm-type-attributes))

(defvar helm-types nil)
(defun helm-document-type-attribute (type doc)
  (add-to-list 'helm-types type t)
  (put type 'helm-typeattrdoc
       (concat "- " (symbol-name type) "\n\n" doc "\n")))

(defadvice documentation-property (after helm-document-type-attribute activate)
  "Display type attributes' documentation as `helm-type-attributes' docstring."
  (when (eq (ad-get-arg 0) 'helm-type-attributes)
    (setq ad-return-value
          (concat ad-return-value "\n\n++++ Types currently defined ++++\n"
                  (mapconcat (lambda (sym) (get sym 'helm-typeattrdoc))
                             helm-types "\n")))))

;; Built-in plug-in: dummy
(defun helm-dummy-candidate (candidate source)
  "Use `helm-pattern' as CANDIDATE in SOURCE."
  ;; `source' is defined in filtered-candidate-transformer
  (list helm-pattern))

(defun helm-compile-source--dummy (source)
  (if (assoc 'dummy source)
      (append source
              '((candidates "dummy")
                (accept-empty)
                (match identity)
                (filtered-candidate-transformer . helm-dummy-candidate)
                (volatile)))
      source))

;; Built-in plug-in: candidates-in-buffer
(defun helm-candidates-in-buffer ()
  "Get candidates from the candidates buffer according to `helm-pattern'.

BUFFER is `helm-candidate-buffer' by default.  Each
candidate must be placed in one line.  This function is meant to
be used in candidates-in-buffer or candidates attribute of an
helm source.  Especially fast for many (1000+) candidates.

eg.
 '((name . \"many files\")
   (init . (lambda () (with-current-buffer (helm-candidate-buffer 'local)
                        (insert-many-filenames))))
   (search re-search-forward)  ; optional
   (candidates-in-buffer)
   (type . file))

+===============================================================+
| The new way of making and narrowing candidates: Using buffers |
+===============================================================+

By default, `helm' makes candidates by evaluating the
candidates function, then narrows them by `string-match' for each
candidate.

But this way is very slow for many candidates. The new way is
storing all candidates in a buffer and narrowing them by
`re-search-forward'. Search function is customizable by search
attribute. The important point is that buffer processing is MUCH
FASTER than string list processing and is the Emacs way.

The init function writes all candidates to a newly-created
candidate buffer.  The candidates buffer is created or specified
by `helm-candidate-buffer'.  Candidates are stored in a line.

The candidates function narrows all candidates, IOW creates a
subset of candidates dynamically. It is the task of
`helm-candidates-in-buffer'.  As long as
`helm-candidate-buffer' is used,`(candidates-in-buffer)' is
sufficient in most cases.

Note that `(candidates-in-buffer)' is shortcut of three attributes:
  (candidates . helm-candidates-in-buffer)
  (volatile)
  (match identity)
And `(candidates-in-buffer . func)' is shortcut of three attributes:
  (candidates . func)
  (volatile)
  (match identity)
The expansion is performed in `helm-get-sources'.

The candidates-in-buffer attribute implies the volatile attribute.
The volatile attribute is needed because `helm-candidates-in-buffer'
creates candidates dynamically and need to be called everytime
`helm-pattern' changes.

Because `helm-candidates-in-buffer' plays the role of `match' attribute
function, specifying `(match identity)' makes the source slightly faster.

To customize `helm-candidates-in-buffer' behavior, use search,
get-line and search-from-end attributes. See also `helm-sources' docstring."
  (declare (special source))
  (helm-candidates-in-buffer-1
   (helm-candidate-buffer)
   helm-pattern
   (or (assoc-default 'get-line source)
       #'buffer-substring-no-properties)
   ;; use external variable `source'.
   (or (assoc-default 'search source)
       (if (assoc 'search-from-end source)
           '(helm-candidates-in-buffer-search-from-end)
           '(helm-candidates-in-buffer-search-from-start)))
   (helm-candidate-number-limit source)
   (assoc 'search-from-end source)))

(defun helm-candidates-in-buffer-search-from-start (pattern)
  "Search PATTERN with `re-search-forward' with bound and noerror args."
  (re-search-forward pattern nil t))

(defun helm-candidates-in-buffer-search-from-end (pattern)
  "Search PATTERN with `re-search-backward' with bound and noerror args."
  (re-search-backward pattern nil t))

(defun helm-candidates-in-buffer-1 (buffer pattern get-line-fn
                                    search-fns limit search-from-end)
  ;; buffer == nil when candidates buffer does not exist.
  (when buffer
    (with-current-buffer buffer
      (let ((start-point (if search-from-end (point-max) (point-min)))
            (endp (if search-from-end #'bobp #'eobp)))
        (goto-char (1- start-point))
        (if (string= pattern "")
            (helm-initial-candidates-from-candidate-buffer
             endp get-line-fn limit search-from-end)
            (helm-search-from-candidate-buffer
             pattern get-line-fn search-fns limit search-from-end
             start-point endp))))))

(defun helm-point-is-moved (proc)
  "If point is moved after executing PROC, return t, otherwise nil."
  (/= (point) (progn (funcall proc) (point))))

(defun helm-search-from-candidate-buffer (pattern get-line-fn search-fns
                                          limit search-from-end
                                          start-point endp)
  (let (buffer-read-only
        matches exit newmatches)
    (helm-search-from-candidate-buffer-internal
     (lambda ()
       (clrhash helm-cib-hash)
       (dolist (searcher search-fns)
         (goto-char start-point)
         (setq newmatches nil)
         (loop with item-count = 0
               while (funcall searcher pattern)
               for cand = (funcall get-line-fn (point-at-bol) (point-at-eol))
               do (helm-accumulate-candidates-internal
                   cand newmatches helm-cib-hash item-count limit)
               unless (helm-point-is-moved
                       (lambda ()
                         (if search-from-end
                             (goto-char (1- (point-at-bol)))
                             (forward-line 1))))
               return nil)
         (setq matches (append matches (nreverse newmatches))))
       (delq nil matches)))))

(defun helm-initial-candidates-from-candidate-buffer (endp
                                                      get-line-fn
                                                      limit search-from-end)
  (delq nil (loop with next-line-fn =
                  (if search-from-end
                      (lambda (x) (goto-char (max (1- (point-at-bol)) 1)))
                      #'forward-line)
                  until (funcall endp)
                  for i from 1 to limit
                  collect (funcall get-line-fn (point-at-bol) (point-at-eol))
                  do (funcall next-line-fn 1))))

(defun helm-search-from-candidate-buffer-internal (search-fn)
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

(defun helm-candidate-buffer (&optional create-or-buffer)
  "Register and return a buffer containing candidates of current source.
`helm-candidate-buffer' searches buffer-local candidates buffer first,
then global candidates buffer.

Acceptable values of CREATE-OR-BUFFER:

- nil (omit)
  Only return the candidates buffer.
- a buffer
  Register a buffer as a candidates buffer.
- 'global
  Create a new global candidates buffer,
  named \" *helm candidates:SOURCE*\".
- other non-nil value
  Create a new local candidates buffer,
  named \" *helm candidates:SOURCE*HELM-CURRENT-BUFFER\"."
  (let* ((global-bname (format " *helm candidates:%s*"
                               helm-source-name))
         (local-bname (format " *helm candidates:%s*%s"
                              helm-source-name
                              (buffer-name helm-current-buffer))))
    (flet ((register-func ()
             (setq helm-candidate-buffer-alist
                   (cons (cons helm-source-name create-or-buffer)
                         (delete (assoc helm-source-name
                                        helm-candidate-buffer-alist)
                                 helm-candidate-buffer-alist))))
           (kill-buffers-func ()
             (loop for b in (buffer-list)
                   if (string-match (format "^%s" (regexp-quote global-bname))
                                    (buffer-name b))
                   do (kill-buffer b)))
           (create-func ()
             (with-current-buffer
                 (get-buffer-create (if (eq create-or-buffer 'global)
                                        global-bname
                                        local-bname))
               (buffer-disable-undo)
               (erase-buffer)
               (font-lock-mode -1)))
           (return-func ()
             (or (get-buffer local-bname)
                 (get-buffer global-bname)
                 (helm-aif (assoc-default helm-source-name
                                          helm-candidate-buffer-alist)
                     (and (buffer-live-p it) it)))))
      (when create-or-buffer
        (register-func)
        (unless (bufferp create-or-buffer)
          (and (eq create-or-buffer 'global) (kill-buffers-func))
          (create-func)))
      (return-func))))

(defun helm-init-candidates-in-buffer (buffer data)
  "Register BUFFER with DATA for an helm candidates-in-buffer session."
  (let ((buf (helm-candidate-buffer (get-buffer-create buffer))))
    (with-current-buffer buf (insert data)))
  buffer)

(defun helm-compile-source--candidates-in-buffer (source)
  (helm-aif (assoc 'candidates-in-buffer source)
      (append source
              `((candidates . ,(or (cdr it) 'helm-candidates-in-buffer))
                (volatile) (match identity)))
    source))


;; Utility: resplit helm window
;;;###autoload
(defun helm-toggle-resplit-window ()
  "Toggle resplit helm window, vertically or horizontally."
  (interactive)
  (with-helm-window
    (let ((before-height (window-height)))
      (delete-window)
      (set-window-buffer
       (select-window (if (= (window-height) before-height)
                          (prog1
                              (split-window-vertically)
                            (setq helm-split-window-state 'vertical))
                          (setq helm-split-window-state 'horizontal)
                          (split-window-horizontally)))
       helm-buffer))))

;; Utility: Resize helm window.
(defun helm-enlarge-window-1 (n)
  "Enlarge or narrow helm window.
If N is positive enlarge, if negative narrow."
  (unless helm-samewindow
    (let ((horizontal-p (eq helm-split-window-state 'horizontal)))
      (with-helm-window
        (enlarge-window n horizontal-p)))))

;;;###autoload
(defun helm-narrow-window ()
  "Narrow helm window."
  (interactive)
  (helm-enlarge-window-1 -1))

;;;###autoload
(defun helm-enlarge-window ()
  "Enlarge helm window."
  (interactive)
  (helm-enlarge-window-1 1))

;; Utility: select another action by key
(defun helm-select-nth-action (n)
  "Select the N nth action for the currently selected candidate."
  (setq helm-saved-selection (helm-get-selection))
  (unless helm-saved-selection
    (error "Nothing is selected"))
  (setq helm-saved-action (helm-get-nth-action n (helm-get-action)))
  (helm-exit-minibuffer))

(defun helm-get-nth-action (n action)
  (cond ((and (zerop n) (functionp action))
         action)
        ((listp action)
         (or (cdr (elt action n))
             (error "No such action")))
        ((and (functionp action) (< 0 n))
         (error "Sole action"))
        (t
         (error "Error in `helm-select-nth-action'"))))

;;;###autoload
(defun helm-select-2nd-action ()
  "Select the 2nd action for the currently selected candidate."
  (interactive)
  (helm-select-nth-action 1))

;;;###autoload
(defun helm-select-3rd-action ()
  "Select the 3rd action for the currently selected candidate."
  (interactive)
  (helm-select-nth-action 2))

;;;###autoload
(defun helm-select-4th-action ()
  "Select the 4th action for the currently selected candidate."
  (interactive)
  (helm-select-nth-action 3))

;;;###autoload
(defun helm-select-2nd-action-or-end-of-line ()
  "Select the 2nd action for the currently selected candidate.
This happen when point is at the end of minibuffer.
Otherwise goto the end of minibuffer."
  (interactive)
  (if (eolp)
      (helm-select-nth-action 1)
      (end-of-line)))

;; Utility: Persistent Action
(defmacro with-helm-display-same-window (&rest body)
  "Execute BODY in the window used for persistent action.
Make `pop-to-buffer' and `display-buffer' display in the same window."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-function 'helm-persistent-action-display-buffer))
     ,@body))

(defvar helm-persistent-action-display-window nil)
(defun helm-initialize-persistent-action ()
  (set (make-local-variable 'helm-persistent-action-display-window) nil))

;;;###autoload
(defun* helm-execute-persistent-action
    (&optional (attr 'persistent-action) onewindow)
  "Perform the associated action ATTR without quitting helm.
ATTR default is 'persistent-action', but it can be helm else.
In this case you have to add this new attribute to your source.
When `helm-samewindow' and ONEWINDOW are non--nil,
the helm window is never split in persistent action."
  (interactive)
  (helm-log "executing persistent-action")
  (with-helm-window
    (save-selected-window
      (helm-select-persistent-action-window onewindow)
      (helm-log-eval (current-buffer))
      (let ((helm-in-persistent-action t))
        (with-helm-display-same-window
          (helm-execute-selection-action
           nil
           (or (assoc-default attr (helm-get-current-source))
               (helm-get-action))
           t)
          (helm-log-run-hook 'helm-after-persistent-action-hook))))))


(defun helm-persistent-action-display-window (&optional onewindow)
  "Return the window that will be used for presistent action.
If ONEWINDOW is non--nil window will not be splitted in persistent action
if `helm-samewindow' is non--nil also."
  (with-helm-window
    (setq helm-persistent-action-display-window
          (cond ((window-live-p helm-persistent-action-display-window)
                 helm-persistent-action-display-window)
                ((and helm-samewindow (one-window-p t) (not onewindow))
                 (split-window))
                ((get-buffer-window helm-current-buffer))
                (t
                 (next-window (selected-window) 1))))))

(defun helm-select-persistent-action-window (&optional onewindow)
  "Select the window that will be used for persistent action.
See `helm-persistent-action-display-window' for how to use ONEWINDOW."
  (select-window (get-buffer-window (helm-buffer-get)))
  (select-window
   (setq minibuffer-scroll-window
         (helm-persistent-action-display-window onewindow))))

(defun helm-persistent-action-display-buffer (buf &optional not-this-window)
  "Make `pop-to-buffer' and `display-buffer' display in the same window.
If `helm-persistent-action-use-special-display' is non-nil and
BUF is to be displayed by `special-display-function', use it.
Otherwise ignores `special-display-buffer-names' and `special-display-regexps'.
Argument NOT-THIS-WINDOW if present will be used as
second argument of `display-buffer'."
  (let* ((name (buffer-name buf))
         display-buffer-function pop-up-windows pop-up-frames
         (same-window-regexps
          (unless (and helm-persistent-action-use-special-display
                       (or (member name
                                   (mapcar (lambda (x) (or (car-safe x) x))
                                           special-display-buffer-names))
                           (remove-if-not
                            (lambda (x) (string-match (or (car-safe x) x) name))
                            special-display-regexps)))
            '("."))))
    (display-buffer buf not-this-window)))

;; scroll-other-window(-down)? for persistent-action
(defun helm-scroll-other-window-base (command)
  (with-selected-window (helm-persistent-action-display-window)
    (funcall command helm-scroll-amount)))

;;;###autoload
(defun helm-scroll-other-window ()
  "Scroll other window (not *Helm* window) upward."
  (interactive)
  (helm-scroll-other-window-base 'scroll-up))

;;;###autoload
(defun helm-scroll-other-window-down ()
  "Scroll other window (not *Helm* window) downward."
  (interactive)
  (helm-scroll-other-window-base 'scroll-down))


;; Utility: Visible Mark

(defun helm-clear-visible-mark ()
  (with-current-buffer (helm-buffer-get)
    (mapc 'delete-overlay helm-visible-mark-overlays)
    (set (make-local-variable 'helm-visible-mark-overlays) nil)))
(add-hook 'helm-after-initialize-hook 'helm-clear-visible-mark)

(defvar helm-marked-candidates nil
  "Marked candadates.  List of \(source . real\) pair.")

(defun helm-this-visible-mark ()
  (loop for o in helm-visible-mark-overlays
        when (equal (point-at-bol) (overlay-start o))
        return o))

(defun helm-delete-visible-mark (overlay)
  (setq helm-marked-candidates
        (remove
         (cons (helm-get-current-source) (helm-get-selection))
         helm-marked-candidates))
  (delete-overlay overlay)
  (setq helm-visible-mark-overlays
        (delq overlay helm-visible-mark-overlays)))

(defun helm-make-visible-mark ()
  (let ((o (make-overlay (point-at-bol) (1+ (point-at-eol)))))
    (overlay-put o 'face   'helm-visible-mark)
    (overlay-put o 'source (assoc-default 'name (helm-get-current-source)))
    (overlay-put o 'string (buffer-substring (overlay-start o) (overlay-end o)))
    (overlay-put o 'real   (helm-get-selection))
    (add-to-list 'helm-visible-mark-overlays o))
  (push (cons (helm-get-current-source) (helm-get-selection))
        helm-marked-candidates))

;;;###autoload
(defun helm-toggle-visible-mark ()
  "Toggle helm visible mark at point."
  (interactive)
  (with-helm-window
    (helm-aif (helm-this-visible-mark)
        (helm-delete-visible-mark it)
      (helm-make-visible-mark))
    (helm-next-line)))

;;;###autoload
(defun helm-mark-all ()
  "Mark all visible unmarked candidates in current source."
  (interactive)
  (with-helm-window
    (save-excursion
      (goto-char (helm-get-previous-header-pos))
      (helm-next-line)
      (let* ((next-head (helm-get-next-header-pos))
             (end       (and next-head
                             (save-excursion
                               (goto-char next-head)
                               (forward-line -1)
                               (point))))
             (maxpoint  (or end (point-max))))
        (while (< (point) maxpoint)
          (helm-mark-current-line)
          (let* ((prefix (get-text-property (point-at-bol) 'display))
                 (cand   (helm-get-selection))
                 (bn     (and (helm-file-completion-source-p)
                              (helm-c-basename cand)))
                 (src    (assoc-default 'name (helm-get-current-source))))
            (when (and (not (helm-this-visible-mark))
                       (not (or (string= prefix "[?]")
                                (string= prefix "[@]"))))
              ;; Don't mark possibles directories ending with . or ..
              ;; autosave files/links and non--existent file.
              (unless
                  (and (or (helm-file-completion-source-p)
                           (equal src "Files from Current Directory"))
                       (or (string-match "^\\.#.*\\|^#.*#$\\|\\.$" bn)
                           ;; We need to test here when not using a transformer
                           ;; that tag prefix (i.e on tramp)
                           (not (file-exists-p cand))))
                (helm-make-visible-mark))))
          (forward-line 1) (end-of-line))))
    (helm-mark-current-line)
    (message "%s candidates marked" (length helm-marked-candidates))))

;;;###autoload
(defun helm-unmark-all ()
  "Unmark all candidates in all sources of current helm session."
  (interactive)
  (with-helm-window
    (let ((len (length helm-marked-candidates)))
      (save-excursion
        (helm-clear-visible-mark))
      (setq helm-marked-candidates nil)
      (helm-mark-current-line)
      (message "%s candidates unmarked" len))))

;;;###autoload
(defun helm-toggle-all-marks ()
  "Toggle all marks.
Mark all visible candidates of current source or unmark all candidates
visible or invisible in all sources of current helm session"
  (interactive)
  (let ((marked (helm-marked-candidates)))
    (if (and (>= (length marked) 1)
             (with-helm-window helm-visible-mark-overlays))
        (helm-unmark-all)
        (helm-mark-all))))

;;;###autoload
(defun helm-display-all-visible-marks ()
  "Show all `helm' visible marks strings.
Only useful for debugging."
  (interactive)
  (with-helm-window
    (lexical-let ((overlays (reverse helm-visible-mark-overlays)))
      (helm-run-after-quit
       (lambda ()
         (with-output-to-temp-buffer "*helm visible marks*"
           (dolist (o overlays) (princ (overlay-get o 'string)))))))))

(defun helm-marked-candidates ()
  "Return marked candidates of current source if any.
Otherwise one element list of current selection.

It is analogous to `dired-get-marked-files'."
  (with-current-buffer (helm-buffer-get)
    (let ((cands
           (if helm-marked-candidates
               (loop with current-src = (helm-get-current-source)
                     for (source . real) in (reverse helm-marked-candidates)
                     when (equal current-src source)
                     collect (helm-coerce-selection real source))
               (list (helm-get-selection)))))
      (helm-log-eval cands)
      cands)))

(defun helm-reset-marked-candidates ()
  (with-current-buffer (helm-buffer-get)
    (set (make-local-variable 'helm-marked-candidates) nil)))

(add-hook 'helm-after-initialize-hook 'helm-reset-marked-candidates)

(defun helm-current-source-name= (name)
  (save-excursion
    (goto-char (helm-get-previous-header-pos))
    (equal name (helm-current-line-contents))))

(defun helm-revive-visible-mark ()
  "Restore marked candidates when helm update display."
  (with-current-buffer helm-buffer
    (dolist (o helm-visible-mark-overlays)
      (goto-char (point-min))
      (while (and (search-forward (overlay-get o 'string) nil t)
                  (helm-current-source-name= (overlay-get o 'source)))
        ;; Calculate real value of candidate.
        ;; It can be nil if candidate have only a display value.
        (let ((real (get-text-property (point-at-bol 0) 'helm-realvalue)))
          (if real
              ;; Check if real value of current candidate is the same
              ;; that the one stored in overlay.
              (and (string= (overlay-get o 'real) real)
                   (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0))))
              (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0)))))))))
(add-hook 'helm-update-hook 'helm-revive-visible-mark)

(defun helm-next-point-in-list (curpos points &optional prev)
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
                    return (1- i))
              (loop for pt in points
                    for i from 0
                    if (< curpos pt)
                    return i))
          points))))

;;;###autoload
(defun helm-next-visible-mark (&optional prev)
  "Move next helm visible mark.
If PREV is non-nil move to precedent."
  (interactive)
  (with-helm-window
    (ignore-errors
      (goto-char (helm-next-point-in-list
                  (point)
                  (sort (mapcar 'overlay-start helm-visible-mark-overlays) '<)
                  prev)))
    (helm-mark-current-line)))

;;;###autoload
(defun helm-prev-visible-mark ()
  "Move previous helm visible mark."
  (interactive)
  (helm-next-visible-mark t))

;; Utility: Selection Paste
;;;###autoload
(defun helm-yank-selection ()
  "Set minibuffer contents to current selection."
  (interactive)
  (helm-set-pattern (helm-get-selection nil t)))

;;;###autoload
(defun helm-kill-selection-and-quit ()
  "Store current selection to kill ring.
You can paste it by typing \\[yank]."
  (interactive)
  (helm-run-after-quit
   (lambda (sel)
     (kill-new sel)
     (message "Killed: %s" sel))
   (helm-get-selection nil t)))


;;; Follow-mode: Automatical execution of persistent-action
;;
;;
;;;###autoload
(define-minor-mode helm-follow-mode
  "Execute persistent action everytime the cursor is moved when enabled."
  :group 'helm
  :init-value t
  (with-current-buffer helm-buffer
    (setq helm-follow-mode (not helm-follow-mode))
    (message "helm-follow-mode is %s"
             (if helm-follow-mode "enabled" "disabled"))))

(defun helm-follow-execute-persistent-action-maybe ()
  "Execute persistent action in mode `helm-follow-mode'.
This happen after `helm-input-idle-delay' secs."
  (and (not (get-buffer-window helm-action-buffer 'visible))
       (buffer-local-value 'helm-follow-mode
                           (get-buffer-create helm-buffer))
       (sit-for (and helm-input-idle-delay
                     (max helm-input-idle-delay 0.1)))
       (helm-window)
       (helm-get-selection)
       (save-excursion
         (helm-execute-persistent-action))))


;; Bug Report
(defvar helm-maintainer-mail-address "emacs-helm@googlegroups.com")

(defvar helm-bug-report-salutation
  "Describe bug below, using a precise recipe.

When I executed M-x ...

How to send a bug report:
  1) Be sure to use the LATEST version of helm.el.
  2) Enable debugger. M-x toggle-debug-on-error or (setq debug-on-error t)
  3) Use Lisp version instead of compiled one: (load \"helm.el\")
  4) If you got an error, please paste *Backtrace* buffer.
  5) Type C-c C-c to send.")

(defvar helm-no-dump-variables
  '(helm-candidate-buffer-alist
    helm-help-message
    helm-candidate-cache
    )
  "Variables not to dump in bug report.")

(defun helm-dumped-variables-in-bug-report ()
  (let ((hash (make-hash-table)))
    (loop for var in (apropos-internal "helm-" 'boundp)
          for vname = (symbol-name var)
          unless (or (string-match "-map$" vname)
                     (string-match "^helm-c-source-" vname)
                     (string-match "-hash$" vname)
                     (string-match "-face$" vname)
                     (memq var helm-no-dump-variables))
          collect var)))

;;;###autoload
(defun helm-send-bug-report ()
  "Send a bug report of helm.el."
  (interactive)
  (with-current-buffer (or helm-last-buffer
                           (current-buffer))
    (reporter-submit-bug-report
     helm-maintainer-mail-address
     "helm.el"
     (helm-dumped-variables-in-bug-report)
     nil nil
     helm-bug-report-salutation)))

;;;###autoload
(defun helm-send-bug-report-from-helm ()
  "Send a bug report of helm.el in helm session."
  (interactive)
  (helm-run-after-quit 'helm-send-bug-report))


(provide 'helm)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-dynamic: t
;; End:

;;; helm.el ends here
