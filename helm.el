;;; helm.el --- Emacs incremental and narrowing framework -*- lexical-binding: t -*-

;; Copyright (C) 2007         Tamas Patrovics
;;               2008 ~ 2011  rubikitch <rubikitch@ruby-lang.org>
;;               2011 ~ 2015  Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

(require 'cl-lib)
(require 'advice) ; Shutup byte compiler about ad-deactivate.
(require 'helm-lib)
(require 'helm-multi-match)
(require 'helm-source)


;;; Multi keys
;;
;;
;;;###autoload
(defun helm-define-multi-key (keymap key functions &optional delay)
  "In KEYMAP, define key sequence KEY for function list FUNCTIONS.
Each function run sequentially each time the key KEY is pressed.
If DELAY is specified switch back to initial function of FUNCTIONS list
after DELAY seconds.
The functions in FUNCTIONS list are functions with no args.
e.g
  \(defun foo ()
    (message \"Run foo\"))
  \(defun bar ()
    (message \"Run bar\"))
  \(defun baz ()
    (message \"Run baz\"))

\(helm-define-multi-key global-map \"<f5> q\" '(foo bar baz) 2)

Each time \"<f5> q\" is pressed the next function is executed, if you wait
More than 2 seconds, next hit will run again the first function and so on."
  (define-key keymap key (helm-make-multi-command functions delay)))

;;;###autoload
(defmacro helm-multi-key-defun (name docstring funs &optional delay)
  "Define NAME as a multi-key command running FUNS.
After DELAY seconds the FUNS list is reinitialised.
See `helm-define-multi-key'."
  (declare (indent 2))
  (setq docstring (if docstring (concat docstring "\n\n")
                    "This is a helmish multi-key command."))
  `(defalias (quote ,name) (helm-make-multi-command ,funs ,delay) ,docstring))

(defun helm-make-multi-command (functions &optional delay)
  "Return an anonymous multi-key command running FUNCTIONS.
Run each function of FUNCTIONS list in turn when called within DELAY seconds."
  (declare (indent 1))
  (let ((funs functions)
        (iter (cl-gensym "helm-iter-key"))
        (timeout delay))
    (eval (list 'defvar iter nil))
    (lambda () (interactive) (helm-run-multi-key-command funs iter timeout))))

(defun helm-run-multi-key-command (functions iterator delay)
  (let ((fn (lambda ()
                (cl-loop for count from 1 to (length functions)
                      collect count)))
        next)
    (unless (and (symbol-value iterator)
                 ;; Reset iterator when another key is pressed.
                 (eq this-command real-last-command))
      (set iterator (helm-iter-list (funcall fn))))
    (setq next (helm-iter-next (symbol-value iterator)))
    (unless next
      (set iterator (helm-iter-list (funcall fn)))
      (setq next (helm-iter-next (symbol-value iterator))))
    (and next (symbol-value iterator) (call-interactively (nth (1- next) functions)))
    (when delay (run-with-idle-timer delay nil `(lambda ()
                                                  (setq ,iterator nil))))))

(helm-multi-key-defun helm-toggle-resplit-and-swap-windows
    "Multi key command to resplit and swap helm window.
First call run `helm-toggle-resplit-window',
second call within 0.5s run `helm-swap-windows'."
  '(helm-toggle-resplit-window helm-swap-windows) 1)

;;;###autoload
(defun helm-define-key-with-subkeys (map key subkey command
                                         &optional other-subkeys menu exit-fn)
  "Allow defining in MAP a KEY and SUBKEY to COMMAND.

This allow typing KEY to call COMMAND the first time and
type only SUBKEY on subsequent calls.

Arg MAP is the keymap to use, SUBKEY is the initial short keybinding to
call COMMAND.

Arg OTHER-SUBKEYS is an alist specifying other short keybindings
to use once started.
e.g:

\(helm-define-key-with-subkeys global-map
   \(kbd \"C-x v n\") ?n 'git-gutter:next-hunk '((?p . git-gutter:previous-hunk))\)


In this example, `C-x v n' will run `git-gutter:next-hunk'
subsequent hits on \"n\" will run this command again
and subsequent hits on \"p\" will run `git-gutter:previous-hunk'.

Arg MENU is a string to display in minibuffer
to describe SUBKEY and OTHER-SUBKEYS.
Arg EXIT-FN specify a function to run on exit.

Any other keys pressed run their assigned command defined in MAP
and exit the loop running EXIT-FN if specified.

NOTE: SUBKEY and OTHER-SUBKEYS bindings support
only char syntax actually (e.g ?n)
so don't use strings, vectors or whatever to define them."
  (declare (indent 1))
  (define-key map key
    (lambda ()
      (interactive)
      (unwind-protect
          (progn
            (call-interactively command)
            (while (let ((input (read-key menu)) other kb com)
                     (setq last-command-event input)
                     (cond
                      ((eq input subkey)
                       (call-interactively command)
                       t)
                      ((setq other (assoc input other-subkeys))
                       (call-interactively (cdr other))
                       t)
                      (t
                       (setq kb (vector last-command-event))
                       (setq com (lookup-key map kb))
                       (if (commandp com)
                           (call-interactively com)
                         (setq unread-command-events
                               (nconc (mapcar 'identity kb)
                                      unread-command-events)))
                       nil)))))
        (and exit-fn (funcall exit-fn))))))


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
    (define-key map (kbd "<C-down>")   'helm-follow-action-forward)
    (define-key map (kbd "<C-up>")     'helm-follow-action-backward)
    (define-key map (kbd "<prior>")    'helm-previous-page)
    (define-key map (kbd "<next>")     'helm-next-page)
    (define-key map (kbd "M-v")        'helm-previous-page)
    (define-key map (kbd "C-v")        'helm-next-page)
    (define-key map (kbd "M-<")        'helm-beginning-of-buffer)
    (define-key map (kbd "M->")        'helm-end-of-buffer)
    (define-key map (kbd "C-g")        'helm-keyboard-quit)
    (define-key map (kbd "<right>")    'helm-next-source)
    (define-key map (kbd "<left>")     'helm-previous-source)
    (define-key map (kbd "<RET>")      'helm-maybe-exit-minibuffer)
    (define-key map (kbd "C-i")        'helm-select-action)
    (define-key map (kbd "C-z")        'helm-execute-persistent-action)
    (define-key map (kbd "C-j")        'helm-execute-persistent-action)
    (define-key map (kbd "C-o")        'helm-next-source)
    (define-key map (kbd "C-l")        'helm-recenter-top-bottom-other-window)
    (define-key map (kbd "M-C-l")      'helm-reposition-window-other-window)
    (define-key map (kbd "C-M-v")      'helm-scroll-other-window)
    (define-key map (kbd "M-<next>")   'helm-scroll-other-window)
    (define-key map (kbd "C-M-y")      'helm-scroll-other-window-down)
    (define-key map (kbd "C-M-S-v")    'helm-scroll-other-window-down)
    (define-key map (kbd "M-<prior>")  'helm-scroll-other-window-down)
    (define-key map (kbd "<C-M-down>") 'helm-scroll-other-window)
    (define-key map (kbd "<C-M-up>")   'helm-scroll-other-window-down)
    (define-key map (kbd "C-@")        'helm-toggle-visible-mark)
    (define-key map (kbd "C-SPC")      'helm-toggle-visible-mark)
    (define-key map (kbd "M-SPC")      'helm-toggle-visible-mark)
    (define-key map (kbd "M-[")        nil)
    (define-key map (kbd "M-(")        'helm-prev-visible-mark)
    (define-key map (kbd "M-)")        'helm-next-visible-mark)
    (define-key map (kbd "C-k")        'helm-delete-minibuffer-contents)
    (define-key map (kbd "C-x C-f")    'helm-quit-and-find-file)
    (define-key map (kbd "M-m")        'helm-toggle-all-marks)
    (define-key map (kbd "M-a")        'helm-mark-all)
    (define-key map (kbd "M-U")        'helm-unmark-all)
    (define-key map (kbd "C-w")        'helm-yank-text-at-point)
    (define-key map (kbd "C-M-a")      'helm-show-all-in-this-source-only)
    (define-key map (kbd "C-M-e")      'helm-display-all-sources)
    (define-key map (kbd "C-r")        'undefined)
    (define-key map (kbd "C-s")        'undefined)
    (define-key map (kbd "M-s")        'undefined)
    (define-key map (kbd "C-}")        'helm-narrow-window)
    (define-key map (kbd "C-{")        'helm-enlarge-window)
    (define-key map (kbd "C-c -")      'helm-swap-windows)
    (define-key map (kbd "C-c C-d")    'helm-delete-current-selection)
    (define-key map (kbd "C-c C-y")    'helm-yank-selection)
    (define-key map (kbd "C-c C-k")    'helm-kill-selection-and-quit)
    (define-key map (kbd "C-c C-i")    'helm-copy-to-buffer)
    (define-key map (kbd "C-c C-f")    'helm-follow-mode)
    (define-key map (kbd "C-c C-u")    'helm-refresh)
    (define-key map (kbd "C-c >")      'helm-toggle-truncate-line)
    (define-key map (kbd "M-p")        'previous-history-element)
    (define-key map (kbd "M-n")        'next-history-element)
    (define-key map (kbd "C-!")        'helm-toggle-suspend-update)
    (define-key map (kbd "C-x b")      'helm-resume-previous-session-after-quit)
    (define-key map (kbd "C-x C-b")    'helm-resume-list-buffers-after-quit)
    ;; Disable `file-cache-minibuffer-complete'.
    (define-key map (kbd "<C-tab>")    'undefined)
    ;; Multi keys
    (define-key map (kbd "C-t")        'helm-toggle-resplit-and-swap-windows)
    ;; Debugging command
    (define-key map (kbd "C-h C-d")    'undefined)
    (define-key map (kbd "C-h C-d")    'helm-enable-or-switch-to-debug)
    ;; Allow to eval keymap without errors.
    (define-key map [f1] nil)
    (define-key map (kbd "C-h C-h")    'undefined)
    (define-key map (kbd "C-h h")      'undefined)
    ;; Use `describe-mode' key in `global-map'.
    (cl-dolist (k (where-is-internal 'describe-mode global-map))
      (define-key map k 'helm-help))
    (define-key map (kbd "C-c ?")    'helm-help)
    ;; Bind all actions from 1 to 12 to their corresponding nth index+1.
    (cl-loop for n from 0 to 12 do
             (define-key map (kbd (format "<f%s>" (1+ n)))
               `(lambda ()
                  (interactive)
                  (helm-select-nth-action ,n))))
    ;; Bind keys to allow executing default action
    ;; on first 9 candidates before and after selection.
    (cl-loop for n from 1 to 9
         for key = (format "C-c %d" n)
         for key- = (format "C-x %d" n)
         for fn = `(lambda ()
                     (interactive)
                     (helm-execute-selection-action-at-nth ,n))
         for fn- = `(lambda ()
                      (interactive)
                      (helm-execute-selection-action-at-nth ,(- n)))
         do (progn
              (define-key map (kbd key) fn)
              (define-key map (kbd key-) fn-)))
    map)
  "Keymap for helm.")


(defgroup helm nil
  "Open helm."
  :prefix "helm-" :group 'convenience)

(defcustom helm-completion-window-scroll-margin 5
  " `scroll-margin' to use for helm completion window.
Which see.  Set to 0 to disable.
NOTE: This have no effect when `helm-display-source-at-screen-top'
id non--nil."
  :group 'helm
  :type  'integer)

(defcustom helm-display-source-at-screen-top t
  "Display candidates at the top of screen.
This happen when using `helm-next-source' and `helm-previous-source'.
NOTE: When non--nil (default) disable `helm-completion-window-scroll-margin'."
  :group 'helm
  :type 'boolean)

(defcustom helm-candidate-number-limit 100
  "Limit candidate number globally.
Do not show more candidates than this limit from individual sources.
It is usually pointless to show hundreds of matches
when the pattern is empty, because it is much simpler to type a
few characters to narrow down the list of potential candidates.

Set it to nil if you don't want this limit."
  :group 'helm
  :type '(choice (const :tag "Disabled" nil) integer))

(defcustom helm-idle-delay 0.01
  "Be idle for this many seconds, before updating in delayed sources.
This is useful for sources involving heavy operations
\(like launching external programs\), so that candidates
from the source are not retrieved unnecessarily if the user keeps typing.

It also can be used to declutter the results helm displays,
so that results from certain sources are not shown with every
character typed, only if the user hesitates a bit.
Be sure to know what you are doing when modifying this."
  :group 'helm
  :type 'float)

(defcustom helm-input-idle-delay 0.01
  "Be idle for this many seconds, before updating.

Unlike `helm-idle-delay', it is also effective for non-delayed sources.
If nil, candidates are collected immediately.

Note:  If this value is too low compared to `helm-idle-delay',
you may have duplicated sources when using multiples sources.
Safe value is always >= `helm-idle-delay'.
Default settings are equal value for both.
Be sure to know what you are doing when modifying this."
  :group 'helm
  :type 'float)

(defcustom helm-exit-idle-delay 0
  "Be idle for this many seconds before exiting minibuffer while helm is updating.
Note that this does nothing when helm-buffer is up to date
\(i.e exit without delay in this condition\)."
  :group 'helm
  :type 'float)

(defcustom helm-full-frame nil
  "Use current window to show the candidates.
If t then Helm doesn't pop up a new window."
  :group 'helm
  :type 'boolean)

(defvaralias 'helm-samewindow 'helm-full-frame)
(make-obsolete-variable 'helm-samewindow 'helm-full-frame "1.4.8.1")

(defcustom helm-quick-update nil
  "If non-nil, suppress displaying sources which are out of screen at first.
They are treated as delayed sources at this input.
This flag makes `helm' a bit faster with many sources."
  :group 'helm
  :type 'boolean)

(defcustom helm-candidate-separator
  "--------------------"
  "Candidates separator of `multiline' source."
  :group 'helm
  :type 'string)

(defcustom helm-save-configuration-functions
  '(set-window-configuration . current-window-configuration)
  "The functions used to restore/save window or frame configurations.
It is a pair where the car is the function to restore window or frame config,
and the cdr is the function to save the window or frame config.

If you want to save and restore frame configuration, set this variable to
 '\(set-frame-configuration . current-frame-configuration\)
NOTE: This may not work properly with own-frame minibuffer settings.
Older version saves/restores frame configuration, but the default is changed now
because flickering can occur in some environment."
  :group 'helm
  :type 'sexp)

(defcustom helm-persistent-action-use-special-display nil
  "If non-nil, use `special-display-function' in persistent action."
  :group 'helm
  :type 'boolean)

(defcustom helm-display-function 'helm-default-display-buffer
  "Function to display *helm* buffer.
It is `helm-default-display-buffer' by default,
which affects `helm-full-frame'."
  :group 'helm
  :type 'symbol)

(defcustom helm-case-fold-search 'smart
  "Add 'smart' option to `case-fold-search'.
When smart is enabled, ignore case in the search strings
if pattern contains no uppercase characters.
Otherwise, with a nil or t value, the behavior is same as
`case-fold-search'.
Default value is smart, other possible values are nil and t.
NOTE: This have no effect in asynchronous sources, you will
have to implement a similar feature directly in the process.
See in helm-grep.el how it is implemented."
  :group 'helm
  :type '(choice (const :tag "Ignore case" t)
          (const :tag "Respect case" nil)
          (other :tag "Smart" 'smart)))

(defcustom helm-file-name-case-fold-search
  (if (memq system-type
            '(cygwin windows-nt ms-dos darwin))
      t
    helm-case-fold-search)
  "Local setting of `helm-case-fold-search' for reading filenames.

See `helm-case-fold-search' for more info."
  :group 'helm
  :type 'symbol)

(defcustom helm-reuse-last-window-split-state nil
  "Reuse the last state of window split, vertical or horizontal.
That is when you use `helm-toggle-resplit-window' the next helm session
will reuse the same window scheme than the one of last session unless
`helm-split-window-default-side' is 'same or 'other."
  :group 'helm
  :type 'boolean)

(defcustom helm-split-window-preferred-function 'helm-split-window-default-fn
  "Default function used for splitting window."
  :group 'helm
  :type 'function)

(defcustom helm-split-window-default-side 'below
  "The default side to display `helm-buffer'.
Must be one acceptable arg for `split-window' SIDE,
that is `below', `above', `left' or `right'.

Other acceptable values are `same' which always display `helm-buffer'
in current window and `other' that display `helm-buffer' below if only one
window or in `other-window-for-scrolling' if available.

A nil value as same effect as `below'.
If `helm-full-frame' is non--nil, it take precedence on this.

See also `helm-split-window-in-side-p' and `helm-always-two-windows' that
take precedence on this.

NOTE: this have no effect if `helm-split-window-preferred-function' is not
`helm-split-window-default-fn' unless this new function handle this."
  :group 'helm
  :type 'symbol)

(defcustom helm-split-window-in-side-p nil
  "Force splitting inside selected window when non--nil.
See also `helm-split-window-default-side'.

NOTE: this have no effect if `helm-split-window-preferred-function' is not
`helm-split-window-default-fn' unless this new function handle this."
  :group 'helm
  :type 'boolean)

(defcustom helm-always-two-windows nil
  "When non--nil helm will use two windows in this frame.
That is one window to display `helm-buffer' and one to display
`helm-current-buffer'.

Note: this have no effect when `helm-split-window-in-side-p' is non--nil,
or when `helm-split-window-default-side' is set to 'same.

When `helm-autoresize-mode' is enabled, setting this to nil
will have no effect.

Also when non-nil it overhides the effect of `helm-split-window-default-side'
set to `other'."
  :group 'helm
  :type 'boolean)

(defcustom helm-sources-using-default-as-input '(helm-source-imenu
                                                 helm-source-imenu-all
                                                 helm-source-info-elisp
                                                 helm-source-etags-select
                                                 helm-source-man-pages
                                                 helm-source-occur
                                                 helm-source-moccur)
  "List of helm sources that need to use `helm--maybe-use-default-as-input'.
When a source is member of this list, default `thing-at-point'
will be used as input."
  :group 'helm
  :type '(repeat (choice symbol)))

(defcustom helm-delete-minibuffer-contents-from-point t
  "When non--nil, `helm-delete-minibuffer-contents' delete region from `point'.
Otherwise delete `minibuffer-contents'.
See documentation of `helm-delete-minibuffer-contents'."
  :group 'helm
  :type 'boolean)

(defcustom helm-follow-mode-persistent nil
  "Retrieve last state of `helm-follow-mode' in next helm session when non--nil.
This will not make it persistent through emacs sessions though,
you will have to set explicitely the `follow' attribute in the source where
you want this mode enabled definitely."
  :group 'helm
  :type 'boolean)

(defcustom helm-prevent-escaping-from-minibuffer t
  "Prevent escaping from minibuffer during helm session."
  :group 'helm
  :type 'boolean)

(defcustom helm-move-to-line-cycle-in-source nil
  "Move to end or beginning of source when reaching top or bottom of source.
This happen when using `helm-next/previous-line'."
  :group 'helm
  :type 'boolean)

(defcustom helm-fuzzy-match-fn 'helm-fuzzy-match
  "The function for fuzzy matching in `helm-source-sync' based sources."
  :group 'helm
  :type 'function)

(defcustom helm-fuzzy-search-fn 'helm-fuzzy-search
  "The function for fuzzy matching in `helm-source-in-buffer' based sources."
  :group 'helm
  :type 'function)

(defcustom helm-fuzzy-sort-fn 'helm-fuzzy-matching-default-sort-fn
  "The sort transformer function used in fuzzy matching.
When nil no sorting will be done."
  :group 'helm
  :type 'function)

(defcustom helm-fuzzy-matching-highlight-fn 'helm-fuzzy-default-highlight-match
  "The function to highlight matches in fuzzy matching.
When nil no highlighting will be done."
  :group 'helm
  :type 'function)

(defcustom helm-autoresize-max-height 40
  "Specifies a maximum height and defaults to the height of helm window's frame in percentage.

See `fit-window-to-buffer' for more infos."
  :group 'helm
  :type 'integer)

(defcustom helm-autoresize-min-height 10
  "Specifies a minimum height and defaults to the height of helm window's frame in percentage.

If nil the default of `window-min-height' is used
See `fit-window-to-buffer' for more infos."
  :group 'helm
  :type 'integer)

(defcustom helm-input-method-verbose-flag nil
  "The default value of `input-method-verbose-flag' to use in helm minibuffer.
It is nil by default to allow helm updating and exiting without turning off
the input method when complex methods are in use, if you set it to any other
value allowed by `input-method-verbose-flag' you will have at each time you want
to exit or helm update to disable the `current-input-method' with `C-\\'."
  :group 'helm
  :type '(radio :tag "A flag to control extra guidance given by input methods in helm."
          (const :tag "Never provide guidance" nil)
          (const :tag "Always provide guidance" t)
          (const :tag "Provide guidance only in complex methods" complex-only)))

(defcustom helm-display-header-line t
  "Display header-line when non nil."
  :group 'helm
  :type 'boolean)

(defcustom helm-inherit-input-method t
  "Inherit `current-input-method' from `current-buffer' when non--nil.
The default is to enable this by default, the user can toggle the current
input method with `toggle-input-method'."
  :group 'helm
  :type 'boolean)

(defcustom helm-echo-input-in-header-line nil
  "Send current input in header-line."
  :group 'helm
  :type 'boolean)


;;; Faces
;;
;;
(defgroup helm-faces nil
  "Customize the appearance of helm."
  :prefix "helm-"
  :group 'faces
  :group 'helm)

(defface helm-source-header
    '((((background dark))
       :background "#22083397778B"
       :foreground "white"
       :weight bold :height 1.3 :family "Sans Serif")
      (((background light))
       :background "#abd7f0"
       :foreground "black"
       :weight bold :height 1.3 :family "Sans Serif"))
  "Face for source header in the helm buffer."
  :group 'helm-faces)

(defface helm-visible-mark
    '((((min-colors 88) (background dark))
       (:background "green1" :foreground "black"))
      (((background dark))
       (:background "green" :foreground "black"))
      (((background light)) :background "#d1f5ea")
      (((min-colors 88))
       (:background "green1"))
      (t (:background "green")))
  "Face for visible mark."
  :group 'helm-faces)

(defface helm-header
    '((t (:inherit header-line)))
  "Face for header lines in the helm buffer."
  :group 'helm-faces)

(defface helm-candidate-number
    '((((background dark)) :background "Yellow" :foreground "black")
      (((background light)) :background "#faffb5" :foreground "black"))
  "Face for candidate number in mode-line." :group 'helm-faces)

(defface helm-selection
    '((((background dark)) :background "ForestGreen"
       :distant-foreground "black")
      (((background light)) :background "#b5ffd1"
       :distant-foreground "black"))
  "Face for currently selected item in the helm buffer."
  :group 'helm-faces)

(defface helm-separator
    '((((background dark)) :foreground "red")
      (((background light)) :foreground "#ffbfb5"))
  "Face for multiline source separator."
  :group 'helm-faces)

(defface helm-action
    '((t (:underline t)))
  "Face for action lines in the helm action buffer."
  :group 'helm-faces)

(defface helm-prefarg
    '((((background dark)) :foreground "green")
      (((background light)) :foreground "red"))
  "Face for showing prefix arg in mode-line."
  :group 'helm-faces)

(defface helm-match
  '((((background light)) :foreground "#b00000")
    (((background dark))  :foreground "gold1"))
  "Face used to highlight matches."
  :group 'helm-faces)

(defface helm-header-line-left-margin
  '((t (:foreground "black" :background "yellow")))
  "Face used to highlight helm-header sign in left-margin."
  :group 'helm-faces)


;;; Variables.
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

(defvar helm-source-filter nil
  "A list of source names to be displayed.
Other sources won't appear in the search results.
If nil then there is no filtering.
See also `helm-set-source-filter'.")

(defvar helm-selection-overlay nil
  "Overlay used to highlight the currently selected item.")

(defvar helm-async-processes nil
  "List of information about asynchronous processes managed by helm.")

(defvar helm-before-initialize-hook nil
  "Run before helm initialization.
This hook is run before init functions in `helm-sources',
that is before creation of `helm-buffer'.
Local variables for `helm-buffer' that need a value from `current-buffer'
can be set here with `helm-set-local-variable'.")

(defvar helm-after-initialize-hook nil
  "Run after helm initialization.
This hook run after `helm-buffer' is created but not from `helm-buffer'
so the hook have to specify in which buffer it should run.")

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

(defvar helm-exit-minibuffer-hook nil
  "Run just before exiting minibuffer.")

(defvar helm-after-persistent-action-hook nil
  "Run after executing persistent action.")

(defvar helm-move-selection-before-hook nil
  "Run before moving selection in `helm-buffer'.")

(defvar helm-move-selection-after-hook nil
  "Run after moving selection in `helm-buffer'.")

(defvar helm-after-preselection-hook nil
  "Run after preselection in `helm-buffer'.")

(defvar helm-window-configuration-hook nil
  "Run when switching to and back from action buffer.")

(defconst helm-restored-variables
  '(helm-candidate-number-limit
    helm-source-filter
    helm-source-in-each-line-flag
    helm-map
    helm-sources)
  "Variables which are restored after `helm' invocation.")

(defvar helm-execute-action-at-once-if-one nil
  "Execute default action and exit when only one candidate is remaining.
It can be also a function called with no args returning a boolean value.")

(defvar helm-quit-if-no-candidate nil
  "Quit when there is no candidates when non--nil.
This variable accepts a function, which is executed if no candidate.")

(defvar helm-source-in-each-line-flag nil
  "Non-nil means add helm-source text-property in each candidate.
experimental feature.")

(defvar helm-debug-variables nil
  "A list of helm variables to show in `helm-debug-output'.
Otherwise all variables started with `helm-' are shown.")

(defvar helm-debug-buffer "*Debug Helm Log*")

(defvar helm-debug nil
  "If non-nil, write log message into `helm-debug-buffer' buffer.
It is disabled by default because `helm-debug-buffer' grows quickly.")

(defvar helm-compile-source-functions
  '(helm-compile-source--type
    helm-compile-source--dummy
    helm-compile-source--candidates-in-buffer)
  "Functions to compile elements of `helm-sources' (plug-in).")

(defvar helm-mode-line-string "\
\\<helm-map>\
\\[helm-help]:Help \
\\[helm-select-action]:Act \
\\[helm-maybe-exit-minibuffer]/\
f1/f2/f-n:NthAct \
\\[helm-toggle-suspend-update]:Tog.suspend"
  "Help string displayed in mode-line in `helm'.
It can be a string or a list of two args, in this case,
first arg is a string that will be used as name for candidates number,
second arg any string to display in mode line.
If nil, use default `mode-line-format'.")

(defvar helm-minibuffer-set-up-hook nil
  "Hook that run at initialization of minibuffer.
Useful for modifying the settings of minibuffer in helm.

Here an example to hide minibuffer when using
`helm-echo-input-in-header-line':

      (add-hook 'helm-minibuffer-set-up-hook #'helm-hide-minibuffer-maybe)

Note that we check `helm-echo-input-in-header-line' value
from `helm-buffer' which allow detecting possible local
value of this var.")

(defvar helm-help-message
     "* Helm Generic Help

\\<helm-map>`helm' is an Emacs incremental completion and selection narrowing framework.

Narrow the list by typing some pattern,
Multiple patterns are allowed by separating with a space.
Select with natural Emacs operations, choose with RET.

** Help

C-h m\t\tRun this generic help for helm.

** Basic Operations

These are the default key bindings:

| Key     | Alternative Keys | Command                                                   |
|---------+------------------+-----------------------------------------------------------|
| C-p     | Up               | Previous Line                                             |
| C-n     | Down             | Next Line                                                 |
| M-v     | PageUp           | Previous Page                                             |
| C-v     | PageDown         | Next Page                                                 |
| Enter   |                  | Execute first (default) action / Select                   |
| M-<     |                  | First Line                                                |
| M->     |                  | Last Line                                                 |
| C-M-S-v | M-PageUp, C-M-y  | Previous Page (other-window)                              |
| C-M-v   | M-PageDown       | Next Page (other-window)                                  |
| Tab     | C-i              | Show action list                                          |
| Left    |                  | Previous Source                                           |
| Right   | C-o              | Next Source                                               |
| C-k     |                  | Delete pattern (with prefix arg delete from point to end) |
| C-j     | C-z              | Persistent Action (Execute and keep helm session)         |

** Shortcuts For nth Action

f1-12: Execute nth 1 to 12 Action(s).

** Shortcuts for executing Default Action on nth candidate

C-x <n> => execute default action on number <n> candidate before selection.
C-c <n> => execute default action on number <n> candidate after selection.

Of course this works only on the 9 first and previous candidates.

Helm provides nothing to visualize candidates numbers, up to you to install
linum-relative package and enable linum-relative in helm..

** Visible Marks

Visible marks store candidate. Some actions uses marked candidates.

** Miscellaneous Commands

\\[helm-toggle-resplit-and-swap-windows]\t\tToggle vertical/horizontal split on first hit and swap helm window on second hit.
\\[helm-quit-and-find-file]\t\tDrop into `helm-find-files'.
\\[helm-delete-current-selection]\t\tDelete selected item (visually).
\\[helm-kill-selection-and-quit]\t\tKill display value of candidate and quit (with prefix arg kill the real value).
\\[helm-yank-selection]\t\tYank selection into pattern.
\\[helm-follow-mode]\t\tToggle automatical execution of persistent action.
\\[helm-follow-action-forward]\tRun persistent action and goto next line.
\\[helm-follow-action-backward]\t\tRun persistent action and goto previous line.
\\[helm-refresh]\t\tRecalculate and redisplay candidates.
\\[helm-toggle-suspend-update]\t\tSuspend/reenable update.
 
** Global Commands

\\<global-map>\\[helm-resume] revives last `helm' session.
It is very useful, so you should bind any key.

** Helm Map
\\{helm-map}"
  "Detailed help message string for `helm'.
It also accepts function or variable symbol.")

(defvar helm-autoresize-mode) ;; Undefined in `helm-default-display-buffer'.


;;; Internal Variables
;;
;;
(defvar helm-current-prefix-arg nil
  "Record `current-prefix-arg' when exiting minibuffer.")
(defvar helm-saved-action nil
  "Saved value of the currently selected action by key.")
(defvar helm-saved-current-source nil
  "Value of the current source when the action list is shown.")
(defvar helm-compiled-sources nil
  "Compiled version of `helm-sources'.")
(defvar helm-in-persistent-action nil
  "Flag whether in persistent-action or not.")
(defvar helm-last-buffer nil
  "`helm-buffer' of previously `helm' session.")
(defvar helm-saved-selection nil
  "Value of the currently selected object when the action list is shown.")
(defvar helm-sources nil
  "[INTERNAL] Value of current sources in use, a list.")
(defvar helm-buffer-file-name nil
  "Variable `buffer-file-name' when `helm' is invoked.")
(defvar helm-candidate-cache (make-hash-table :test 'equal)
  "Holds the available candidate within a single helm invocation.")
(defvar helm-input ""
  "The input typed in the candidates panel.")
(defvar helm-input-local nil
  "Internal, store locally `helm-pattern' value for later use in `helm-resume'.")
(defvar helm-source-name nil)
(defvar helm-current-source nil)
(defvar helm-candidate-buffer-alist nil)
(defvar helm-match-hash (make-hash-table :test 'equal))
(defvar helm-cib-hash (make-hash-table :test 'equal))
(defvar helm-tick-hash (make-hash-table :test 'equal))
(defvar helm-issued-errors nil)
(defvar helm-debug-root-directory nil
  "When non--nil, save helm log to `helm-last-log-file'.
Be aware that if you set that, you will end up with a huge directory
of log files, so use that only for debugging purpose.
See `helm-log-save-maybe' for more info.")
(defvar helm-last-log-file nil
  "The name of the last helm session log file.")
(defvar helm-follow-mode nil)
(defvar helm--local-variables nil)
(defvar helm-split-window-state nil)
(defvar helm--window-side-state nil)
(defvar helm-selection-point nil)
(defvar helm-alive-p nil)
(defvar helm-visible-mark-overlays nil)
(defvar helm-update-blacklist-regexps '("^" "^ *" "$" "!" " " "\\b"
                                        "\\<" "\\>" "\\_<" "\\_>" ".*"))
(defvar helm-force-updating-p nil)
(defvar helm-exit-status 0
  "Flag to inform whether helm have exited or quitted.
Exit with 0 mean helm have exited executing an action.
Exit with 1 mean helm have quitted with \\[keyboard-quit]
It is useful for example to restore a window config if helm abort
in special cases.
See `helm-exit-minibuffer' and `helm-keyboard-quit'.")
(defvar helm-minibuffer-confirm-state nil)
(defvar helm-quit nil)
(defvar helm-attributes nil "List of all `helm' attributes.")
(defvar helm-buffers nil
  "All of `helm-buffer' in most recently used order.")
(defvar helm-current-position nil
  "Cons of \(point . window-start\)  when `helm' is invoked.
It is needed to restore position in `helm-current-buffer'
when `helm' is keyboard-quitted.")
(defvar helm-last-frame-or-window-configuration nil
  "Used to store window or frame configuration when helm start.")
(defvar helm-onewindow-p nil)
(defvar helm-types nil)
(defvar helm--mode-line-string-real nil) ; The string to display in mode-line.
(defvar helm-persistent-action-display-window nil)
(defvar helm-marked-candidates nil
  "Marked candadates.  List of \(source . real\) pair.")
(defvar helm--mode-line-display-prefarg nil)
(defvar helm--temp-follow-flag nil
  "[INTERNAL] A simple flag to notify persistent action we are following.")
(defvar helm--reading-passwd-or-string nil)
(defvar helm--in-update nil)
(defvar helm--in-fuzzy nil)
(defvar helm--maybe-use-default-as-input nil
  "Flag to notify the use of use-default-as-input.
Use only in let-bindings.
Use :default arg of `helm' as input to update display.
Note that if also :input is specified as `helm' arg, it will take
precedence on :default.")
(defvar helm--temp-hooks nil
  "Store temporary hooks added by `with-helm-temp-hook'.")
(defvar helm-truncate-lines nil
  "[Internal] Don't set this globally, it is used as a local var.")
(defvar helm--prompt nil)
(defvar helm--file-completion-sources
  '("Find Files" "Read File Name" "Read File Name History")
  "Sources that use the *find-files mechanism can be added here.
Sources generated by `helm-mode' don't need to be added here, it will
be done automatically.
You should not modify this yourself unless you know what you do.")
;; Same as `ffap-url-regexp' but keep it here to ensure `ffap-url-regexp' is not nil.
(defvar helm--url-regexp "\\(news\\(post\\)?:\\|mailto:\\|file:\\|\\(ftp\\|https?\\|telnet\\|gopher\\|www\\|wais\\)://\\)")

;; Utility: logging
(defun helm-log (format-string &rest args)
  "Log message `helm-debug' is non-nil.
Messages are written to the `helm-debug-buffer' buffer.

Argument FORMAT-STRING is a string to use with `format'.
Use optional arguments ARGS like in `format'."
  (when helm-debug
    (with-current-buffer (get-buffer-create helm-debug-buffer)
      (outline-mode)
      (buffer-disable-undo)
      (set (make-local-variable 'inhibit-read-only) t)
      (goto-char (point-max))
      (insert (let ((tm (current-time)))
                (format (concat (if (string-match "Start session" format-string)
                                    "* " "** ")
                                "%s.%06d (%s)\n %s\n")
                        (format-time-string "%H:%M:%S" tm)
                        (nth 2 tm)
                        (helm-log-get-current-function)
                        (apply #'format (cons format-string args))))))))

(defun helm-log-run-hook (hook)
  "Run HOOK like `run-hooks' but write these actions to helm log buffer."
  (helm-log "Executing %s with value = %S" hook (symbol-value hook))
  (helm-log "Executing %s with global value = %S" hook (default-value hook))
  (run-hooks hook)
  (helm-log "executed %s" hook))

(defun helm-log-get-current-function ()
  "Get function name calling `helm-log'.
The original idea is from `tramp-debug-message'."
  (cl-loop with exclude-func-re = "^helm-\\(?:interpret\\|log\\|.*funcall\\)"
        for btn from 1 to 40
        for btf = (cl-second (backtrace-frame btn))
        for fn  = (if (symbolp btf) (symbol-name btf) "")
        if (and (string-match "^helm" fn)
                (not (string-match exclude-func-re fn)))
        return fn))

(defun helm-log-error (&rest args)
  "Accumulate error messages into `helm-issued-errors'.
ARGS are args given to `format'.
e.g (helm-log-error \"Error %s: %s\" (car err) (cdr err))."
  (apply 'helm-log (concat "ERROR: " (car args)) (cdr args))
  (let ((msg (apply 'format args)))
    (unless (member msg helm-issued-errors)
      (add-to-list 'helm-issued-errors msg))))

(defun helm-log-save-maybe ()
  "May be save log buffer to `helm-last-log-file'.
If `helm-debug-root-directory' is non--nil and a valid directory,
a directory named 'helm-debug-<date of today>'
will be created there and the log recorded in a file named
at the date and time of today in this directory."
  (when (and (stringp helm-debug-root-directory)
             (file-directory-p helm-debug-root-directory)
             helm-debug)
    (let ((logdir (expand-file-name (concat "helm-debug-"
                                            (format-time-string "%Y%m%d"))
                                    helm-debug-root-directory)))
      (make-directory logdir t)
      (with-current-buffer (get-buffer-create helm-debug-buffer)
        (write-region (point-min) (point-max)
                      (setq helm-last-log-file
                            (expand-file-name
                             (format-time-string "%Y%m%d-%H%M%S")
                             logdir))
                      nil 'silent)
        (kill-buffer)))))

;;;###autoload
(defun helm-debug-open-last-log ()
  "Open helm log file of last helm session.
If `helm-last-log-file' is nil, switch to `helm-debug-buffer' ."
  (interactive)
  (if helm-last-log-file
      (view-file helm-last-log-file)
    (switch-to-buffer helm-debug-buffer)
    (view-mode 1) (visual-line-mode 1)))

(defun helm-print-error-messages ()
  "Print error messages in `helm-issued-errors'."
  (and helm-issued-errors
       (message "Helm issued errors: %s"
                (mapconcat 'identity (reverse helm-issued-errors) "\n"))))


;; Programming Tools

(defun helm-this-command ()
  "Return the actual command in action.
Like `this-command' but return the real command,
not `exit-minibuffer' or unwanted functions."
  (cl-loop with bl = '(helm-maybe-exit-minibuffer
                       helm-confirm-and-exit-minibuffer
                       helm-exit-minibuffer
                       exit-minibuffer)
        for count from 1 to 50
        for btf = (backtrace-frame count)
        for fn = (cl-second btf)
        if (and
            ;; In some case we may have in the way an
            ;; advice compiled resulting in byte-code,
            ;; ignore it (Issue #691).
            (symbolp fn)
            (commandp fn)
            (not (memq fn bl)))
        return fn
        else
        if (and (eq fn 'call-interactively)
                (> (length btf) 2))
        return (cadr (cdr btf))))


;; Test tools
(defmacro with-helm-time-after-update (&rest body)
  (helm-with-gensyms (start-time time-elapsed)
    `(let ((,start-time (float-time)) ,time-elapsed)
       (add-hook 'helm-after-update-hook
                 (lambda ()
                   (setq ,time-elapsed (- (float-time) ,start-time))
                   (keyboard-quit)))
       (unwind-protect ,@body
         (remove-hook 'helm-after-update-hook
                      (lambda ()
                        (setq  ,time-elapsed (- (float-time) ,start-time))
                        (keyboard-quit))))
       ,time-elapsed)))


;; Helm API

(defmacro with-helm-restore-variables (&rest body)
  "Restore `helm-restored-variables' after executing BODY."
  (declare (indent 0) (debug t))
  (helm-with-gensyms (orig-vars)
    `(let ((,orig-vars (mapcar (lambda (v)
                                 (cons v (symbol-value v)))
                               helm-restored-variables)))
       (unwind-protect (progn ,@body)
         (cl-loop for (var . value) in ,orig-vars
               do (set var value))
         (helm-log "restore variables")))))

(defmacro with-helm-default-directory (directory &rest body)
  (declare (indent 2) (debug t))
  `(let ((default-directory (or (and ,directory
                                     (file-name-as-directory ,directory))
                                default-directory)))
     ,@body))

(defun helm-default-directory ()
  "Return the local value of `default-directory' in `helm-buffer'."
  (buffer-local-value 'default-directory (get-buffer helm-buffer)))

(defmacro with-helm-temp-hook (hook &rest body)
  "Execute temporarily BODY as a function for HOOK."
  (declare (indent 1) (debug t))
  (helm-with-gensyms (helm--hook)
    `(progn
       (defun ,helm--hook ()
         (unwind-protect
              (progn ,@body)
           (remove-hook ,hook (quote ,helm--hook))
           (fmakunbound (quote ,helm--hook))))
       (push (cons ',helm--hook ,hook) helm--temp-hooks)
       (add-hook ,hook (quote ,helm--hook)))))

(defmacro with-helm-after-update-hook (&rest body)
  "Execute BODY at end of `helm-update'."
  (declare (indent 0) (debug t))
  `(with-helm-temp-hook 'helm-after-update-hook ,@body))

(defmacro with-helm-alive-p (&rest body)
  "Return error when BODY run outside helm context."
  (declare (indent 0) (debug t))
  `(progn
     (if helm-alive-p
         (progn ,@body)
       (error "Running helm command outside of context"))))

(defun helm-attr (attribute-name &optional source compute)
  "Get the value of ATTRIBUTE-NAME of SRC.
If SRC is omitted, use current source.
If COMPUTE is non--nil compute value of ATTRIBUTE-NAME
with `helm-interpret-value'.  COMPUTE can have also 'ignorefn as
value, in this case `helm-interpret-value' will return a function
as value inchanged, but will eval a symbol which is bound
(i.e a variable)."
  (let ((src (or source (helm-get-current-source))))
    (helm-aif (or (assq attribute-name src)
                  (helm-get-attribute-from-source-type attribute-name src))
        (if compute
            (helm-interpret-value (cdr it) src compute)
            (cdr it)))))

(cl-defun helm-attr-defined (attribute-name
                             &optional (src (helm-get-current-source)))
  "Return non-nil if ATTRIBUTE-NAME of SRC is defined.
if SRC is omitted, use current source."
  (and (helm-attr attribute-name src) t))

(cl-defun helm-attrset (attribute-name value
                        &optional
                          (src (helm-get-current-source))
                          alter-type)
  "Set the value of ATTRIBUTE-NAME of source SRC to VALUE.
If ATTRIBUTE-NAME doesn't exists in source it is created with value VALUE.
If ALTER-TYPE is non--nil alter the value of ATTRIBUTE-NAME in `helm-attributes'
if it exists.
If SRC is omitted, use current source.
If operation succeed, return value, otherwise nil."
  (let ((from-type (helm-get-attribute-from-source-type attribute-name src))
        done)
    (helm-aif (or (assq attribute-name src)
                  (and alter-type from-type))
        (prog1 (setcdr it value) (setq done t))
      (unless from-type
        (setcdr src (cons (cons attribute-name value) (cdr src)))
        (setq done t)))
    (and done value)))

(defun helm-get-attribute-from-source-type (attribute source)
  "Get ATTRIBUTE from type attribute of SOURCE."
  (when (assq 'type source)
    (assq attribute
          (assq (cdr (assq 'type source))
                helm-type-attributes))))

(defun helm-get-attribute-from-type (attribute type)
  "Get ATTRIBUTE from TYPE.
arg TYPE is an existing type defined in `helm-type-attributes'."
  (assq attribute (assq type helm-type-attributes)))

(defun helm-get-actions-from-type (source)
  "Get actions list from type attribute of SOURCE."
  (when (assq 'type source)
    (helm-get-attribute-from-source-type 'action source)))

(defun helm-inherit-attribute-from-source (attribute source)
  "Get the ATTRIBUTE of SOURCE."
  (helm-aif (assq attribute source)
      it
    (helm-get-attribute-from-source-type attribute source)))

(defun helm-append-at-nth (seq elm index)
  "Append ELM at INDEX in SEQ."
  (let ((len (length seq)))
    (cond ((> index len) (setq index len))
          ((< index 0) (setq index 0)))
    (if (zerop index)
        (append elm seq)
        (cl-loop for i in seq
                 for count from 1 collect i
                 when (= count index)
                 if (listp elm) append elm
                 else collect elm))))

(defun helm-add-action-to-source (name fn source &optional index)
  "Add new action NAME linked to function FN to SOURCE.
Function FN should be a valid function that take one arg i.e candidate,
argument NAME is a string that will appear in action menu
and SOURCE should be an existing helm source already loaded.
If INDEX is specified, action is added in action list at INDEX,
otherwise it is added at end.
This allow user to add a specific action to an existing source
without modifying source code."
  (let ((actions    (helm-attr 'action source 'ignorefn))
        (new-action (list (cons name fn))))
    (when (functionp actions)
      (setq actions (list (cons "Default action" actions))))
    (helm-attrset 'action
                  (if index
                      (helm-append-at-nth actions new-action index)
                    (append actions new-action))
                  source)))

(defun helm-delete-action-from-source (action-or-name source)
  "Delete ACTION-OR-NAME from SOURCE.
ACTION-OR-NAME can either be the name of action or the symbol function
associated to name."
  (let* ((actions    (helm-attr 'action source 'ignorefn))
         (del-action (if (symbolp action-or-name)
                         (rassoc action-or-name actions)
                       (assoc action-or-name actions))))
    (helm-attrset 'action (delete del-action actions) source)))

(cl-defun helm-add-action-to-source-if (name fn source predicate
                                        &optional (index 4) test-only)
  "Add new action NAME linked to function FN to SOURCE.
Action NAME will be available when the current candidate matches PREDICATE.
This function add an entry in the `action-transformer' attribute
of SOURCE (or create one if not found).
Function PREDICATE should take one arg candidate.
Function FN should be a valid function that take one arg i.e candidate,
argument NAME is a string that will appear in action menu
and SOURCE should be an existing helm source already loaded.
If INDEX is specified, action is added in action list at INDEX.
Value of INDEX should be always >=1, default to 4.
This allow user to add a specific `action-tranformer'
to an existing source without modifying source code.
E.g
Add the action \"Byte compile file async\" linked to
function 'async-byte-compile-file to source `helm-source-find-files'
only when predicate helm-ff-candidates-lisp-p return non--nil:

\(helm-add-action-to-source-if \"Byte compile file async\"
                              'async-byte-compile-file
                              helm-source-find-files
                              'helm-ff-candidates-lisp-p\)."
  (let* ((actions     (helm-attr 'action source 'ignorefn))
         (action-transformers (helm-attr 'action-transformer source))
         (new-action  (list (cons name fn)))
         (transformer `(lambda (actions candidate)
                         (cond ((funcall (quote ,predicate) candidate)
                                (helm-append-at-nth
                                 actions (quote ,new-action) ,index))
                               (t actions)))))
    (when (functionp actions)
      (helm-attrset 'action (list (cons "Default action" actions)) source))
    (when (or (symbolp action-transformers) (functionp action-transformers))
      (setq action-transformers (list action-transformers)))
    (if test-only                       ; debug
        (delq nil (append (list transformer) action-transformers))
      (helm-attrset 'action-transformer
                    (helm-fast-remove-dups
                     (delq nil (append (list transformer) action-transformers))
                     :test 'equal)
                    source))))

(defun helm-set-source-filter (sources)
  "Set the value of `helm-source-filter' to SOURCES and update.

This function sets a filter for helm sources and it may be
called while helm is running. It can be used to toggle
displaying of sources dynamically. For example, additional keys
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
  (let ((cur-disp-sel (with-current-buffer helm-buffer
                        (helm-get-selection nil t))))
    (setq helm-source-filter (helm--normalize-filter-sources sources))
    (helm-log "helm-source-filter = %S" helm-source-filter)
    ;; Use force-update to run init/update functions.
    (helm-force-update (and (stringp cur-disp-sel)
                            (regexp-quote cur-disp-sel)))))

(defun helm--normalize-filter-sources (sources)
  (cl-loop for s in sources collect
           (cond ((symbolp s)
                  (assoc-default 'name (symbol-value s)))
                 ((listp s)
                  (assoc-default 'name s))
                 ((stringp s) s))))

(defun helm-set-sources (sources &optional no-init no-update)
  "Set SOURCES during `helm' invocation.
If NO-INIT is non-nil, skip executing init functions of SOURCES.
If NO-UPDATE is non-nil, skip executing `helm-update'."
  (with-current-buffer helm-buffer
    (setq helm-compiled-sources nil
          helm-sources sources)
    (helm-log "helm-compiled-sources = %S" helm-compiled-sources)
    (helm-log "helm-sources = %S" helm-sources))
  (unless no-init (helm-funcall-foreach 'init))
  (unless no-update (helm-update)))

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
       (helm-log "helm-compiled-sources = %S" helm-compiled-sources)))))

(cl-defun helm-get-selection (&optional (buffer nil buffer-s)
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
          (helm-log "selection = %S" selection)
          selection)))))

(defun helm-get-actions-from-current-source ()
  "Return the associated action for the selected candidate.
It is a function symbol \(sole action\) or list
of \(action-display . function\)."
  (unless (helm-empty-buffer-p (helm-buffer-get))
    (helm-aif (helm-attr 'action-transformer)
        (helm-composed-funcall-with-source
         (helm-get-current-source) it
         (helm-attr 'action nil 'ignorefn)
         ;; Check if the first given transformer
         ;; returns the same set of actions for each
         ;; candidate in marked candidates.
         ;; If so use the car of marked to determine
         ;; the set of actions, otherwise use the selection.
         (if (cl-loop with marked = (helm-marked-candidates)
                      with act = (car (helm-mklist it))
                      with acts = (funcall act nil (car marked))
                      for c in marked
                      always (equal (funcall act nil c) acts))
             (car (helm-marked-candidates))
             (helm-get-selection)))
      (helm-attr 'action nil 'ignorefn))))

(defun helm-get-current-source ()
  "Return the source for the current selection.
Allow also checking if helm-buffer contain candidates."
  (or helm-current-source
      (with-current-buffer (helm-buffer-get)
        (or
         ;; This happen only when `helm-source-in-each-line-flag'
         ;; is non--nil and there is candidates in buffer.
         (get-text-property (point) 'helm-source)
         ;; Return nil when no--candidates.
         (cl-block exit
           ;; This goto-char shouldn't be necessary, but point is moved to
           ;; point-min somewhere else which shouldn't happen.
           (goto-char (overlay-start helm-selection-overlay))
           (let* ((header-pos (or (helm-get-previous-header-pos)
                                  (helm-get-next-header-pos)))
                  (source-name
                   (save-excursion
                     (unless header-pos
                       (cl-return-from exit nil))
                     (goto-char header-pos)
                     (helm-current-line-contents))))
             (cl-loop for source in (helm-get-sources) thereis
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
    (helm-log "buffer = %S" buffer)
    (helm-log "modifiedp = %S" modifiedp)
    modifiedp))

(defun helm-current-buffer-is-modified ()
  "Check if `helm-current-buffer' is modified since `helm' was invoked."
  (helm-buffer-is-modified helm-current-buffer))

(defun helm-run-after-exit (function &rest args)
  "Exectute FUNCTION with ARGS after exiting `helm'.
The action is to call FUNCTION with arguments ARGS.
Contrarily to `helm-exit-and-execute-action' this can be used
to call non--actions functions with any ARGS or no ARGS at all.

Use this on commands invoked from keybindings, but not
on action functions invoked as action from the action menu,
i.e functions called with RET."
  (helm-kill-async-processes)
  (helm-log "function = %S" function)
  (helm-log "args = %S" args)
  (helm-exit-and-execute-action
   (lambda (_candidate)
     (apply function args))))

(defun helm-exit-and-execute-action (action)
  "Exit current helm session and execute ACTION.
Argument ACTION is a function called with one arg (candidate)
and part of the actions of current source.

Use this on commands invoked from keybindings, but not
on action functions invoked as action from the action menu,
i.e functions called with RET."
  (setq helm-saved-action action)
  (setq helm-saved-selection (helm-get-selection))
  (helm-exit-minibuffer))

(defalias 'helm-run-after-quit 'helm-run-after-exit)
(make-obsolete 'helm-run-after-quit 'helm-run-after-exit "1.7.7")
(defalias 'helm-quit-and-execute-action 'helm-exit-and-execute-action)
(make-obsolete 'helm-quit-and-execute-action 'helm-exit-and-execute-action "1.7.7")

(defun helm-interpret-value (value &optional source compute)
  "Interpret VALUE as variable, function or literal and return it.
If VALUE is a function, call it with no arguments and return the value
unless COMPUTE value is 'ignorefn.
If SOURCE compute VALUE for this source.
If VALUE is a variable, return the value.
If VALUE is a symbol, but it is not a function or a variable, cause an error.
Otherwise, return VALUE itself."
  (cond ((and source (functionp value) (not (eq compute 'ignorefn)))
         (helm-funcall-with-source source value))
        ((and (functionp value) (not (eq compute 'ignorefn)))
         (funcall value))
        ((and (symbolp value) (boundp value))
         (symbol-value value))
        ((and (symbolp value) (not (functionp value)))
         (error
          "helm-interpret-value: Symbol must be a function or a variable"))
        (t
         value)))

(defun helm-set-local-variable (&rest args)
  "Bind each pair in ARGS locally to `helm-buffer'.

Use this to set local vars before calling helm.

When used from an init or update function
(i.e when `helm-force-update' is running) the variables are set
using `make-local-variable' within the `helm-buffer'.

Usage: helm-set-local-variable ([VAR VALUE]...)
Just like `setq' except that the vars are not set sequentially.
IOW Don't use VALUE of previous VAR to set the VALUE of next VAR.

\(fn VAR VALUE ...)"
  (if helm-force-updating-p
      (with-helm-buffer
        (cl-loop for i on args by #'cddr
                 do (set (make-local-variable (car i)) (cadr i))))
      (setq helm--local-variables
            (append (cl-loop for i on args by #'cddr
                             collect (cons (car i) (cadr i)))
                    helm--local-variables))))


;; Core: API helper
(cl-defun helm-empty-buffer-p (&optional (buffer helm-buffer))
  "Check if BUFFER have candidates.
Default value for BUFFER is `helm-buffer'."
  (zerop (buffer-size (and buffer (get-buffer buffer)))))

(defun helm-empty-source-p ()
  "Check if current source contains candidates.
This happen only in certains cases when e.g the last element
of a source is deleted without updating the source."
  (with-helm-window
    (or (helm-empty-buffer-p)
        (and (helm-end-of-source-p)
             (eq (point-at-bol) (point-at-eol))
             (or
              (save-excursion
                (forward-line -1)
                (helm-pos-header-line-p))
              (bobp))))))


;; Core: tools

(defun helm-funcall-with-source (source functions &rest args)
  "Call from SOURCE FUNCTIONS list or single function FUNCTIONS with ARGS.
FUNCTIONS can be a symbol or a list of functions.
Return the result of last function call."
  (let ((helm-source-name (assoc-default 'name source))
        (helm-current-source source)
        (funs (if (functionp functions) (list functions) functions)))
    (helm-log "helm-source-name = %S" helm-source-name)
    (helm-log "functions = %S" functions)
    (helm-log "args = %S" args)
    (cl-loop with result for fn in funs
             do (setq result (apply fn args))
             finally return result)))

(defun helm-funcall-foreach (sym &optional sources)
  "Call the associated function to SYM for each source if any."
  (let ((sources (or sources (helm-get-sources))))
    (cl-dolist (source sources)
      (helm-aif (assoc-default sym source)
          (helm-funcall-with-source source it)))))

(defun helm-normalize-sources (sources)
  "If SOURCES is only one source, make a list of one element."
  (cond ((or (and sources
                  (symbolp sources))
             (and (listp sources) (assq 'name sources)))
         (list sources))
        (sources)
        (t helm-sources)))

(defun helm-get-candidate-number (&optional in-current-source)
  "Return candidates number in `helm-buffer'.
If IN-CURRENT-SOURCE is provided return number of candidates
in the source where point is."
  (with-current-buffer (helm-buffer-get)
    (if (or (helm-empty-buffer-p)
            (helm-empty-source-p))
        0
      (save-excursion
        (if in-current-source
            (goto-char (helm-get-previous-header-pos))
          (goto-char (point-min)))
        (forward-line 1)
        (if (helm-pos-multiline-p)
            (save-excursion
              (cl-loop with count-multi = 1
                    while (and (not (if in-current-source
                                        (save-excursion
                                          (forward-line 2)
                                          (or (helm-pos-header-line-p) (eobp)))
                                      (eobp)))
                               (search-forward helm-candidate-separator nil t))
                    do (cl-incf count-multi)
                    finally return count-multi))
          (save-excursion
            (cl-loop with ln = 0
                  while (not (if in-current-source
                                 (or (helm-pos-header-line-p) (eobp))
                               (eobp)))
                  unless (helm-pos-header-line-p)
                  do (cl-incf ln)
                  do (forward-line 1) finally return ln)))))))

(defmacro with-helm-quittable (&rest body)
  "If an error occur in execution of BODY, quit helm safely."
  (declare (indent 0) (debug t))
  `(condition-case _v
       (let (inhibit-quit)
         ,@body)
     (quit (setq quit-flag t)
           (setq helm-quit t)
           (exit-minibuffer)
           (keyboard-quit)
           ;; See comment about this in `with-local-quit'.
           (eval '(ignore nil)))))

(defun helm-compose (arg-lst func-lst)
  "Apply arguments specified in ARG-LST with each function of FUNC-LST.
The result of each function will be the new `car' of ARG-LST.
Each function in FUNC-LST must accept (length ARG-LST) arguments
\(See examples below) .
This function allows easy sequencing of transformer functions.
Where generally, ARG-LST is '(candidates-list source) and FUNC-LST a
list of transformer functions that take one or two arguments depending
we are using 'filtered-candidate-transformer' or 'candidate-transformer'.
e.g
filtered-candidate-transformer:
\(helm-compose '((1 2 3 4 5 6 7)
                '((name . \"A helm source\") (candidates . (a b c))))
              '((lambda (candidates _source)
                  (cl-loop for i in candidates
                        when (cl-oddp i) collect i))
                (lambda (candidates _source)
                  (cl-loop for i in candidates collect (1+ i)))))
=>(2 4 6 8)

candidate-transformer:
\(helm-compose '((1 2 3 4 5 6 7))
                '((lambda (candidates)
                  (cl-loop for i in candidates
                        when (cl-oddp i) collect i))
                (lambda (candidates)
                  (cl-loop for i in candidates collect (1+ i)))))
=> (2 4 6 8)."
  (cl-dolist (func func-lst)
    (setcar arg-lst (apply func arg-lst)))
  (car arg-lst))

(defun helm-composed-funcall-with-source (source funcs &rest args)
  "With SOURCE apply `helm-funcall-with-source' with each FUNCS and ARGS.
This is used in transformers to modify candidates list."
  (if (functionp funcs)
      (apply 'helm-funcall-with-source source funcs args)
      (apply 'helm-funcall-with-source source
             (lambda (&rest oargs) (helm-compose oargs funcs))
             args)))


;; Core: entry point
;; `:allow-nest' is not in this list because it is treated before.
(defconst helm-argument-keys
  '(:sources :input :prompt :resume
    :preselect :buffer :keymap :default :history))

;;;###autoload
(defun helm (&rest plist)
  "Main function to execute helm sources.

Keywords supported:
:sources :input :prompt :resume :preselect
:buffer :keymap :default :history :allow-nest

Extra LOCAL-VARS keywords are supported, see below.

PLIST is a list like \(:key1 val1 :key2 val2 ...\) or
\(&optional sources input prompt resume
            preselect buffer keymap default history\).

Basic keywords are the following:

\:sources

A list of sources used for this session.  It also accepts a
symbol, interpreted as a variable of a helm source
i.e (a symbol can be passed instead of a list of sources).
It also accepts an alist representing a helm source, which is
detected by \(assq 'name ANY-SOURCES\).
NOTE: In this case the source is embedded in the helm command and
have no symbol name, so it is not reachable from outside.
It will be referenced in `helm-sources' as a whole alist.

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
When nil or not present `thing-at-point' will be used instead.
If `helm--maybe-use-default-as-input' is non--nil display will be
updated using :default arg as input unless :input is specified,
which in this case will take precedence on :default
This is a string or a list, in this case the car of the list will
be used as initial default input, but you will be able to cycle in this
list with \\<minibuffer-local-map>\\[next-history-element].

\:history

By default all minibuffer input is pushed to `minibuffer-history',
if an argument HISTORY is provided, input will be pushed to HISTORY.
History element should be a symbol.

\:allow-nest

Allow running this helm command within a running helm session.

Of course, conventional arguments are supported, the two are same.

\(helm :sources sources :input input :prompt prompt :resume resume
       :preselect preselect :buffer buffer :keymap keymap :default default
       :history history\)

and

\(helm sources input prompt resume preselect buffer keymap default history\)

are the same.

However the use of non keyword args is deprecated and should not be used.

Other keywords are interpreted as local variables of this helm session.
The `helm-' prefix can be omitted.  For example,

\(helm :sources 'helm-source-buffers-list
       :buffer \"*buffers*\" :candidate-number-limit 10\)

means starting helm session with `helm-source-buffers'
source in *buffers* buffer and set variable `helm-candidate-number-limit'
to 10 as session local variable.

\(fn &key SOURCES INPUT PROMPT RESUME PRESELECT BUFFER KEYMAP DEFAULT HISTORY ALLOW-NEST OTHER-LOCAL-VARS)"
  (let ((fn (cond ((or (and helm-alive-p (plist-get plist :allow-nest))
                       (and helm-alive-p (memq 'allow-nest plist)))
                   #'helm-nest)
                  ((keywordp (car plist))
                   #'helm)
                  (t #'helm-internal))))
    (if (and helm-alive-p (eq fn #'helm))
        (if (helm-alive-p)
            ;; A helm session is normally running.
            (error "Error: Trying to run helm within a running helm session")
          ;; A helm session is already running and user jump somewhere else
          ;; without desactivating it.
          (with-helm-buffer
            (prog1
                (message "Aborting an helm session running in background")
              ;; `helm-alive-p' will be reset in unwind-protect forms.
              (helm-keyboard-quit))))
      (if (keywordp (car plist))
          ;; Parse `plist' and move not regular `helm-argument-keys'
          ;; to `helm--local-variables', then calling helm on itself
          ;; with normal arguments (the non--arguments-keys removed)
          ;; will end up in [1].
          (progn
            (setq helm--local-variables
                  (append helm--local-variables
                          ;; Vars passed by keyword on helm call
                          ;; take precedence on same vars
                          ;; that may have been passed before helm call.
                          (helm-parse-keys plist)))
            (apply fn (mapcar (lambda (key) (plist-get plist key))
                              helm-argument-keys)))
        (apply fn plist))))) ; [1] fn == helm-internal.

(defun helm-alive-p ()
  "Check if `helm' is alive.
An `helm' session is considered alive if `helm-alive-p' is non--nil,
the `helm-buffer' is visible, and cursor is in minibuffer."
  (and helm-alive-p
       (get-buffer-window helm-buffer 'visible)
       (minibuffer-window-active-p (minibuffer-window))
       (minibufferp (current-buffer))))

(defun helm-parse-keys (keys)
  "Parse the KEYS arguments of `helm'.
Return only the keys that are not in `helm-argument-keys',
prefix them with \"helm\" and convert them to alist.
This allow to add arguments that are not part of `helm-argument-keys',
but are valid helm variables.
e.g :candidate-number-limit will be bound to `helm-candidate-number-limit'
in source.

  (helm-parse-keys '(:sources ((name . \"test\")
                               (candidates . (a b c)))
                     :buffer \"toto\"
                     :candidate-number-limit 4))
  ==> ((helm-candidate-number-limit . 4))."

  (cl-loop for (key value) on keys by #'cddr
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
  ;; Activate the advice for `tramp-read-passwd'.
  (if (fboundp 'advice-add)
      (progn
        (advice-add 'tramp-read-passwd :around #'helm--advice-tramp-read-passwd)
        (advice-add 'ange-ftp-get-passwd :around #'helm--advice-ange-ftp-get-passwd))
      (ad-activate 'tramp-read-passwd)
      (ad-activate 'ange-ftp-get-passwd))
  (helm-log (concat "[Start session] " (make-string 41 ?+)))
  (helm-log "any-prompt = %S" any-prompt)
  (helm-log "any-preselect = %S" any-preselect)
  (helm-log "any-buffer = %S" any-buffer)
  (helm-log "any-keymap = %S" any-keymap)
  (helm-log "any-default = %S" any-default)
  (helm-log "any-history = %S" any-history)
  (setq helm--prompt (or any-prompt "pattern: "))
  (let ((non-essential t)
        (input-method-verbose-flag helm-input-method-verbose-flag)
        (old--cua cua-mode)
        (helm--maybe-use-default-as-input
         (and (null any-input)
              (or helm--maybe-use-default-as-input ; it is let-bounded so use it.
                  (cl-loop for s in (helm-normalize-sources any-sources)
                           thereis (memq s helm-sources-using-default-as-input))))))
    ;; cua-mode overhide local helm bindings.
    ;; disable this stupid thing if enabled.
    (and cua-mode (cua-mode -1))
    (unwind-protect
         (condition-case-unless-debug _v
             (let ( ;; `helm-source-name' is non-nil
                   ;; when `helm' is invoked by action, reset it.
                   helm-source-name
                   helm-current-source
                   helm-in-persistent-action
                   helm-quit
                   (helm-buffer (or any-buffer helm-buffer)))
               (with-helm-restore-variables
                 (helm-initialize
                  any-resume any-input any-default any-sources)
                 (helm-display-buffer helm-buffer)
                 ;; We are now in helm-buffer.
                 (when helm-prevent-escaping-from-minibuffer
                   (helm--remap-mouse-mode 1)) ; Disable mouse bindings.
                 (add-hook 'post-command-hook 'helm--maybe-update-keymap)
                 (add-hook 'post-command-hook 'helm--update-header-line)
                 (helm-log "show prompt")
                 (unwind-protect
                      (helm-read-pattern-maybe
                       any-prompt any-input any-preselect
                       any-resume any-keymap any-default any-history)
                   (helm-cleanup)))
               (prog1
                   (unless helm-quit (helm-execute-selection-action))
                 (helm-log (concat "[End session] " (make-string 41 ?-)))))
           (quit
            (helm-restore-position-on-quit)
            (helm-log (concat "[End session (quit)] " (make-string 34 ?-)))
            nil))
      (if (fboundp 'advice-add)
          (progn
            (advice-remove 'tramp-read-passwd
                           #'helm--advice-tramp-read-passwd)
            (advice-remove 'ange-ftp-get-passwd
                           #'helm--advice-ange-ftp-get-passwd))
          (ad-deactivate 'tramp-read-passwd)
          (ad-deactivate 'ange-ftp-get-passwd))
      (helm-log "helm-alive-p = %S" (setq helm-alive-p nil))
      (helm--remap-mouse-mode -1)       ; Reenable mouse bindings.
      (setq helm-alive-p nil)
      ;; Reset helm-pattern so that lambda's using it
      ;; before running helm will not start with its old value.
      (setq helm-pattern "")
      (and old--cua (cua-mode 1))
      (helm-log-save-maybe))))


;;; Helm resume
;;
;;
(defun helm-resume (arg)
  "Resurrect previously invoked `helm'.
Called with a prefix arg, allow choosing among all existing
helm buffers.  i.e choose among various helm sessions.
Called from lisp, you can specify a buffer-name as a string with ARG."
  (interactive "P")
  (let (any-buffer helm-full-frame cur-dir)
    (if arg
        (if (and (stringp arg) (bufferp (get-buffer arg)))
            (setq any-buffer arg)
          (setq any-buffer (helm-resume-select-buffer)))
      (setq any-buffer helm-last-buffer))
    (cl-assert any-buffer nil
               "helm-resume: No helm buffers found to resume")
    ;; Reset `cursor-type' to nil as it have been set to t
    ;; when quitting previous session.
    (with-current-buffer any-buffer (setq cursor-type nil))
    (setq helm-full-frame (buffer-local-value
                           'helm-full-frame (get-buffer any-buffer)))
    (setq helm-compiled-sources nil)
    (setq cur-dir (buffer-local-value
                   'default-directory (get-buffer any-buffer)))
    (setq helm-saved-selection nil
          helm-saved-action nil)
    (unless (buffer-live-p helm-current-buffer)
      ;; `helm-current-buffer' may have been killed.
      (setq helm-current-buffer (current-buffer)))
    ;; Restart with same `default-directory' value this session
    ;; was initially started with.
    (with-helm-default-directory cur-dir
        (helm
         :sources (buffer-local-value
                   'helm-sources (get-buffer any-buffer))
         :input (buffer-local-value 'helm-input-local (get-buffer any-buffer))
         :resume t
         :buffer any-buffer))))

(defun helm-resume-previous-session-after-quit (arg)
  "Resume previous helm session within running helm."
  (interactive "p")
  (if (and helm-alive-p (> (length helm-buffers) arg))
      (helm-run-after-exit `(lambda () (helm-resume (nth ,arg helm-buffers))))
    (message "No previous helm sessions to resume yet!")))

(defun helm-resume-list-buffers-after-quit ()
  "List resumable helm buffers within running helm."
  (interactive)
  (if (and helm-alive-p (> (length helm-buffers) 0))
      (helm-run-after-exit (lambda () (helm-resume t)))
    (message "No previous helm sessions to resume yet!")))

(defun helm-resume-p (any-resume)
  "Whether current helm session is resumed or not."
  (eq any-resume t))

(defun helm-resume-select-buffer ()
  "Select an `helm-buffer' in `helm-buffers' list to resume a helm session.
Return nil if no `helm-buffer' found."
  (when helm-buffers
    (or (helm :sources '(((name . "Resume helm buffer")
                          (candidates . helm-buffers)
                          (action . identity)))
              :resume 'noresume
              :buffer "*helm resume*")
        (keyboard-quit))))


;;;###autoload
(defun helm-other-buffer (any-sources any-buffer)
  "Simplified interface of `helm' with other `helm-buffer'.
Call `helm' with only ANY-SOURCES and ANY-BUFFER as args."
  (helm :sources any-sources :buffer any-buffer))

(defun helm-nest (&rest same-as-helm)
  "Allow calling `helm' whithin a running helm session.
Arguments SAME-AS-HELM are the same as `helm', which see."
  (with-helm-window
    (let ((orig-helm-current-buffer helm-current-buffer)
          (orig-helm-buffer helm-buffer)
          (orig-helm--prompt helm--prompt)
          (orig-helm--in-fuzzy helm--in-fuzzy)
          (orig-helm-last-frame-or-window-configuration
           helm-last-frame-or-window-configuration)
          (orig-one-window-p helm-onewindow-p))
      (unwind-protect
           (let (helm-current-position
                 helm-current-buffer
                 helm-pattern
                 (helm-buffer (or (cl-getf same-as-helm :buffer)
                                  (nth 5 same-as-helm)
                                  "*Helm*"))
                 helm-sources
                 helm-compiled-sources
                 (helm-full-frame t)
                 (enable-recursive-minibuffers t))
             (apply #'helm same-as-helm))
        (with-current-buffer orig-helm-buffer
          (setq helm-alive-p t) ; Nested session set this to nil on exit.
          (setq helm-buffer orig-helm-buffer)
          (setq helm--prompt orig-helm--prompt)
          (setq helm--in-fuzzy orig-helm--in-fuzzy)
          (helm-initialize-overlays helm-buffer)
          (unless (helm-empty-buffer-p) (helm-mark-current-line t))
          (setq helm-last-frame-or-window-configuration
                orig-helm-last-frame-or-window-configuration)
          (setq cursor-type nil)
          (setq helm-current-buffer orig-helm-current-buffer)
          (setq helm-onewindow-p orig-one-window-p)
          ;; Be sure advices, hooks, and local modes keep running.
          (if (fboundp 'advice-add)
              (progn
                (advice-add 'tramp-read-passwd
                            :around #'helm--advice-tramp-read-passwd)
                (advice-add 'ange-ftp-get-passwd
                            :around #'helm--advice-ange-ftp-get-passwd))
              (ad-activate 'tramp-read-passwd)
              (ad-activate 'ange-ftp-get-passwd))
          (when helm-prevent-escaping-from-minibuffer
            (helm--remap-mouse-mode 1))
          (unless (cl-loop for h in post-command-hook
                           thereis (memq h '(helm--maybe-update-keymap
                                             helm--update-header-line)))
            (add-hook 'post-command-hook 'helm--maybe-update-keymap)
            (add-hook 'post-command-hook 'helm--update-header-line))
          (helm-display-mode-line (helm-get-current-source)))))))


;;; Core: Accessors
;;
(defun helm-current-position (save-or-restore)
  "Restore or save current position in `helm-current-buffer'.
Argument SAVE-OR-RESTORE is one of save or restore."
  (cl-case save-or-restore
    (save
     (helm-log "Save position at %S" (cons (point) (window-start)))
     (setq helm-current-position (cons (point) (window-start))))
    (restore
     ;; Maybe `helm-current-buffer' have been deleted
     ;; during helm session so check if it is here
     ;; otherwise position in underlaying buffer will be lost.
     (when (get-buffer-window helm-current-buffer 'visible)
       (helm-log "Restore position at  %S in buffer %s"
                 helm-current-position
                 (buffer-name (current-buffer)))
       (goto-char (car helm-current-position))
       ;; Fix this position with the NOFORCE arg of `set-window-start'
       ;; otherwise, if there is some other buffer than `helm-current-buffer'
       ;; one, position will be lost.
       (set-window-start (selected-window) (cdr helm-current-position) t)))))


(defun helm-frame-or-window-configuration (save-or-restore)
  "Save or restore last frame or window configuration.
Possible value of SAVE-OR-RESTORE are 'save and 'restore.
window or frame configuration is saved/restored according to values of
`helm-save-configuration-functions'."
  (helm-log "helm-save-configuration-functions = %S"
            helm-save-configuration-functions)
  (let ((window-persistent-parameters (append '((no-other-window . t))
                                              window-persistent-parameters)))
    (cl-case save-or-restore
      (save    (setq helm-last-frame-or-window-configuration
                     (funcall (cdr helm-save-configuration-functions))))
      (restore (funcall (car helm-save-configuration-functions)
                        helm-last-frame-or-window-configuration)
               ;; Restore frame focus.
               ;; This is needed for minibuffer own-frame config
               ;; when recursive minibuffers are in use.
               ;; e.g M-: + helm-minibuffer-history.
               (let ((frame (if (minibufferp helm-current-buffer)
                                (selected-frame)
                              (last-nonminibuffer-frame))))
                 (select-frame-set-input-focus frame))))))

(defun helm-split-window-default-fn (window)
  (let (split-width-threshold)
    (if (and (fboundp 'window-in-direction)
             ;; Don't try to split when starting in a minibuffer
             ;; e.g M-: and try to use helm-show-kill-ring.
             (not (minibufferp helm-current-buffer)))
        (if (or (one-window-p t)
                helm-split-window-in-side-p)
            (split-window
             (selected-window) nil (if (eq helm-split-window-default-side 'other)
                                       'below helm-split-window-default-side))
          ;; If more than one window reuse one of them.
          (cl-case helm-split-window-default-side
            (left  (or (helm-window-in-direction 'left)
                       (helm-window-in-direction 'above)
                       (selected-window)))
            (above (or (helm-window-in-direction 'above)
                       (helm-window-in-direction 'left)
                       (selected-window)))
            (right (or (helm-window-in-direction 'right)
                       (helm-window-in-direction 'below)
                       (selected-window)))
            (below (or (helm-window-in-direction 'below)
                       (helm-window-in-direction 'right)
                       (selected-window)))
            (same  (selected-window))
            (other (other-window-for-scrolling))
            (t     (or (window-next-sibling) (selected-window)))))
      (split-window-sensibly window))))

(defun helm-window-in-direction (direction)
  "Same as `window-in-direction' but check if window is dedicated."
  (helm-aif (window-in-direction direction)
      (and (not (window-dedicated-p it)) it)))


;;; Display helm buffer
;;
;;
(defun helm-display-buffer (buffer)
  "Display BUFFER.
The function used to display `helm-buffer'."
  (let (pop-up-frames
        (split-window-preferred-function
         helm-split-window-preferred-function)
        (helm-split-window-default-side
         (if (and (not helm-full-frame)
                  helm-reuse-last-window-split-state)
             (cond ((eq helm-split-window-default-side 'same) 'same)
                   ((eq helm-split-window-default-side 'other) 'other)
                   (helm--window-side-state)
                   (t helm-split-window-default-side))
           helm-split-window-default-side)))
    (prog1
        (funcall (with-current-buffer buffer helm-display-function) buffer)
      (setq helm-onewindow-p (one-window-p t))
      ;; Don't allow other-window and friends switching out of minibuffer.
      (when helm-prevent-escaping-from-minibuffer
        (helm-prevent-switching-other-window)))))

(cl-defun helm-prevent-switching-other-window (&key (enabled t))
  "Allow setting `no-other-window' window parameter in all windows.
Arg ENABLE will be the value of the `no-other-window' window property."
  (walk-windows
   (lambda (w)
       (unless (window-dedicated-p w)
         (set-window-parameter w 'no-other-window enabled)))
   0))

(defun helm-default-display-buffer (buffer)
  "Default function to display `helm-buffer' BUFFER.
It uses `switch-to-buffer' or `display-buffer' depending of value
of `helm-full-frame' and/or `helm-split-window-default-side'."
  (if (or (buffer-local-value 'helm-full-frame (get-buffer buffer))
          (and (eq helm-split-window-default-side 'same)
               (one-window-p t)))
      (progn (delete-other-windows) (switch-to-buffer buffer))
    (when (and (or helm-always-two-windows helm-autoresize-mode
                   (and (not helm-split-window-in-side-p)
                        (eq (save-selected-window
                              (funcall helm-split-window-preferred-function
                                       (selected-window)))
                            (get-buffer-window helm-current-buffer))))
               (not (eq helm-split-window-default-side 'same))
               (not (minibufferp helm-current-buffer))
               (not helm-split-window-in-side-p))
      (delete-other-windows))
    (display-buffer buffer)))


;;; Core: initialize
;;
(defun helm-initialize (any-resume any-input any-default any-sources)
  "Start initialization of `helm' session.
For ANY-RESUME ANY-INPUT ANY-DEFAULT and ANY-SOURCES See `helm'."
  (helm-log "start initialization: any-resume=%S any-input=%S"
            any-resume any-input)
  (helm-frame-or-window-configuration 'save)
  (setq helm-sources (helm-normalize-sources any-sources))
  (setq helm--in-fuzzy
        (cl-loop for s in helm-sources
                 for matchfns = (helm-match-functions
                                 (if (symbolp s) (symbol-value s) s))
                 for searchfns = (helm-search-functions
                                  (if (symbolp s) (symbol-value s) s))
                 when (or (member 'helm-fuzzy-match matchfns)
                          (member 'helm-fuzzy-search searchfns))
                 return t))
  (helm-log "sources = %S" helm-sources)
  (helm-current-position 'save)
  (if (helm-resume-p any-resume)
      (helm-initialize-overlays (helm-buffer-get))
    (helm-initial-setup any-default))
  (setq helm-alive-p t)
  (unless (eq any-resume 'noresume)
    (helm-recent-push helm-buffer 'helm-buffers)
    (setq helm-last-buffer helm-buffer))
  (when any-input
    (setq helm-input any-input
          helm-pattern any-input)
    (helm--fuzzy-match-maybe-set-pattern))
  ;; If a `resume' attribute is present `helm-funcall-foreach'
  ;; will run its function.
  (when (helm-resume-p any-resume)
    (helm-funcall-foreach 'resume))
  (helm-log "end initialization"))

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
    (overlay-put helm-selection-overlay 'face 'helm-selection)
    (overlay-put helm-selection-overlay 'priority 1)))

(defun helm-restore-position-on-quit ()
  "Restore position in `helm-current-buffer' when quitting."
  (helm-current-position 'restore))

(defun helm-recent-push (elt list-var)
  "Add ELT to the value of LIST-VAR as most recently used value."
  (let ((m (member elt (symbol-value list-var))))
    (and m (set list-var (delq (car m) (symbol-value list-var))))
    (push elt (symbol-value list-var))))

(defun helm--current-buffer ()
  "[internal] Return `current-buffer' BEFORE `helm-buffer' is initialized.
Note that this will return the minibuffer in use after helm have started,
so to get the buffer where helm started while in a helm session,
use `helm-current-buffer'.
It is intended to use this only in `helm-initial-setup'."
  (if (minibuffer-window-active-p (minibuffer-window))
      ;; If minibuffer is active be sure to use it's buffer
      ;; as `helm-current-buffer', this allow to use helm
      ;; from an already active minibuffer (M-: etc...)
      (window-buffer (active-minibuffer-window))
    ;; Fix Issue #456
    ;; Use this instead of `current-buffer' to ensure
    ;; helm session started in helm-mode from a completing-read
    ;; Use really the buffer where we started and not the one
    ;; where the completing-read is wrapped. i.e
    ;; (with-current-buffer SOME-OTHER-BUFFER (completing-read [...])
    (window-buffer (with-selected-window (minibuffer-window)
                     (minibuffer-selected-window)))))

(defun helm--run-init-hooks (hook)
  "Run after and before init hooks local to source.
See :after-init-hook and :before-init-hook in `helm-source'."
  (cl-loop with sname = (cl-ecase hook
                          (before-init-hook "h-before-init-hook")
                          (after-init-hook "h-after-init-hook"))
           with h = (cl-gensym sname)
           for s in (helm-get-sources)
           for hv = (assoc-default hook s)
           if (and hv (not (symbolp hv)))
           do (set h hv)
           and do (helm-log-run-hook h)
           else do (helm-log-run-hook hv)))

(defun helm-initial-setup (any-default)
  "Initialize helm settings and set up the helm buffer."
  ;; Run global hook.
  (helm-log-run-hook 'helm-before-initialize-hook)
  ;; Run local source hook.
  (helm--run-init-hooks 'before-init-hook)
  ;; For initialization of helm locals vars that need
  ;; a value from current buffer, it is here.
  (helm-set-local-variable 'current-input-method current-input-method)
  (setq helm-current-prefix-arg nil
        helm-saved-action nil
        helm-saved-selection nil
        helm-suspend-update-flag nil
        helm-current-buffer (helm--current-buffer)
        helm-buffer-file-name buffer-file-name
        helm-issued-errors nil
        helm-compiled-sources nil
        helm-saved-current-source nil)
  (unless (and (or helm-split-window-state
                   helm--window-side-state)
               helm-reuse-last-window-split-state)
    (setq helm-split-window-state
          (if (or (null split-width-threshold)
                  (and (integerp split-width-threshold)
                       (>= split-width-threshold (+ (frame-width) 4))))
              'vertical 'horizontal))
    (setq helm--window-side-state
          (or helm-split-window-default-side 'below)))
  ;; Call the init function for sources where appropriate
  (helm-funcall-foreach
   'init (and helm-source-filter
              (cl-remove-if-not (lambda (s)
                                    (member (assoc-default 'name s)
                                            helm-source-filter))
                                (helm-get-sources))))
  (setq helm-pattern (or (and helm--maybe-use-default-as-input
                              (or (if (listp any-default)
                                      (car any-default) any-default)
                                  (with-helm-current-buffer
                                    (thing-at-point 'symbol))))
                         ""))
  (setq helm-input "")
  (clrhash helm-candidate-cache)
  (helm-create-helm-buffer)
  (helm-clear-visible-mark)
  ;; Run global hook.
  (helm-log-run-hook 'helm-after-initialize-hook)
  ;; Run local source hook.
  (helm--run-init-hooks 'after-init-hook))

(defun helm-create-helm-buffer ()
  "Create and setup `helm-buffer'."
  (let ((root-dir default-directory))
    (with-current-buffer (get-buffer-create helm-buffer)
      (helm-log "kill local variables: %S" (buffer-local-variables))
      (kill-all-local-variables)
      (set (make-local-variable 'inhibit-read-only) t)
      (buffer-disable-undo)
      (erase-buffer)
      (set (make-local-variable 'helm-map) helm-map)
      (make-local-variable 'helm-sources)
      (set (make-local-variable 'helm-follow-mode) nil)
      (set (make-local-variable 'helm-display-function) helm-display-function)
      (set (make-local-variable 'helm-selection-point) nil)
      (set (make-local-variable 'scroll-margin)
           (if helm-display-source-at-screen-top
               0 helm-completion-window-scroll-margin))
      (set (make-local-variable 'default-directory) root-dir)
      (set (make-local-variable 'helm-marked-candidates) nil)
      (helm-initialize-persistent-action)
      (helm-log "helm-display-function = %S" helm-display-function)
      (helm-log "helm--local-variables = %S" helm--local-variables)
      (cl-loop for (var . val) in helm--local-variables
               do (set (make-local-variable var) val)
               finally (setq helm--local-variables nil))
      (setq truncate-lines helm-truncate-lines) ; already local.
      (setq cursor-type nil)
      (setq mode-name "Helm"))
    (helm-initialize-overlays helm-buffer)
    (get-buffer helm-buffer)))

(define-minor-mode helm--minor-mode
    "[INTERNAL] Enable keymap in helm minibuffer.
This mode have no effect when run outside of helm context.
Please don't use it.

\\{helm-map}"
  :group 'helm
  :keymap (and helm-alive-p helm-map)
  (unless helm-alive-p (setq helm--minor-mode nil)))

(defun helm--reset-default-pattern ()
  (setq helm-pattern "")
  (setq helm--maybe-use-default-as-input nil))

(defun helm-read-pattern-maybe (any-prompt any-input
                                any-preselect any-resume any-keymap
                                any-default any-history)
  "Read pattern with prompt ANY-PROMPT and initial input ANY-INPUT.
For ANY-PRESELECT ANY-RESUME ANY-KEYMAP ANY-DEFAULT ANY-HISTORY, See `helm'."
  (if (and (helm-resume-p any-resume)
           ;; When no source, helm-buffer is empty
           ;; or contain non--candidate lines (e.g grep exit status)
           (helm-get-current-source))
      (helm-mark-current-line t)
      (helm-update any-preselect))
  (with-current-buffer (helm-buffer-get)
    (let* ((src        (helm-get-current-source))
           (src-keymap (assoc-default 'keymap src))
           (hist       (or (and any-history (symbolp any-history) any-history)
                           ;; Needed for resuming.
                           (assoc-default 'history src)))
           (timer nil)
           blink-matching-paren
           (resize-mini-windows (and (null helm-echo-input-in-header-line)
                                     resize-mini-windows))
           (first-src (car helm-sources))
           (first-src-val (if (symbolp first-src)
                              (symbol-value first-src)
                              first-src))
           (source-process-p (or (assq 'candidates-process src)
                                 (assq 'candidates-process first-src-val)))
           (source-delayed-p (or (assq 'delayed src)
                                 (assq 'delayed first-src-val))))
      (helm-log "helm-get-candidate-number => %S"
                (helm-get-candidate-number))
      (helm-log "helm-execute-action-at-once-if-one = %S"
                helm-execute-action-at-once-if-one)
      (helm-log "helm-quit-if-no-candidate = %S" helm-quit-if-no-candidate)
      ;; If source is delayed `helm-execute-action-at-once-if-one'
      ;; and `helm-quit-if-no-candidate' are handled after update finish.
      (when source-delayed-p
        ;; Note that we quickly add the hook now when `helm-update'
        ;; is already started, but because source is delayed the hook
        ;; should have the time to be passed !!!
        ;; the hook will remove itself once done.
        (with-helm-after-update-hook (helm-exit-or-quit-maybe)))
      ;; Reset `helm-pattern' for non--delayed sources and update
      ;; display if no result found with precedent value of `helm-pattern'
      ;; unless `helm-quit-if-no-candidate' is non--nil, in this case
      ;; Don't force update with an empty pattern.
      ;; Reset also `helm--maybe-use-default-as-input' as this checking
      ;; happen only on startup.
      (when helm--maybe-use-default-as-input
        ;; Store value of `default' temporarily here waiting next update
        ;; to allow actions like helm-moccur-action matching pattern
        ;; at the place it jump to.
        (setq helm-input helm-pattern)
        (if (or source-delayed-p source-process-p)
            ;; Reset pattern to next update.
            (with-helm-after-update-hook
              (helm--reset-default-pattern))
            ;; Reset pattern right now.
            (helm--reset-default-pattern))
        ;; Ensure force-update when no candidates
        ;; when we start with an empty pattern.
        (and (helm-empty-buffer-p)
             (null helm-quit-if-no-candidate)
             (helm-force-update)))
      ;; Handle `helm-execute-action-at-once-if-one' and
      ;; `helm-quit-if-no-candidate' now only for not--delayed sources.
      (cond ((and (if (functionp helm-execute-action-at-once-if-one)
                      (funcall helm-execute-action-at-once-if-one)
                      helm-execute-action-at-once-if-one)
                  (not source-delayed-p)
                  (= (helm-get-candidate-number) 1))
             (ignore))              ; Don't enter the minibuffer loop.
            ((and helm-quit-if-no-candidate
                  (not source-delayed-p)
                  (= (helm-get-candidate-number) 0))
             (setq helm-quit t)
             (and (functionp helm-quit-if-no-candidate)
                  (funcall helm-quit-if-no-candidate)))
            (t              ; Enter now minibuffer and wait for input.
             (let ((tap (or any-default
                            (with-helm-current-buffer
                              (thing-at-point 'symbol)))))
               (unwind-protect
                    (minibuffer-with-setup-hook
                        (lambda ()
                          ;; Start minor-mode with global value of helm-map.
                          (helm--minor-mode 1)
                          ;; Now overhide the global value of `helm-map' with
                          ;; the local one which is in this order:
                          ;; - The keymap of current source.
                          ;; - The value passed in ANY-KEYMAP
                          ;;   which will become buffer local.
                          ;; - Or fallback to the global value of helm-map.
                          (helm--maybe-update-keymap
                           (or src-keymap any-keymap helm-map))
                          (helm-log-run-hook 'helm-minibuffer-set-up-hook)
                          (setq timer
                                (run-with-idle-timer
                                 (max helm-input-idle-delay 0.001) 'repeat
                                 (lambda ()
                                   ;; Stop updating in persistent action
                                   ;; or when `helm-suspend-update-flag'
                                   ;; is non--nil.
                                   (unless (or helm-in-persistent-action
                                               helm-suspend-update-flag)
                                     (save-selected-window
                                       (helm-check-minibuffer-input)
                                       (helm-print-error-messages))))))
                          (helm--update-header-line)) ; minibuffer has already been filled here
                      (read-from-minibuffer (or any-prompt "pattern: ")
                                            any-input helm-map
                                            nil hist tap
                                            helm-inherit-input-method))
                 (when timer (cancel-timer timer) (setq timer nil)))))))))

(defun helm-exit-or-quit-maybe ()
  "Exit and run default action if only one candidate, quit if no candidates.
This function is handling `helm-execute-action-at-once-if-one' and
`helm-quit-if-no-candidate' in delayed sources."
  (with-helm-window
    (cond ((and (if (functionp helm-execute-action-at-once-if-one)
                    (funcall helm-execute-action-at-once-if-one)
                    helm-execute-action-at-once-if-one)
                (= (helm-get-candidate-number) 1))
           (helm-exit-minibuffer))
          ((and helm-quit-if-no-candidate
                (= (helm-get-candidate-number) 0))
           (setq helm-quit t)
           (and (functionp helm-quit-if-no-candidate)
                (funcall helm-quit-if-no-candidate))
           (keyboard-quit)))))

(defun helm-toggle-suspend-update ()
  "Enable or disable update of display in helm.
This can be useful for e.g writing quietly a complex regexp."
  (interactive)
  (when (setq helm-suspend-update-flag (not helm-suspend-update-flag))
    (helm-kill-async-processes)
    (setq helm-pattern ""))
  (message (if helm-suspend-update-flag
               "Helm update suspended!"
             "Helm update reenabled!")))

(defadvice tramp-read-passwd (around disable-helm-update)
  ;; Suspend update when prompting for a tramp password.
  (setq helm-suspend-update-flag t)
  (setq overriding-terminal-local-map nil)
  (setq helm--reading-passwd-or-string t)
  (let (stimers)
    (unwind-protect
         (progn
           (setq stimers (with-timeout-suspend))
           ad-do-it)
      (with-timeout-unsuspend stimers)
      (setq helm--reading-passwd-or-string nil)
      (setq helm-suspend-update-flag nil))))

(defun helm--advice-tramp-read-passwd (old--fn &rest args)
  ;; Suspend update when prompting for a tramp password.
  (setq helm-suspend-update-flag t)
  (setq overriding-terminal-local-map nil)
  (setq helm--reading-passwd-or-string t)
  (unwind-protect
       ;; No need to suspend timer in emacs-24.4
       ;; it is fixed upstream.
       (apply old--fn args)
    (setq helm--reading-passwd-or-string nil)
    (setq helm-suspend-update-flag nil)))

(defun helm--advice-ange-ftp-get-passwd (old--fn &rest args)
  ;; Suspend update when prompting for a ange password.
  (setq helm-suspend-update-flag t)
  (setq overriding-terminal-local-map nil)
  (setq helm--reading-passwd-or-string t)
  (unwind-protect
       (apply old--fn args)
    (setq helm--reading-passwd-or-string nil)
    (setq helm-suspend-update-flag nil)))

(defadvice ange-ftp-get-passwd (around disable-helm-update)
  ;; Suspend update when prompting for a ange password.
  (setq helm-suspend-update-flag t)
  (setq overriding-terminal-local-map nil)
  (setq helm--reading-passwd-or-string t)
  (unwind-protect
       ad-do-it
    (setq helm--reading-passwd-or-string nil)
    (setq helm-suspend-update-flag nil)))

(defun helm--maybe-update-keymap (&optional map)
  "Handle differents keymaps in multiples sources.

It will override `helm-map' with the local map of current source.
If no map is found in current source do nothing (keep previous map)."
  (with-helm-buffer
    (helm-aif (or map (assoc-default 'keymap (helm-get-current-source)))
        ;; We used a timer in the past to leave
        ;; enough time to helm to setup its keymap
        ;; when changing source from a recursive minibuffer.
        ;; e.g C-x C-f M-y C-g
        ;; => *find-files have now the bindings of *kill-ring.
        ;; It is no more true now we are using `minor-mode-overriding-map-alist'
        ;; and `helm--minor-mode' thus it fix issue #1076 for emacs-24.3
        ;; where concurrent timers are not supported.
        ;; i.e update keymap+check input.
        (with-current-buffer (window-buffer (minibuffer-window))
          (setq minor-mode-overriding-map-alist `((helm--minor-mode . ,it)))))))

;;; Prevent loosing focus when using mouse.
;;
(defvar helm--remap-mouse-mode-map
  (let ((map (make-sparse-keymap)))
    (cl-loop for k in '([mouse-1] [mouse-2] [mouse-3]
                        [down-mouse-1] [down-mouse-2] [down-mouse-3]
                        [drag-mouse-1] [drag-mouse-2] [drag-mouse-3]
                        [double-mouse-1] [double-mouse-2] [double-mouse-3]
                        [triple-mouse-1] [triple-mouse-2] [triple-mouse-3])
             do (define-key map k 'undefined))
    map))

(define-minor-mode helm--remap-mouse-mode
    "[INTERNAL] Prevent escaping helm minibuffer with mouse clicks.
Do nothing when used outside of helm context.

WARNING: Do not use this mode yourself, it is internal to helm."
  :group 'helm
  :global t
  :keymap helm--remap-mouse-mode-map
  (unless helm-alive-p
    (setq helm--remap-mouse-mode-map nil)))

;; Core: clean up

(defun helm-cleanup ()
  "Clean up the mess when helm exit or quit."
  (helm-log "start cleanup")
  (with-current-buffer helm-buffer
    ;; bury-buffer from this window.
    (bury-buffer) ;[1]
    (remove-hook 'post-command-hook 'helm--maybe-update-keymap)
    (remove-hook 'post-command-hook 'helm--update-header-line)
    ;; Be sure we call this from helm-buffer.
    (helm-funcall-foreach 'cleanup))
  (helm-kill-async-processes)
  ;; Remove the temporary hooks added
  ;; by `with-helm-temp-hook' that
  ;; may not have been consumed.
  (when helm--temp-hooks
    (cl-loop for (fn . hook) in helm--temp-hooks
             do (set hook (delete fn (symbol-value hook)))))
  ;; When running helm from a dedicated frame
  ;; with no minibuffer, helm will run in the main frame
  ;; which have a minibuffer, so be sure to disable
  ;; the `no-other-window' prop there.
  (helm-prevent-switching-other-window :enabled nil)
  (helm-log-run-hook 'helm-cleanup-hook)
  (helm-frame-or-window-configuration 'restore)
  ;; [1] now bury-buffer from underlying windows otherwise,
  ;; if this window is killed the underlying buffer will
  ;; be a helm buffer.
  (replace-buffer-in-windows helm-buffer)
  (setq helm-alive-p nil)
  (setq helm-debug nil)
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


;;; Core: input handling
;;
;;
(defun helm-check-minibuffer-input ()
  "Check minibuffer content."
  (with-helm-quittable
    (with-selected-window (or (active-minibuffer-window)
                              (minibuffer-window))
      (helm-check-new-input (minibuffer-contents)))))

(defun helm-check-new-input (input)
  "Check INPUT string and update the helm buffer if necessary."
  ;; First time minibuffer is entered
  ;; we check value of `helm-pattern' that have been set
  ;; in `helm-initial-setup' when `helm--maybe-use-default-as-input'
  ;; is non--nil.  After this initial check, reset
  ;; `helm--maybe-use-default-as-input' and ignore this.
  ;; This happen only when source is `delayed'.
  (when helm--maybe-use-default-as-input ; nil when non--delayed.
    (setq input helm-pattern))
  (unless (equal input helm-pattern)
    (setq helm-pattern input)
    (unless (helm-action-window)
      (setq helm-input helm-pattern))
    (helm-log "helm-pattern = %S" helm-pattern)
    (helm-log "helm-input = %S" helm-input)
    (setq helm--in-update t)
    (helm-update)))

(defun helm--reset-update-flag ()
  (run-with-idle-timer
   helm-exit-idle-delay nil
   (lambda () (setq helm--in-update nil))))

(add-hook 'helm-after-update-hook #'helm--reset-update-flag)


;;; Core: source compiler
;;
;;
(defun helm-compile-sources (sources funcs)
  "Compile SOURCES with FUNCS.
See `helm-compile-source-functions'.
Helm plug-ins are realized by this function."
  (mapcar
   (lambda (source)
     (cl-loop with src = (if (listp source) source (symbol-value source))
              for noplug = (assoc 'dont-plug src)
              for f in funcs
              unless (and noplug (memq f (cdr noplug)))
              do (setq src (funcall f src))
              finally (cl-return src)))
   sources))


;; Core: all candidates

(defun helm-get-candidates (source)
  "Retrieve and return the list of candidates from SOURCE."
  (let* (inhibit-quit
         (candidate-fn (assoc-default 'candidates source))
         (candidate-proc (assoc-default 'candidates-process source))
         (type-error (lambda ()
                       (error
                        "`%s' must either be a function, a variable or a list"
                        (or candidate-fn candidate-proc))))
         (candidates (condition-case-unless-debug err
                         ;; Process candidates-(process) function
                         ;; It may return a process or a list of candidates.
                         (if candidate-proc
                             ;; Calling `helm-interpret-value' with no
                             ;; SOURCE arg force the use of `funcall'
                             ;; and not `helm-funcall-with-source'.
                             (helm-interpret-value candidate-proc)
                           (helm-interpret-value candidate-fn source))
                       (error (helm-log "Error: %S" err) nil))))
    (when (and (processp candidates) (not candidate-proc))
      (warn "Candidates function `%s' should be called in a `candidates-process' attribute"
            candidate-fn))
    (cond ((processp candidates)
           ;; Candidates will be filtered later in process filter.
           candidates)
          ((or (null candidates)
               ;; Can happen when the output of a process
               ;; is empty, and the candidates function call
               ;; something like (split-string (buffer-string) "\n")
               ;; which result in a list of one empty string (Issue #938).
               ;; e.g (completing-read "test: " '(""))
               (equal candidates '("")))
           nil)
          ((listp candidates)
           ;; Transform candidates with `candidate-transformer' functions if
           ;; some, otherwise return candidates.
           (helm-transform-candidates candidates source))
          (t (funcall type-error)))))

(defmacro helm-while-no-input (&rest body)
  "Same as `while-no-input' but without testing with `input-pending-p'."
  (declare (debug t) (indent 0))
  (let ((catch-sym (make-symbol "input"))
        inhibit-quit)
    `(with-local-quit
       (catch ',catch-sym
	 (let ((throw-on-input ',catch-sym))
           ,@body)))))

(defun helm-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is not yet a cached value."
  (let* ((name (assoc-default 'name source))
         (candidate-cache (gethash name helm-candidate-cache)))
    (helm-aif candidate-cache
        (prog1 it (helm-log "Use cached candidates"))
      (helm-log "No cached candidates, calculate candidates")
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
               (puthash name candidates helm-candidate-cache)))
        candidates))))


;;; Core: candidate transformers

(defun helm-process-candidate-transformer (candidates source)
  "Execute `candidate-transformer' function(s) on CANDIDATES in SOURCE."
  (helm-aif (assoc-default 'candidate-transformer source)
      (helm-composed-funcall-with-source source it candidates)
    candidates))

(defun helm-process-filtered-candidate-transformer (candidates source)
  "Execute `filtered-candidate-transformer' function(s) on CANDIDATES in SOURCE."
  (helm-aif (assoc-default 'filtered-candidate-transformer source)
      (helm-composed-funcall-with-source source it candidates source)
    candidates))

(defmacro helm--maybe-process-filter-one-by-one-candidate (candidate source)
  "Execute `filter-one-by-one' function(s) on CANDIDATE in SOURCE."
  `(helm-aif (assoc-default 'filter-one-by-one ,source)
       (if (and (listp it)
                (not (functionp it))) ;; Don't treat lambda's as list.
           (cl-loop for f in it
                 do (setq ,candidate (funcall f ,candidate))
                 finally return ,candidate)
         (setq ,candidate (funcall it ,candidate)))
     ,candidate))

(defun helm--initialize-one-by-one-candidates (candidates source)
  "Process the CANDIDATES with the `filter-one-by-one' function in SOURCE.
Return CANDIDATES when pattern is empty."
  (helm-aif (and (string= helm-pattern "")
                 (assoc-default 'filter-one-by-one source))
      (cl-loop for cand in candidates collect
               (helm--maybe-process-filter-one-by-one-candidate cand source))
    candidates))

(defun helm-process-filtered-candidate-transformer-maybe
    (candidates source process-p)
  "Execute `filtered-candidate-transformer' function(s) on CANDIDATES in SOURCE.
When PROCESS-P is non-nil execute `filtered-candidate-transformer'
functions if some, otherwise return CANDIDATES."
  (if process-p
      ;; When no filter return CANDIDATES unmodified.
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
When PROCESS-P is non-nil execute the `filtered-candidate-transformer' functions
otherwise only the `candidate-transformer' functions are processed.
When attribute `real-to-display' is present, execute its function on all maybe
filtered CANDIDATES."
  (helm-process-real-to-display
   (helm-process-filtered-candidate-transformer-maybe
    (helm-process-candidate-transformer
     (helm--initialize-one-by-one-candidates candidates source) source)
    source process-p)
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

(defun helm-candidate-get-display (candidate)
  "Get searched display part from CANDIDATE.
CANDIDATE is a string, a symbol, or \(DISPLAY . REAL\) cons cell."
  (cond ((car-safe candidate))
        ((symbolp candidate)
         (symbol-name candidate))
        ((numberp candidate)
         (number-to-string candidate))
        (t candidate)))

(defun helm-process-pattern-transformer (pattern source)
  "Execute pattern-transformer attribute PATTERN function in SOURCE."
  (helm-aif (assoc-default 'pattern-transformer source)
      (helm-composed-funcall-with-source source it pattern)
    pattern))

(defun helm-default-match-function (candidate)
  "Check if `helm-pattern' match CANDIDATE.
Default function to match candidates according to `helm-pattern'."
  (string-match helm-pattern candidate))


;;; Fuzzy matching
;;
;;
(defvar helm--fuzzy-regexp-cache (make-hash-table :test 'eq))
(defun helm--fuzzy-match-maybe-set-pattern ()
  ;; Computing helm-pattern with helm--mapconcat-pattern
  ;; is costly, so cache it once time for all and reuse it
  ;; until pattern change.
  (when helm--in-fuzzy
    (let ((fun (if (string-match "\\`\\^" helm-pattern)
                   #'identity
                   #'helm--mapconcat-pattern)))
      (clrhash helm--fuzzy-regexp-cache)
      ;; FIXME: Splitted part are not handled here,
      ;; I must compute them in `helm-search-match-part'
      ;; when negation and in-buffer are used.
      (if (string-match "\\`!" helm-pattern)
          (puthash 'helm-pattern
                   (if (> (length helm-pattern) 1)
                       (list (funcall fun (substring helm-pattern 1 2))
                             (funcall fun (substring helm-pattern 1)))
                       '("" ""))
                   helm--fuzzy-regexp-cache)
          (puthash 'helm-pattern
                   (if (> (length helm-pattern) 0)
                       (list (funcall fun (substring helm-pattern 0 1))
                             (funcall fun helm-pattern))
                       '("" ""))
                   helm--fuzzy-regexp-cache)))))

(defun helm-fuzzy-match (candidate)
  "Check if `helm-pattern' fuzzy match CANDIDATE.
This function is used with sources build with `helm-source-sync'."
  (unless (string-match " " helm-pattern)
    ;; When pattern have one or more spaces, let
    ;; multi-match doing the job with no fuzzy matching.[1]
    (let ((regexp (cadr (gethash 'helm-pattern helm--fuzzy-regexp-cache))))
      (if (string-match "\\`!" helm-pattern)
          (not (string-match regexp candidate))
          (string-match regexp candidate)))))

(defun helm-fuzzy-search (pattern)
  "Same as `helm-fuzzy-match' but for sources build with `helm-source-in-buffer'."
  (unless (string-match " " helm-pattern)
    ;; Same as in `helm-fuzzy-match' ref[1].
    (let* ((regexps (gethash 'helm-pattern helm--fuzzy-regexp-cache))
           (partial-regexp (car regexps))
           (regexp (cadr regexps)))
      (if (string-match "\\`!" pattern)
          ;; Don't try to search here, just return
          ;; the position of line and go ahead,
          ;; letting `helm-search-match-part' checking if
          ;; pattern match against this line.
          (prog1 (list (point-at-bol) (point-at-eol))
            (forward-line 1))
          ;; We could use here directly `re-search-forward'
          ;; on the regexp produced by `helm--mapconcat-pattern',
          ;; but it is very slow because emacs have to do an incredible
          ;; amount of loops to match e.g "[^f]*o[^o]..." in the whole buffer,
          ;; more the regexp is long more the amount of loops grow.
          ;; (Probably leading to a max-lisp-eval-depth error if both
          ;; regexp and buffer are too big)
          ;; So just search the first bit of pattern e.g "[^f]*f", and
          ;; then search the corresponding line with the whole regexp,
          ;; which increase dramatically the speed of the search.
          (cl-loop while (re-search-forward partial-regexp nil t)
                   for bol = (point-at-bol)
                   for eol = (point-at-eol)
                   if (progn (goto-char bol)
                             (re-search-forward regexp eol t))
                   do (goto-char eol) and return t
                   else do (goto-char eol)
                   finally return nil)))))

(defun helm-score-candidate-for-pattern (candidate pattern)
  "Give a score to CANDIDATE according to PATTERN.
Score is calculated against number of contiguous matches found with PATTERN.
If PATTERN is fully matched in CANDIDATE a maximal score (100) is given.
A bonus of one point is given when PATTERN prefix match CANDIDATE.
Contiguous matches have a coefficient of 2."
  (let* ((cand (if (stringp candidate)
                   candidate (helm-stringify candidate)))
         (pat-lookup (helm--collect-pairs-in-string pattern))
         (str-lookup (helm--collect-pairs-in-string cand))
         (bonus (if (equal (car pat-lookup) (car str-lookup)) 1 0))
         (bonus1 (and (string-match (concat "\\<" (regexp-quote pattern) "\\>")
                                    cand)
                      100)))
    (+ bonus (or bonus1
                 ;; Give a coefficient of 2 for contiguous matches.
                 ;; That's mean that "wiaaaki" will not take precedence
                 ;; on "aaawiki" when matching on "wiki" even if "wiaaaki"
                 ;; starts by "wi".
                 (* (length (cl-nintersection
                             pat-lookup str-lookup :test 'equal))
                    2)))))

(defun helm-fuzzy-matching-default-sort-fn (candidates _source &optional use-real)
  "The transformer for sorting candidates in fuzzy matching.
It is sorting on the display part of by default.

Sort CANDIDATES according to their score calculated by
`helm-score-candidate-for-pattern'.  When two candidates have the
same score sort is made by length.  Set USE-REAL to non-nil to
sort on the real part."
  (if (string= helm-pattern "")
      candidates
    (let ((table-scr (make-hash-table :test 'equal)))
      (sort candidates
            (lambda (s1 s2)
              ;; Score and measure the length on real or display part of candidate
              ;; according to `use-real'.
              (let* ((real-or-disp-fn (if use-real #'cdr #'car))
                     (cand1 (if (consp s1)
                                (funcall real-or-disp-fn s1)
                              s1))
                     (cand2 (if (consp s2)
                                (funcall real-or-disp-fn s2)
                              s2))
                     (data1 (or (gethash cand1 table-scr)
                                (puthash cand1
                                         (list (helm-score-candidate-for-pattern
                                                cand1 helm-pattern)
                                               (length (helm-stringify cand1)))
                                         table-scr)))
                     (data2 (or (gethash cand2 table-scr)
                                (puthash cand2
                                         (list (helm-score-candidate-for-pattern
                                                cand2 helm-pattern)
                                               (length (helm-stringify cand2)))
                                         table-scr)))
                     (len1 (cadr data1))
                     (len2 (cadr data2))
                     (scr1 (car data1))
                     (scr2 (car data2)))
                (cond ((= scr1 scr2)
                       (< len1 len2))
                      ((> scr1 scr2)))))))))

(defun helm-fuzzy-default-highlight-match (candidate)
  "The default function to highlight matches in fuzzy matching.
It is meant to use with `filter-one-by-one' slot."
  (let* ((pair (and (consp candidate) candidate))
         (display (helm-stringify (if pair (car pair) candidate)))
         (real (cdr pair))
         (regex (helm-aif (and helm-migemo-mode
                               (assoc helm-pattern
                                      helm-mm--previous-migemo-info))
                    (cdr it)
                  helm-pattern))
         ;; FIXME This is called at each turn, cache it to optimize.
         (mp (helm-aif (helm-attr 'match-part (helm-get-current-source))
                 (funcall it display))))
    (with-temp-buffer
      (insert (propertize display 'read-only nil)) ; Fix (#1176)
      (goto-char (point-min))
      (when mp
        ;; FIXME the first part of display may contain an occurence of mp.
        ;; e.g "helm-adaptive.el:27:(defgroup helm-adapt" 
        (search-forward mp nil t)
        (goto-char (match-beginning 0)))
      (if (re-search-forward regex (and mp (+ (point) (length mp))) t)
          (add-text-properties
           (match-beginning 0) (match-end 0) '(face helm-match))
          (cl-loop with multi-match
                   with patterns = (if (string-match-p " " helm-pattern)
                                       (prog1 (split-string helm-pattern)
                                         (setq multi-match t))
                                       (split-string helm-pattern "" t))
                   for p in patterns
                   for re = (or (and helm-migemo-mode
                                     (assoc-default
                                      p helm-mm--previous-migemo-info))
                                p)
                   do
                   (when (re-search-forward re nil t)
                     (add-text-properties
                      (match-beginning 0) (match-end 0)
                      '(face helm-match)))
                   (when multi-match (goto-char (point-min)))))
      (setq display (buffer-string)))
    (if real (cons display real) display)))

(defun helm-fuzzy-highlight-matches (candidates _source)
  "The filtered-candidate-transformer function to highlight matches in fuzzy.
See `helm-fuzzy-default-highlight-match'."
  (cl-loop for c in candidates
           collect (funcall helm-fuzzy-matching-highlight-fn c)))

(defun helm-match-functions (source)
  (let ((matchfns (or (assoc-default 'match source)
                      (assoc-default 'match-strict source)
                      #'helm-default-match-function)))
    (if (and (listp matchfns) (not (functionp matchfns)))
        matchfns (list matchfns))))

(defun helm-search-functions (source)
  (let ((searchfns (assoc-default 'search source)))
    (if (and (listp searchfns) (not (functionp searchfns)))
        searchfns (list searchfns))))

(defmacro helm--accumulate-candidates (candidate newmatches
                                       hash item-count limit source)
  "Add CAND into NEWMATCHES and use HASH to uniq NEWMATCHES.
Argument ITEM-COUNT count the matches.
if ITEM-COUNT reaches LIMIT, exit from inner loop."
  `(unless (gethash ,candidate ,hash)
     (unless (assq 'allow-dups ,source)
       (puthash ,candidate t ,hash))
     (helm--maybe-process-filter-one-by-one-candidate ,candidate ,source)
     (push ,candidate ,newmatches)
     (cl-incf ,item-count)
     (when (= ,item-count ,limit) (cl-return))))

(defun helm-take-first-elements (seq n)
  "Return the N first element of SEQ if SEQ is longer than N.
It is used to narrow down list of candidates to `helm-candidate-number-limit'."
  (if (> (length seq) n) (cl-subseq seq 0 n) seq))

(cl-defun helm-set-case-fold-search (&optional (pattern helm-pattern))
  "Used to set the value of `case-fold-search' in helm.
Return t or nil depending of value of `helm-case-fold-search'
and `helm-pattern'."
  (let ((helm-case-fold-search
         (helm-aif (assq 'case-fold-search (helm-get-current-source))
             (cdr it)
           helm-case-fold-search))
        ;; Only parse basename for filenames
        ;; to avoid setting case sensitivity
        ;; when expanded directories contains upcase
        ;; characters.
        (bn-or-pattern (if (string-match "[~/]*" pattern)
                           ;; `helm-basename' is not available yet.
                           (file-name-nondirectory
                            (directory-file-name pattern))
                         pattern)))
    (helm-set-case-fold-search-1 bn-or-pattern)))

(defun helm-set-case-fold-search-1 (pattern)
  (cl-case helm-case-fold-search
    (smart (let ((case-fold-search nil))
             (if (string-match "[[:upper:]]" pattern) nil t)))
    (t helm-case-fold-search)))

(defun helm-match-from-candidates (cands matchfns match-part-fn limit source)
  (let (matches)
    (condition-case-unless-debug err
        (let ((item-count 0)
              (case-fold-search (helm-set-case-fold-search)))
          (clrhash helm-match-hash)
          (cl-dolist (match matchfns)
            (when (< item-count limit)
              (let (newmatches)
                (cl-dolist (candidate cands)
                  (unless (gethash candidate helm-match-hash)
                    (let ((target (helm-candidate-get-display candidate)))
                      (when (funcall match
                                     (if match-part-fn
                                         (funcall match-part-fn target) target))
                        (helm--accumulate-candidates
                         candidate newmatches
                         helm-match-hash item-count limit source)))))
                ;; filter-one-by-one may return nil candidates, so delq them if some.
                (setq matches (nconc matches (nreverse (delq nil newmatches))))))))
      (error (unless (eq (car err) 'invalid-regexp) ; Always ignore regexps errors.
               (helm-log-error "helm-match-from-candidates in source `%s': %s %s"
                               (assoc-default 'name source) (car err) (cdr err)))
             (setq matches nil)))
    matches))

(defun helm-compute-matches (source)
  "Start computing candidates in SOURCE."
  (save-current-buffer
    (let ((matchfns (helm-match-functions source))
          (matchpartfn (assoc-default 'match-part source))
          (helm-source-name (assoc-default 'name source))
          (helm-current-source source)
          (limit (helm-candidate-number-limit source))
          (helm-pattern (helm-process-pattern-transformer
                         helm-pattern source)))
      (helm--fuzzy-match-maybe-set-pattern)
      ;; If source have a `filtered-candidate-transformer' attr
      ;; Filter candidates with this func, otherwise just compute
      ;; candidates.
      (helm-process-filtered-candidate-transformer
       (if (or (equal helm-pattern "")
               (equal matchfns '(identity)))
           ;; Compute all candidates up to LIMIT.
           (helm-take-first-elements
            (helm-get-cached-candidates source) limit)
         ;; Compute candidates according to pattern with their match fns.
         (helm-match-from-candidates
          ;; FIXME: What when volatile is used, and the display expected
          ;; comes from the filtered-candidate-transformer fn ?
          ;; In this case match function try to match on real which is maybe not
          ;; a string.
          (helm-get-cached-candidates source) matchfns matchpartfn limit source))
       source))))

(defun helm-render-source (source matches)
  "Display MATCHES from SOURCE according to its settings."
  (helm-log "Source name = %S" (assoc-default 'name source))
  (when matches
    (helm-insert-header-from-source source)
    (if (not (assq 'multiline source))
        (cl-loop for m in matches
                 for count from 1
                 do (helm-insert-match m 'insert source count))
      (let ((start (point))
            (count 0)
            separate)
        (cl-dolist (match matches)
          (cl-incf count)
          (if separate
              (helm-insert-candidate-separator)
            (setq separate t))
          (helm-insert-match match 'insert source count))
        (put-text-property start (point) 'helm-multiline t)))))

(defmacro helm--maybe-use-while-no-input (&rest body)
  "Wrap BODY in `helm-while-no-input' unless initializing a remote connection."
  `(progn
     (if (and (file-remote-p helm-pattern)
              (not (file-remote-p helm-pattern nil t)))
         ;; Tramp will ask for passwd, don't use `helm-while-no-input'.
         ,@body
       (helm-log "Using here `helm-while-no-input'")
       (helm-while-no-input ,@body))))

(defun helm--collect-matches (src-list)
  (let ((matches (helm--maybe-use-while-no-input
                  (cl-loop for src in src-list
                           collect (helm-compute-matches src)))))
    (unless (eq matches t) matches)))

(defun helm--compute-sources (src-list)
  (cl-loop with matches = (helm--collect-matches src-list)
        for src in src-list
        for mtc in matches
        do (helm-render-source src mtc)))

(cl-defun helm-process-delayed-sources (delayed-sources &optional preselect source)
  "Process helm DELAYED-SOURCES.
Move selection to string or regexp PRESELECT if non--nil.
This function is called in `helm-process-delayed-sources-timer'
when emacs is idle for `helm-idle-delay'."
  (with-helm-quittable
    (helm-log "Delayed sources = %S"
              (mapcar (lambda (s)
                        (assoc-default 'name s))
                      delayed-sources))
    (with-current-buffer (helm-buffer-get)
      (save-excursion
        (goto-char (point-max))
        (helm--compute-sources delayed-sources)
        (when (and (not (helm-empty-buffer-p))
                   ;; No selection yet.
                   (= (overlay-start helm-selection-overlay)
                      (overlay-end helm-selection-overlay)))
          (helm-update-move-first-line 'without-hook)))
      (save-excursion
        (goto-char (point-min))
        (helm-log-run-hook 'helm-update-hook))
      (setq helm-force-updating-p nil)
      (unless (assoc 'candidates-process source)
        (helm-display-mode-line (helm-get-current-source))
        (helm-log-run-hook 'helm-after-update-hook))
      (when preselect (helm-preselect preselect source)))))


;;; Core: helm-update
;;
(defun helm-update (&optional preselect source)
  "Update candidates list in `helm-buffer' according to `helm-pattern'.
Argument PRESELECT is a string or regexp used to move selection to a particular
place once updating is done.  It should be used on single source because search
is done on whole `helm-buffer' and not on current source."
  (helm-log "Start updating")
  (helm-kill-async-processes)
  ;; When persistent action have been called
  ;; we have two windows even with `helm-full-frame'.
  ;; So go back to one window when updating if `helm-full-frame'
  ;; is non--nil.
  (with-helm-window
    (when helm-onewindow-p (delete-other-windows)))
  (with-current-buffer (helm-buffer-get)
    (set (make-local-variable 'helm-input-local) helm-pattern)
    (let (normal-sources
          delayed-sources
          matches)
      (unwind-protect
           (progn
             ;; Iterate over all the sources
             (cl-loop for source in (cl-remove-if-not
                                     'helm-update-source-p (helm-get-sources))
                   if (helm-delayed-source-p source)
                   ;; Delayed sources just get collected for later
                   ;; processing
                   collect source into ds
                   else
                   ;; Collect the normal sources
                   collect source into ns
                   ;; Export the variables from cl-loop
                   finally (setq delayed-sources ds
                                 normal-sources ns))
             ;; When no normal-sources erase buffer
             ;; for the next possible delayed source
             ;; or the already computed normal-source
             ;; that is no more "updateable" (requires-pattern).
             (unless normal-sources (erase-buffer))
             ;; Compute matches without rendering the sources.
             (helm-log "Matches: %S"
                       (setq matches (helm--collect-matches normal-sources)))
             ;; If computing matches finished and is not interrupted
             ;; erase the helm-buffer and render results (Fix #1157).
             (when matches
               (erase-buffer)
               (cl-loop for src in normal-sources
                        for mtc in matches
                        do (helm-render-source src mtc))))
        (helm-log "Delayed sources = %S"
                  (mapcar (lambda (s) (assoc-default 'name s))
                          delayed-sources))
        (cond ((and preselect delayed-sources normal-sources)
               ;; Preselection run here when there is
               ;; normal AND delayed sources.
               (helm-log "Update preselect candidate %s" preselect)
               (helm-preselect preselect source))
              (delayed-sources ; Preselection and hooks will run later.
               (helm-update-move-first-line 'without-hook))
              (t              ; No delayed sources, run the hooks now.
               (helm-update-move-first-line)
               (unless (assoc 'candidates-process source)
                 (helm-display-mode-line (helm-get-current-source))
                 (helm-log-run-hook 'helm-after-update-hook))
               (when preselect
                 (helm-log "Update preselect candidate %s" preselect)
                 (helm-preselect preselect source))
               (setq helm-force-updating-p nil)))
        (when delayed-sources
          ;; Allow giving a value to `delayed' attr from inside source.
          ;; Retain the biggest value (the slower) found in DELAYED-SOURCES.
          (let ((helm-idle-delay (cl-loop with delay = helm-idle-delay
                                       for s in delayed-sources
                                       for d = (assoc-default 'delayed s)
                                       when d do (setq delay (max delay d))
                                       finally return delay)))
            (run-with-idle-timer
             ;; Be sure helm-idle-delay is >
             ;; to helm-input-idle-delay
             ;; otherwise use value of helm-input-idle-delay
             ;; or 0.01 if == to 0.
             (max helm-idle-delay helm-input-idle-delay 0.001) nil
             'helm-process-delayed-sources delayed-sources preselect source)))
        (helm-log "end update")))))

;; Update keymap after updating.
;; Now we run this in post-command-hook, it is
;; probably no more needed in helm-after-update-hook.
;; Leave it commented as a reminder for now.
;; (add-hook 'helm-after-update-hook 'helm--maybe-update-keymap)

(defun helm-update-source-p (source)
  "Whether SOURCE need updating or not."
  (let ((len (string-width
              (if (or (assoc 'matchplugin source)
                      (null (assoc 'no-matchplugin source)))
                  ;; Don't count spaces entered when using
                  ;; multi-match.
                  (replace-regexp-in-string " " "" helm-pattern)
                helm-pattern))))
    (and (or (not helm-source-filter)
             (member (assoc-default 'name source) helm-source-filter))
         (>= len
             (helm-aif (assoc 'requires-pattern source) (or (cdr it) 1) 0))
         ;; These incomplete regexps hang helm forever
         ;; so defer update. Maybe replace spaces quoted when using
         ;; multi-match.
         (not (member (replace-regexp-in-string "\\s\\ " " " helm-pattern)
                      helm-update-blacklist-regexps)))))

(defun helm-delayed-source-p (source)
  "Wheter SOURCE is a delayed source or not."
  (or (assoc 'delayed source)
      (and helm-quick-update
           (> (length helm-sources) 1)
           (< (window-height (get-buffer-window (current-buffer)))
              (line-number-at-pos (point-max))))))

(defun helm-update-move-first-line (&optional without-hook)
  "Goto first line of `helm-buffer'."
  (goto-char (point-min))
  (unless without-hook
    (save-excursion (helm-log-run-hook 'helm-update-hook)))
  (helm-next-line))

(defun helm-force-update (&optional preselect)
  "Force recalculation and update of candidates.

The difference with `helm-update' is this function is reevaling
the `init' and `update' attributes functions when present
before running `helm-update', also `helm-candidate-cache',
if some (async candidates are not cached)
is not reinitialized so that candidates are not recomputed
unless pattern have changed.

Selection is preserved to current candidate or moved to PRESELECT
if specified."
  (let ((source    (helm-get-current-source))
        (selection (helm-get-selection nil t))
        ;; `helm-goto-source' need to have all sources displayed
        ;; So disable `helm-quick-update'.
        helm-quick-update)
    (setq helm-force-updating-p t)
    (when source
      (mapc 'helm-force-update--reinit
            (helm-get-sources)))
    (helm-update (or preselect selection) source)
    (with-helm-window (recenter))))

(defun helm-refresh ()
  "Force recalculation and update of candidates."
  (interactive)
  (with-helm-alive-p
    (helm-force-update)))

(defun helm-force-update--reinit (source)
  "Reinit SOURCE by calling his update and/or init functions."
  (helm-aif (helm-funcall-with-source
             source 'helm-candidate-buffer)
      (kill-buffer it))
  (cl-dolist (attr '(update init))
    (helm-aif (assoc-default attr source)
        (helm-funcall-with-source source it)))
  (helm-remove-candidate-cache source))

(defun helm-remove-candidate-cache (source)
  "Remove SOURCE from `helm-candidate-cache'."
  (remhash (assoc-default 'name source) helm-candidate-cache))

(defun helm-insert-match (match insert-function source &optional num)
  "Insert MATCH into `helm-buffer' with INSERT-FUNCTION for SOURCE.
If MATCH is a list then insert the string intended to appear on the display
and store the real value in a text property."
  (let ((start     (point-at-bol (point)))
        (dispvalue (helm-candidate-get-display match))
        (realvalue (cdr-safe match)))
    (when (and (stringp dispvalue)
             (not (zerop (length dispvalue))))
      (funcall insert-function dispvalue)
      ;; Some sources with candidates-in-buffer have already added
      ;; 'helm-realvalue property when creating candidate buffer.
      (unless (get-text-property start 'helm-realvalue)
        (and realvalue
             (put-text-property start (point-at-eol)
                                'helm-realvalue realvalue)))
      (when num
        (put-text-property start (point-at-eol) 'helm-cand-num num))
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


;;; Core: async process
;;
(defun helm-output-filter (process output-string)
  "The `process-filter' function for helm async sources."
  (with-helm-quittable
    (helm-output-filter-1 (assoc process helm-async-processes) output-string)))

(defun helm-output-filter-1 (process-assoc output-string)
  (helm-log "output-string = %S" output-string)
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
         (car process-assoc) output-string source
         (helm-candidate-number-limit source))))
    (helm-output-filter--post-process)))

(defun helm-output-filter--process-source (process output-string source limit)
  (cl-dolist (candidate (helm-transform-candidates
                         (helm-output-filter--collect-candidates
                          (split-string output-string "\n")
                          (assoc 'incomplete-line source))
                         source t))
    (setq candidate
          (helm--maybe-process-filter-one-by-one-candidate candidate source))
    (if (assq 'multiline source)
        (let ((start (point)))
          (helm-insert-candidate-separator)
          (helm-insert-match candidate 'insert-before-markers source
                             (1+ (cdr (assoc 'item-count source))))
          (put-text-property start (point) 'helm-multiline t))
        (helm-insert-match candidate 'insert-before-markers source
                           (1+ (cdr (assoc 'item-count source)))))
    (cl-incf (cdr (assoc 'item-count source)))
    (when (>= (assoc-default 'item-count source) limit)
      (helm-kill-async-process process)
      (cl-return))))

(defun helm-output-filter--collect-candidates (lines incomplete-line-info)
  "Collect LINES maybe completing the truncated first and last lines."
  ;; The output of process may come in chunks of any size,
  ;; so the last line of LINES come truncated, this truncated line is
  ;; stored in INCOMPLETE-LINE-INFO and will be concated with the first
  ;; incomplete line of next chunk arriving.
  ;; INCOMPLETE-LINE-INFO is an attribute of source which is created
  ;; with an empty string when the source is computed => (incomplete-line . "")
  (helm-log "incomplete-line-info = %S" (cdr incomplete-line-info))
  (butlast
   (cl-loop for line in lines
            ;; On start `incomplete-line-info' value is empty string.
            for newline = (helm-aif (cdr incomplete-line-info)
                              (prog1
                                  (concat it line)
                                (setcdr incomplete-line-info nil))
                            line)
            collect newline
            ;; Store last incomplete line (last chunk truncated)
            ;; until new output arrives.
            ;; Previously we were storing 'line' in incomplete-line-info
            ;; assuming output is truncated in only two chunks,
            ;; which may be wrong if output is very large and is truncated
            ;; in more than two chunks, so store now 'newline' which
            ;; contain the previous chunks (Issue #1187).
            finally do (setcdr incomplete-line-info newline))))

(defun helm-output-filter--post-process ()
  (let ((src (helm-get-current-source)))
    (helm-log-run-hook 'helm-update-hook)
    (helm-aif (get-buffer-window helm-buffer 'visible)
        (with-selected-window it
          (helm-skip-noncandidate-line 'next)
          (helm-mark-current-line)
          (helm-display-mode-line src)
          (helm-log-run-hook 'helm-after-update-hook)))))

(defun helm-process-deferred-sentinel-hook (process event file)
  "Defer remote processes in sentinels.
Meant to be called at beginning of a sentinel process function."
  (when (and (string= event "finished\n")
             (or (file-remote-p file)
                 ;; `helm-suspend-update-flag'
                 ;; is non--nil here only during a
                 ;; running process, this will never be called
                 ;; when user set it explicitely with `C-!'.
                 helm-suspend-update-flag))
    (setq helm-suspend-update-flag t)
    ;; Kill the process but don't delete entry in
    ;; `helm-async-processes'.
    (helm-kill-async-process process)
    ;; When tramp tries to open the same connection twice in a
    ;; short time frame (less than 5s) it throw 'suppress which
    ;; call the real-handler on the main "Emacs", so we wait
    ;; 5s before updating to avoid this [1], but allowing user to
    ;; enter input during this delay.
    ;; [1] On last Emacs versions, this is fixed and tramp return
    ;; nil in this situation.
    ;; Note: It is difficult to have a value < to 5 for
    ;; `tramp-connection-min-time-diff', because the process die
    ;; when calling too quickly same process.
    (run-at-time (or (and (boundp 'tramp-connection-min-time-diff)
                          tramp-connection-min-time-diff)
                     5)
                 nil (lambda ()
                         (when helm-alive-p ; Don't run timer fn after quit.
                           (setq helm-suspend-update-flag nil)
                           (helm-check-minibuffer-input))))))

(defun helm-kill-async-processes ()
  "Kill all asynchronous processes registered in `helm-async-processes'."
  (while helm-async-processes
    (helm-kill-async-process (caar helm-async-processes))
    (setq helm-async-processes (cdr helm-async-processes))))

(defun helm-kill-async-process (process)
  "Stop output from `helm-output-filter' and kill associated PROCESS."
  (set-process-filter process nil)
  (delete-process process))


;;; Core: action
;;
(defun helm-execute-selection-action ()
  "Execute current action and kill the action buffer if present."
  (helm-log-run-hook 'helm-before-action-hook)
  ;; Position can be different when `helm-current-buffer'
  ;; is splitted, so jump to this position before executing action.
  (helm-current-position 'restore)
  (unwind-protect
       (helm-execute-selection-action-1)
    (helm-aif (get-buffer helm-action-buffer)
        (kill-buffer it))
    (helm-log-run-hook 'helm-after-action-hook)))

(defun helm-execute-selection-action-1 (&optional
                                          selection action
                                          preserve-saved-action)
  "Execute ACTION on current SELECTION.
If PRESERVE-SAVED-ACTION is non--nil save action."
  (helm-log "executing action")
  (setq action (helm-get-default-action
                (or action
                    helm-saved-action
                    (if (get-buffer helm-action-buffer)
                        (helm-get-selection helm-action-buffer)
                      (helm-get-actions-from-current-source)))))
  (let ((source (or helm-saved-current-source
                    (helm-get-current-source)))
        non-essential)
    (setq selection (helm-coerce-selection
                     (or selection
                         helm-saved-selection
                         (helm-get-selection)
                         (and (assoc 'accept-empty source) ""))
                     source))
    (unless preserve-saved-action (setq helm-saved-action nil))
    (when (and selection action) (funcall action selection))))

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

(defun helm-select-action ()
  "Select an action for the currently selected candidate.
If action buffer is selected, back to the helm buffer."
  (interactive)
  (helm-log-run-hook 'helm-select-action-hook)
  (setq helm-saved-selection (helm-get-selection))
  (with-selected-frame (with-helm-window (selected-frame))
    (prog1
        (cond ((get-buffer-window helm-action-buffer 'visible)
               (set-window-buffer (get-buffer-window helm-action-buffer)
                                  helm-buffer)
               (kill-buffer helm-action-buffer)
               (helm-set-pattern helm-input 'noupdate))
              (helm-saved-selection
               (setq helm-saved-current-source (helm-get-current-source))
               (let ((actions (helm-get-actions-from-current-source)))
                 (if (functionp actions)
                     (message "Sole action: %s" actions)
                     (helm-show-action-buffer actions)
                     ;; Be sure the minibuffer is entirely deleted (#907).
                     (helm--delete-minibuffer-contents-from "")
                     ;; Make `helm-pattern' differs from the previous value.
                     (setq helm-pattern 'dummy)
                     (helm-check-minibuffer-input))))
              (t (message "No Actions available")))
      (helm-display-mode-line (helm-get-current-source))
      (run-hooks 'helm-window-configuration-hook))))

(defun helm-show-action-buffer (actions)
  (with-current-buffer (get-buffer-create helm-action-buffer)
    (erase-buffer)
    (buffer-disable-undo)
    (set-window-buffer (get-buffer-window helm-buffer) helm-action-buffer)
    (set (make-local-variable 'helm-sources)
         (list
          (helm-build-sync-source "Actions"
            :volatile t
            :nomark t
            :keymap 'helm-map
            :candidates actions
            :mode-line '("Action(s)" "TAB:BackToCands RET/f1/f2/fn:NthAct")
            :candidate-transformer
             (lambda (candidates)
               (cl-loop for (i . j) in candidates
                        for count from 1
                        collect
                        (cons (concat (cond ((> count 12)
                                             "      ")
                                            ((< count 10)
                                             (format "[f%s]  " count))
                                            (t (format "[f%s] " count)))
                                      (propertize i 'face 'helm-action))
                              j)))
            :candidate-number-limit nil)))
    (set (make-local-variable 'helm-source-filter) nil)
    (set (make-local-variable 'helm-selection-overlay) nil)
    (helm-initialize-overlays helm-action-buffer)))


;; Core: selection

(defun helm-display-source-at-screen-top-maybe (unit)
  "Display source at top of screen when UNIT value is 'source.
With any other value of UNIT return nil."
  (when (and helm-display-source-at-screen-top (eq unit 'source))
    (set-window-start (selected-window)
                      (save-excursion (forward-line -1) (point)))))

(defun helm-skip-noncandidate-line (direction)
  "Skip source header or candidates separator when going in DIRECTION.
Possible value of DIRECTION are 'next or 'previous.
Same as `helm-skip-header-and-separator-line' but ensure
point is moved to the right place when at bop or eob."
  (helm-skip-header-and-separator-line direction)
  (and (bobp) (forward-line 1))     ; Skip first header.
  (and (eobp) (forward-line -1)))   ; Avoid last empty line.

(defun helm-skip-header-and-separator-line (direction)
  "Skip source header or candidate separator when going to next/previous line.
Possible value of DIRECTION are 'next or 'previous."
  (let ((fn (cl-ecase direction
              (next 'eobp)
              (previous 'bobp))))
  (while (and (not (funcall fn))
              (or (helm-pos-header-line-p)
                  (helm-pos-candidate-separator-p)))
    (forward-line (if (and (eq direction 'previous)
                           (not (eq (point-at-bol) (point-min))))
                      -1 1)))))

(defun helm-display-mode-line (source &optional force)
  "Setup mode-line and header-line for `helm-buffer'."
  (set (make-local-variable 'helm-mode-line-string)
       (helm-interpret-value (or (and (listp source) ; Check if source is empty.
                                      (assoc-default 'mode-line source))
                                 (default-value 'helm-mode-line-string))
                             source))
  (let ((follow (and (eq (cdr (assq 'follow source)) 1) " (HF)"))
        (marked (and helm-marked-candidates
                     (cl-loop with cur-name = (assoc-default 'name source)
                              for c in helm-marked-candidates
                              for name = (assoc-default 'name (car c))
                              when (string= name cur-name)
                              collect c))))
    ;; Setup mode-line.
    (if helm-mode-line-string
        (setq mode-line-format
              `(" " mode-line-buffer-identification " "
                    (:eval (format "L%-3d" (helm-candidate-number-at-point)))
                    ,follow
                    (:eval ,(and marked
                                 (concat
                                  " "
                                  (propertize
                                   (format "M%d" (length marked))
                                   'face 'helm-visible-mark))))
                    (:eval (when ,helm--mode-line-display-prefarg
                             (let ((arg (prefix-numeric-value
                                         (or prefix-arg current-prefix-arg))))
                               (unless (= arg 1)
                                 (propertize (format " [prefarg:%s]" arg)
                                             'face 'helm-prefarg)))))
                    " "
                    (:eval (helm-show-candidate-number
                            (car-safe helm-mode-line-string)))
                    " " helm--mode-line-string-real " " mode-line-end-spaces)
              helm--mode-line-string-real
              (substitute-command-keys (if (listp helm-mode-line-string)
                                           (cadr helm-mode-line-string)
                                           helm-mode-line-string)))
        (setq mode-line-format (default-value 'mode-line-format)))
    ;; Setup header-line.
    (cond (helm-echo-input-in-header-line
           (setq force t)
           (helm--set-header-line))
          (helm-display-header-line
           (let ((hlstr (helm-interpret-value
                          (and (listp source)
                               (assoc-default 'header-line source))
                          source))
                 (endstr (make-string (window-width) ? )))
             (setq header-line-format
                   (propertize (concat " " hlstr endstr)
                               'face 'helm-header))))))
  (when force (force-mode-line-update)))

(defun helm--set-header-line (&optional update)
  (with-selected-window (minibuffer-window)
    (let* ((beg  (save-excursion (vertical-motion 0 (helm-window)) (point)))
           (end  (save-excursion (end-of-visual-line) (point)))
           ;; The visual line where the cursor is.
           (cont (buffer-substring beg end))
           (pref (propertize
                  " "
                  'display (if (string-match-p (regexp-quote helm--prompt) cont)
                               '(space :width left-fringe)
                               (propertize
                                "->"
                                'face 'helm-header-line-left-margin))))
           (pos  (- (point) beg)))
      (with-helm-buffer
        (setq header-line-format (concat pref cont " "))
        (put-text-property
         ;; Increment pos to handle the space before prompt (i.e `pref').
         (1+ pos) (+ 2 pos)
         'face ;don't just use 'cursor; this can hide the current character
         (list :inverse-video t
               :foreground (face-background 'cursor)
               :background (face-background 'default))
         header-line-format)
        (when update (force-mode-line-update))))))

(defun helm--update-header-line ()
  ;; This should be used in `post-command-hook',
  ;; nowhere else.
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (helm--set-header-line t)))

(defun helm-hide-minibuffer-maybe ()
  "Hide minibuffer contents in a Helm session.
This function should normally go to `helm-minibuffer-set-up-hook'.
It has no effect if `helm-echo-input-in-header-line' is nil."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                              `(:background ,bg-color :foreground ,bg-color)))
      (setq cursor-type nil))))

(defun helm-show-candidate-number (&optional name)
  "Used to display candidate number in mode-line.
You can specify NAME of candidates e.g \"Buffers\" otherwise
it is \"Candidate\(s\)\" by default."
  (when helm-alive-p
    (unless (helm-empty-source-p)
      ;; Build a fixed width string when candidate-number < 1000
      (let* ((cand-name (or name "Candidate(s)"))
             (width (length (format "[999 %s]" cand-name))))
        (propertize
         (format (concat "%-" (number-to-string width) "s")
                 (format "[%s %s]"
                         (helm-get-candidate-number 'in-current-source)
                         cand-name))
         'face 'helm-candidate-number)))))

(cl-defun helm-move-selection-common (&key where direction)
  "Move the selection marker to a new position.
Position is determined by WHERE and DIRECTION.
Key arg WHERE can be one of:
 - line
 - page
 - edge
 - source
Key arg DIRECTION can be one of:
 - previous
 - next
 - A source or a source name when used with :WHERE 'source."
  (let ((move-func (cl-case where
                     (line (cl-ecase direction
                             (previous 'helm-move--previous-line-fn)
                             (next 'helm-move--next-line-fn)))
                     (page (cl-ecase direction
                             (previous 'helm-move--previous-page-fn)
                             (next 'helm-move--next-page-fn)))
                     (edge (cl-ecase direction
                             (previous 'helm-move--beginning-of-buffer-fn)
                             (next 'helm-move--end-of-buffer-fn)))
                     (source (cl-case direction
                               (previous 'helm-move--previous-source-fn)
                               (next 'helm-move--next-source-fn)
                               (t (lambda () ; A source is passed as DIRECTION arg.
                                    (helm-move--goto-source-fn direction))))))))
    (unless (or (helm-empty-buffer-p (helm-buffer-get))
                (not (helm-window)))
      (with-helm-window
        (helm-log-run-hook 'helm-move-selection-before-hook)
        (funcall move-func)
        (and (memq direction '(next previous))
             (helm-skip-noncandidate-line direction))
        (when (helm-pos-multiline-p)
          (helm-move--beginning-of-multiline-candidate))
        (helm-display-source-at-screen-top-maybe where)
        (when (helm-get-previous-header-pos)
          (helm-mark-current-line))
        (helm-display-mode-line (helm-get-current-source))
        (helm-log-run-hook 'helm-move-selection-after-hook)))))

(defun helm-move--beginning-of-multiline-candidate ()
  (let ((header-pos (helm-get-previous-header-pos))
        (separator-pos (helm-get-previous-candidate-separator-pos)))
    (when header-pos
      (goto-char (if (or (null separator-pos)
                         (< separator-pos header-pos))
                     header-pos
                     separator-pos))
      (forward-line 1))))

(defun helm-move--previous-multi-line-fn ()
  (forward-line -1)
  (unless (helm-pos-header-line-p)
    (helm-skip-header-and-separator-line 'previous)
    (helm-move--beginning-of-multiline-candidate)))

(defun helm-move--previous-line-fn ()
  (if (not (helm-pos-multiline-p))
      (forward-line -1)
    (helm-move--previous-multi-line-fn))
  (when (and helm-move-to-line-cycle-in-source
             (helm-pos-header-line-p))
    (forward-line 1)
    (helm-move--end-of-source)
    ;; We are at end of helm-buffer
    ;; check if last candidate is a multiline candidate
    ;; and jump to it
    (when (and (eobp)
               (save-excursion (forward-line -1) (helm-pos-multiline-p)))
      (helm-move--previous-multi-line-fn))))

(defun helm-move--next-multi-line-fn ()
  (let ((header-pos (helm-get-next-header-pos))
        (separator-pos (helm-get-next-candidate-separator-pos)))
    (cond ((and separator-pos
                (or (null header-pos) (< separator-pos header-pos)))
           (goto-char separator-pos))
          (header-pos
           (goto-char header-pos)))))

(defun helm-move--next-line-fn ()
  (if (not (helm-pos-multiline-p))
      (forward-line 1)
    (helm-move--next-multi-line-fn))
  (when (and helm-move-to-line-cycle-in-source
             (or (save-excursion (and (helm-pos-multiline-p)
                                      (goto-char (overlay-end
                                                  helm-selection-overlay))
                                      (helm-end-of-source-p t)))
                 (helm-end-of-source-p t)))
    (helm-move--beginning-of-source)))

(defun helm-move--previous-page-fn ()
  (condition-case nil
      (scroll-down)
    (beginning-of-buffer (goto-char (point-min)))))

(defun helm-move--next-page-fn ()
  (condition-case nil
      (scroll-up)
    (end-of-buffer (goto-char (point-max)))))

(defun helm-move--beginning-of-buffer-fn ()
  (goto-char (point-min)))

(defun helm-move--end-of-buffer-fn ()
  (goto-char (point-max)))

(defun helm-move--end-of-source ()
  (goto-char (or (helm-get-next-header-pos) (point-max)))
  (when (helm-pos-header-line-p) (forward-line -2)))

(defun helm-move--beginning-of-source ()
  (goto-char (helm-get-previous-header-pos))
  (forward-line 1))

(defun helm-move--previous-source-fn ()
  (forward-line -1)
  (if (bobp)
      (goto-char (point-max))
    (helm-skip-header-and-separator-line 'previous))
  (goto-char (helm-get-previous-header-pos))
  (forward-line 1))

(defun helm-move--next-source-fn ()
  (goto-char (or (and (not (save-excursion
                             (forward-line 1) (eobp)))
                      ;; Empty source at eob are just
                      ;; not displayed unless they are dummy.
                      ;; Issue #1117.
                      (helm-get-next-header-pos))
                 (point-min))))

(defun helm-move--goto-source-fn (source-or-name)
  (goto-char (point-min))
  (let ((name (if (stringp source-or-name) source-or-name
                (assoc-default 'name source-or-name))))
    (condition-case err
        (while (not (string= name (helm-current-line-contents)))
          (goto-char (helm-get-next-header-pos)))
      (error (helm-log "%S" err)))))

(defun helm-candidate-number-at-point ()
  (with-helm-buffer
    (or (get-text-property (point) 'helm-cand-num) 1)))

(defun helm--next-or-previous-line (direction &optional arg)
  ;; Be sure to not use this in non--interactives calls.
  (let ((helm-move-to-line-cycle-in-source
         (and helm-move-to-line-cycle-in-source arg)))
    (if (and arg (> arg 1))
        (cl-loop with pos = (helm-candidate-number-at-point)
                 with cand-num = (helm-get-candidate-number t)
                 with iter = (min arg (if (eq direction 'next)
                                          (- cand-num pos)
                                          (min arg (1- pos))))
                 for count from 1
                 while (<= count iter)
                 do
                 (helm-move-selection-common :where 'line :direction direction))
        (helm-move-selection-common :where 'line :direction direction))))

(defun helm-previous-line (&optional arg)
  "Move selection to the ARG previous line(s).
Same behavior than `helm-next-line' when called with a numeric prefix arg."
  (interactive "p")
  (helm--next-or-previous-line 'previous arg))

(defun helm-next-line (&optional arg)
  "Move selection to the next ARG line(s).
When a numeric prefix arg is given and this numeric arg
is > to the number of candidates, move to last candidate of
current source (i.e don't move to next source if some)."
  (interactive "p")
  (helm--next-or-previous-line 'next arg))

(defun helm-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (helm-move-selection-common :where 'page :direction 'previous))

(defun helm-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (helm-move-selection-common :where 'page :direction 'next))

(defun helm-beginning-of-buffer ()
  "Move selection at the top."
  (interactive)
  (helm-move-selection-common :where 'edge :direction 'previous))

(defun helm-end-of-buffer ()
  "Move selection at the bottom."
  (interactive)
  (helm-move-selection-common :where 'edge :direction 'next))

(defun helm-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (helm-move-selection-common :where 'source :direction 'previous))

(defun helm-next-source ()
  "Move selection to the next source."
  (interactive)
  (helm-move-selection-common :where 'source :direction 'next))

(defun helm-goto-source (source-or-name)
  "Move the selection to the source SOURCE-OR-NAME."
  (helm-move-selection-common :where 'source :direction source-or-name))

(defun helm--follow-action (arg)
  (let ((helm--temp-follow-flag t))
    (when (or (eq last-command 'helm-follow-action-forward)
              (eq last-command 'helm-follow-action-backward)
              (eq last-command 'helm-execute-persistent-action))
      (if (> arg 0)
          (helm-next-line 1)
        (helm-previous-line 1)))
    (helm-execute-persistent-action)))

(defun helm-follow-action-forward ()
  "Go to next line and execute persistent action."
  (interactive)
  (helm--follow-action 1))

(defun helm-follow-action-backward ()
  "Go to previous line and execute persistent action."
  (interactive)
  (helm--follow-action -1))

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
               (and header-pos separator-pos
                    (< separator-pos header-pos)
                    separator-pos)
               header-pos
               (point-max)))
       (1+ (point-at-eol))))
    (setq helm-selection-point (overlay-start helm-selection-overlay)))
  (helm-follow-execute-persistent-action-maybe))

(defun helm-confirm-and-exit-minibuffer ()
  "Maybe ask for confirmation when exiting helm.
It is similar to `minibuffer-complete-and-exit' adapted to helm.
If `minibuffer-completion-confirm' value is 'confirm,
send in minibuffer confirm message and exit on next hit.
If `minibuffer-completion-confirm' value is t,
don't exit and send message 'no match'."
  (interactive)
  (if (and (helm--updating-p)
           (null helm--reading-passwd-or-string))
      (progn (message "[Display not ready]")
             (sit-for 0.5) (message nil))
      (let* ((empty-buffer-p (with-current-buffer helm-buffer
                               (eq (point-min) (point-max))))
             (sel (helm-get-selection))
             (unknown (and (not empty-buffer-p)
                           (string= (get-text-property
                                     0 'display
                                     (helm-get-selection nil 'withprop))
                                    "[?]"))))
        (cond ((and (or empty-buffer-p unknown)
                    (eq minibuffer-completion-confirm 'confirm))
               (setq helm-minibuffer-confirm-state
                     'confirm)
               (setq minibuffer-completion-confirm nil)
               (minibuffer-message " [confirm]"))
              ((and (or empty-buffer-p
                        (unless (if minibuffer-completing-file-name
                                    (and minibuffer-completion-predicate
                                         (funcall minibuffer-completion-predicate sel))
                                    (and (stringp sel)
                                         ;; SEL may be a cons cell when helm-comp-read
                                         ;; is called directly with a collection composed
                                         ;; of (display . real) and real is a cons cell.
                                         (try-completion sel minibuffer-completion-table
                                                         minibuffer-completion-predicate)))
                          unknown))
                    (eq minibuffer-completion-confirm t))
               (minibuffer-message " [No match]"))
              (t
               (setq helm-minibuffer-confirm-state nil)
               (helm-exit-minibuffer))))))
(add-hook 'helm-after-update-hook 'helm-confirm-and-exit-hook)

(defun helm-confirm-and-exit-hook ()
  "Restore `minibuffer-completion-confirm' when helm update."
  (unless (or (eq minibuffer-completion-confirm t)
              (not helm-minibuffer-confirm-state))
    (setq minibuffer-completion-confirm
          helm-minibuffer-confirm-state)))

(defun helm-read-string (prompt &optional initial-input history
                                  default-value inherit-input-method)
  "Same as `read-string' but for reading string from a helm session."
  (let ((helm--reading-passwd-or-string t))
    (read-string
     prompt initial-input history default-value inherit-input-method)))

(defun helm--updating-p ()
  ;; helm timer is between two cycles.
  ;; IOW `helm-check-minibuffer-input' haven't yet compared input
  ;; and `helm-pattern'.
  (or (not (equal (minibuffer-contents) helm-pattern))
      ;; `helm-check-minibuffer-input' have launched `helm-update'.
      helm--in-update))

(defun helm-maybe-exit-minibuffer ()
  (interactive)
  (if (and (helm--updating-p)
           (null helm--reading-passwd-or-string))
      (progn (message "[Display not ready]")
             (sit-for 0.5) (message nil))
      (helm-exit-minibuffer)))

(defun helm-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (unless helm-current-prefix-arg
    (setq helm-current-prefix-arg current-prefix-arg))
  (setq helm-exit-status 0)
  (helm-log-run-hook 'helm-exit-minibuffer-hook)
  (exit-minibuffer))

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
  (let ((hp (helm-get-next-header-pos)))
    (helm-aif (next-single-property-change (point) 'helm-candidate-separator)
        (or
         ;; Be sure we don't catch
         ;; the separator of next source.
         (and hp (< it hp) it)
         ;; The separator found is in next source
         ;; we are at last cand, so use the header pos.
         (and hp (< hp it) hp)
         ;; A single source, just try next separator.
         it))))

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
(defun helm-debug-output ()
  "Show all helm-related variables at this time."
  (interactive)
  (helm-help-internal " *Helm Debug*" 'helm-debug-output-function))

(defun helm-debug-output-function (&optional vars)
  (message "Calculating all helm-related values...")
  (insert "If you debug some variables or forms, set `helm-debug-variables'
to a list of forms.\n\n")
  (cl-dolist (v (or vars
                    helm-debug-variables
                    (apropos-internal "^helm-" 'boundp)))
    (insert "** "
            (pp-to-string v) "\n"
            (pp-to-string (with-current-buffer helm-buffer (eval v))) "\n"))
  (message "Calculating all helm-related values...Done"))

;;;###autoload
(defun helm-debug-toggle ()
  "Enable/disable helm debug from outside of helm session."
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))

(defun helm-enable-or-switch-to-debug ()
  "First hit enable helm debugging, second hit switch to debug buffer."
  (interactive)
  (with-helm-alive-p
    (if helm-debug
        (helm-run-after-exit
         #'helm-debug-open-last-log)
        (setq helm-debug t)
        (message "Debugging enabled"))))


;; Core: misc
(defun helm-kill-buffer-hook ()
  "Remove tick entry from `helm-tick-hash' and remove buffer from
`helm-buffers' when killing a buffer."
  (cl-loop for key being the hash-keys in helm-tick-hash
        if (string-match (format "^%s/" (regexp-quote (buffer-name))) key)
        do (remhash key helm-tick-hash))
  (setq helm-buffers (remove (buffer-name) helm-buffers)))
(add-hook 'kill-buffer-hook 'helm-kill-buffer-hook)

(defun helm-preselect (candidate-or-regexp &optional source)
  "Move `helm-selection-overlay' to CANDIDATE-OR-REGEXP on startup.
Arg CANDIDATE-OR-REGEXP can be a string or a cons cell of two strings.
When it is a cons cell helm will try to jump first to first element of cons cell
and then to second, allowing a finer preselection when possible duplicates are
before the candidate we want to preselect."
  (with-helm-window
    (when candidate-or-regexp
      (if (and helm-force-updating-p source)
          (helm-goto-source source)
        (goto-char (point-min))
        (forward-line 1))
      (let ((start (point)))
        (or
         (if (consp candidate-or-regexp)
             (and (re-search-forward (car candidate-or-regexp) nil t)
                  (re-search-forward (cdr candidate-or-regexp) nil t))
             (re-search-forward candidate-or-regexp nil t))
         (goto-char start))))
    (forward-line 0) ; Avoid scrolling right on long lines.
    (when (helm-pos-multiline-p)
      (helm-move--beginning-of-multiline-candidate))
    (when (helm-pos-header-line-p) (forward-line 1))
    (helm-mark-current-line)
    (helm-display-mode-line (helm-get-current-source))
    (helm-log-run-hook 'helm-after-preselection-hook)))

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
           (when (helm-end-of-source-p t)
             (let ((headp (save-excursion
                            (forward-line -1)
                            (not (helm-pos-header-line-p)))))
               (and headp (forward-line -1))))))
    (unless (helm-end-of-source-p t)
      (helm-mark-current-line))))

(defun helm-end-of-source-p (&optional at-point)
  "Return non--nil if we are at eob or end of source."
  (save-excursion
    (if (and (helm-pos-multiline-p) (null at-point))
        (null (helm-get-next-candidate-separator-pos))
      (forward-line (if at-point 0 1))
      (or (eq (point-at-bol) (point-at-eol))
          (helm-pos-header-line-p)
          (eobp)))))

(defun helm-beginning-of-source-p (&optional at-point)
  "Return non--nil if we are at bob or beginning of source."
  (save-excursion
    (if (and (helm-pos-multiline-p) (null at-point))
        (null (helm-get-previous-candidate-separator-pos))
      (forward-line (if at-point 0 -1))
      (or (eq (point-at-bol) (point-at-eol))
          (helm-pos-header-line-p)
          (bobp)))))

(defun helm-edit-current-selection-internal (func)
  (with-helm-window
    (forward-line 0)
    (let ((realvalue (get-text-property (point) 'helm-realvalue))
          (multiline (get-text-property (point) 'helm-multiline)))
      (funcall func)
      (forward-line 0)
      (and realvalue
           (put-text-property (point) (point-at-eol)
                              'helm-realvalue realvalue))
      (and multiline
           (put-text-property (point) (point-at-eol)
                              'helm-multiline multiline))
      (helm-mark-current-line))))

(defmacro helm-edit-current-selection (&rest forms)
  "Evaluate FORMS at current selection in the helm buffer.
Used generally to modify current selection."
  (declare (indent 0) (debug t))
  `(helm-edit-current-selection-internal
    (lambda () ,@forms)))

(defun helm--delete-minibuffer-contents-from (from-str)
  ;; Giving an empty string value to FROM-STR delete all.
  (let ((input (minibuffer-contents)))
    (helm-reset-yank-point)
    (if (> (length input) 0)
        ;; minibuffer is not empty, delete contents from end
        ;; of FROM-STR and update.
        (helm-set-pattern from-str)
        ;; minibuffer is already empty, force update.
        (helm-force-update))))

(defun helm-delete-minibuffer-contents (&optional arg)
  "Delete minibuffer contents.
When `helm-delete-minibuffer-contents-from-point' is non--nil,
delete minibuffer contents from point instead of deleting all.
Giving a prefix arg reverse this behavior.
When at end of minibuffer delete all."
  (interactive "P")
  (let ((str (if helm-delete-minibuffer-contents-from-point
                 (if (or arg (eobp))
                     "" (helm-minibuffer-completion-contents))
                 (if (and arg (not (eobp)))
                     (helm-minibuffer-completion-contents) ""))))
    (helm--delete-minibuffer-contents-from str)))


;;; Plugins (Deprecated in favor of helm-types)
;;
;; i.e Inherit instead of helm-type-* classes in your own classes.

;; [DEPRECATED] Enable multi-match by default in old sources.
;; This is deprecated and will not run in sources
;; created by helm-source.
;; Keep it for backward compatibility with old sources.
(defun helm-compile-source--multi-match (source)
  (if (assoc 'no-matchplugin source)
      source
    (let* ((searchers        helm-mm-default-search-functions)
           (defmatch         (helm-aif (assoc-default 'match source)
                                 (helm-mklist it)))
           (defmatch-strict  (helm-aif (assoc-default 'match-strict source)
                                 (helm-mklist it)))
           (defsearch        (helm-aif (assoc-default 'search source)
                                 (helm-mklist it)))
           (defsearch-strict (helm-aif (assoc-default 'search-strict source)
                                 (helm-mklist it)))
           (matchfns         (cond (defmatch-strict)
                                   (defmatch
                                    (append helm-mm-default-match-functions defmatch))
                                   (t helm-mm-default-match-functions)))
           (searchfns        (cond (defsearch-strict)
                                   (defsearch
                                    (append searchers defsearch))
                                   (t searchers))))
      `(,(if (assoc 'candidates-in-buffer source)
             `(search ,@searchfns) `(match ,@matchfns))
         ,@source))))

(add-to-list 'helm-compile-source-functions 'helm-compile-source--multi-match)

(defun helm-compile-source--type (source)
  (helm-aif (assoc-default 'type source)
      (append source (assoc-default it helm-type-attributes) nil)
    source))

(defun define-helm-type-attribute (type definition &optional doc)
  "Register type attribute of TYPE as DEFINITION with DOC.
DOC is displayed in `helm-type-attributes' docstring.

Use this function is better than setting `helm-type-attributes' directly."
  (cl-loop for i in definition do
        ;; without `ignore-errors', error at emacs22
        (ignore-errors (setf i (delete nil i))))
  (helm-add-type-attribute type definition)
  (and doc (helm-document-type-attribute type doc))
  nil)

(defun helm-document-attribute (attribute short-doc &optional long-doc)
  "Register ATTRIBUTE documentation introduced by plug-in.
SHORT-DOC is displayed beside attribute name.
LONG-DOC is displayed below attribute name and short documentation."
  (declare (indent 2))
  (if long-doc
      (setq short-doc (concat "(" short-doc ")"))
    (setq long-doc short-doc
          short-doc ""))
  (add-to-list 'helm-attributes attribute t)
  (put attribute 'helm-attrdoc
       (concat "- " (symbol-name attribute)
               " " short-doc "\n\n" long-doc "\n")))

(defun helm-add-type-attribute (type definition)
  (helm-aif (assq type helm-type-attributes)
      (setq helm-type-attributes (delete it helm-type-attributes)))
  (push (cons type definition) helm-type-attributes))

(defun helm-document-type-attribute (type doc)
  (add-to-list 'helm-types type t)
  (put type 'helm-typeattrdoc
       (concat "- " (symbol-name type) "\n\n" doc "\n")))

;; Built-in plug-in: dummy
(defun helm-dummy-candidate (_candidate _source)
  "Use `helm-pattern' as CANDIDATE in SOURCE."
  ;; `source' is defined in filtered-candidate-transformer
  (list helm-pattern))

(defun helm-compile-source--dummy (source)
  (if (assoc 'dummy source)
      (progn
        (unless (helm-attr-defined
                 'filtered-candidate-transformer source)
          (helm-attrset 'filtered-candidate-transformer
                        'helm-dummy-candidate source))
        (append source
                '((candidates "dummy")
                  (accept-empty)
                  (match identity)
                  (volatile))))
    source))

;; Built-in plug-in: candidates-in-buffer
(defun helm-candidates-in-buffer (&optional source)
  "The top level function used to store candidates in `helm-source-in-buffer'.

Candidates are stored in a buffer generated internally by the function
`helm-candidate-buffer'.  Each candidate must be placed in one line.
This function is meant to be used in candidates-in-buffer or candidates
attribute of an helm source.  Especially fast for many (1000+) candidates.

The buffer is created and feeded in the init attribute function of helm.

e.g:

     (helm-build-in-buffer-source \"test\"
       :init (lambda ()
               (helm-init-candidates-in-buffer
                   'global '(foo foa fob bar baz))))

A shortcut can be used to simplify:
          
     (helm-build-in-buffer-source \"test\"
       :data '(foo foa fob bar baz))

The usage of the `candidates-in-buffer' is deprecated in favor
of `helm-source-in-buffer' class.

It is anyway described below and provided for backward compatibility.

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

The `candidates-in-buffer' attribute implies the volatile attribute.
The volatile attribute is needed because `helm-candidates-in-buffer'
creates candidates dynamically and need to be called everytime
`helm-pattern' changes.

Because `helm-candidates-in-buffer' plays the role of `match' attribute
function, specifying `(match identity)' makes the source slightly faster.

However if source contain `match-part' attribute, match is computed only
on part of candidate returned by the call of function provided by this attribute.
The function should have one arg, candidate, and return only
a specific part of candidate.

To customize `helm-candidates-in-buffer' behavior, use `search',
`get-line' and `match-part' attributes."
  (let ((src (or source (helm-get-current-source))))
    (helm-candidates-in-buffer-1
     (helm-candidate-buffer)
     helm-pattern
     (or (assoc-default 'get-line src)
         #'buffer-substring-no-properties)
     (or (assoc-default 'search src)
         '(helm-candidates-in-buffer-search-default-fn))
     (helm-candidate-number-limit src)
     (helm-attr 'match-part)
     src)))

(defun helm-candidates-in-buffer-search-default-fn (pattern)
  "Search PATTERN with `re-search-forward' with bound and noerror args."
  (condition-case _err
      (re-search-forward pattern nil t)
    (invalid-regexp nil)))

(defun helm-candidates-in-buffer-1 (buffer pattern get-line-fn
                                    search-fns limit
                                    match-part-fn source)
  "Return the list of candidates inserted in BUFFER matching PATTERN."
  ;; buffer == nil when candidates buffer does not exist.
  (when buffer
    (with-current-buffer buffer
      (let ((inhibit-point-motion-hooks t)
            (start-point (1- (point-min))))
        (goto-char start-point)
        (if (string= pattern "")
            (helm-initial-candidates-from-candidate-buffer
             get-line-fn limit)
          (helm-search-from-candidate-buffer
           pattern get-line-fn search-fns limit
           start-point match-part-fn source))))))

(defun helm-search-from-candidate-buffer (pattern get-line-fn search-fns
                                          limit start-point match-part-fn source)
  (let (buffer-read-only
        matches
        newmatches
        (item-count 0)
        (case-fold-search (helm-set-case-fold-search)))
    (helm--search-from-candidate-buffer-1
     (lambda ()
       (clrhash helm-cib-hash)
       (cl-dolist (searcher search-fns)
         (goto-char start-point)
         (forward-line 1) ; >>>[1]
         (setq newmatches nil)
         (cl-loop with pos-lst
                  while (and (setq pos-lst (funcall searcher pattern))
                             (not (eobp)))
                  for cand = (apply get-line-fn
                                    (if (and pos-lst (listp pos-lst))
                                        pos-lst
                                        (list (point-at-bol) (point-at-eol))))
                  when (and (not (gethash cand helm-cib-hash))
                            (or
                             ;; Always collect when cand is matched
                             ;; by searcher funcs and match-part attr
                             ;; is not present.
                             (and (not match-part-fn)
                                  (not (consp pos-lst)))
                             ;; If match-part attr is present, or if SEARCHER fn
                             ;; returns a cons cell, collect PATTERN only if it
                             ;; match the part of CAND specified by
                             ;; the match-part func.
                             (helm-search-match-part
                              cand pattern (or match-part-fn #'identity))))
                  do (helm--accumulate-candidates
                      cand newmatches helm-cib-hash item-count limit source))
         (setq matches (append matches (nreverse newmatches))))
       (delq nil matches)))))

(defun helm-search-match-part (candidate pattern match-part-fn)
  "Match PATTERN only on part of CANDIDATE returned by MATCH-PART-FN.
Because `helm-search-match-part' maybe called even if unspecified
in source (negation), MATCH-PART-FN default to `identity'
to match whole candidate.
When using fuzzy matching and negation (i.e \"!\"),
this function is always called."
  (let ((part (funcall match-part-fn candidate))
        (fuzzy-regexp (cadr (gethash 'helm-pattern helm--fuzzy-regexp-cache)))
        (matchfn (if helm-migemo-mode
                     'helm-mm-migemo-string-match 'string-match)))
    (if (string-match " " pattern)
        (cl-loop for i in (split-string pattern) always
                 (if (string-match "\\`!" i)
                     (not (funcall matchfn (substring i 1) part))
                     (funcall matchfn i part)))
        (if (string-match "\\`!" pattern)
            (if helm--in-fuzzy
                ;; Fuzzy regexp have already been
                ;; computed with substring 1.
                (not (string-match fuzzy-regexp part))
                (not (funcall matchfn (substring pattern 1) part)))
            (funcall matchfn (if helm--in-fuzzy fuzzy-regexp pattern) part)))))

(defun helm-initial-candidates-from-candidate-buffer (get-line-fn limit)
  (delq nil (cl-loop for i from 1 to limit
                     until (eobp)
                     collect (funcall get-line-fn
                                      (point-at-bol) (point-at-eol))
                     do (forward-line 1))))

(defun helm--search-from-candidate-buffer-1 (search-fn)
  ;; We are adding a newline at bob and at eol
  ;; and removing these newlines afterward.
  ;; This is a bad hack that should be removed.
  ;; To avoid matching the empty line at first line
  ;; when searching with e.g occur and "^$" just
  ;; forward-line before searching (See >>>[1] above).
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
                              (buffer-name helm-current-buffer)))
         (register-func
          (lambda ()
            (setq helm-candidate-buffer-alist
                  (cons (cons helm-source-name create-or-buffer)
                        (delete (assoc helm-source-name
                                       helm-candidate-buffer-alist)
                                helm-candidate-buffer-alist)))))
         (kill-buffers-func
          (lambda ()
            (cl-loop for b in (buffer-list)
                     if (string-match (format "^%s" (regexp-quote global-bname))
                                      (buffer-name b))
                     do (kill-buffer b))))
         (create-func
          (lambda ()
            (with-current-buffer
                (get-buffer-create (if (eq create-or-buffer 'global)
                                       global-bname
                                       local-bname))
              (set (make-local-variable 'inhibit-read-only) t) ; Fix (#1176)
              (buffer-disable-undo)
              (erase-buffer)
              (font-lock-mode -1))))
         (return-func
          (lambda ()
            (or (get-buffer local-bname)
                (get-buffer global-bname)
                (helm-aif (assoc-default helm-source-name
                                         helm-candidate-buffer-alist)
                    (and (buffer-live-p it) it))))))
    (when create-or-buffer
      (funcall register-func)
      (unless (bufferp create-or-buffer)
        (and (eq create-or-buffer 'global) (funcall kill-buffers-func))
        (funcall create-func)))
    (funcall return-func)))

(defun helm-init-candidates-in-buffer (buffer data)
  "Register BUFFER with DATA for a helm candidates-in-buffer session.
Arg BUFFER can be a string, a buffer object (bufferp), or a symbol,
either 'local or 'global which is passed to `helm-candidate-buffer'.
Arg DATA can be either a list or a plain string.
Returns the resulting buffer."
  (declare (indent 1))
  (let ((buf (helm-candidate-buffer
              (if (or (stringp buffer)
                      (bufferp buffer))
                  (get-buffer-create buffer)
                buffer)))) ; a symbol.
    (with-current-buffer buf
      (erase-buffer)
      (if (listp data)
          (insert (mapconcat (lambda (i)
                               (cond ((symbolp i) (symbol-name i))
                                     ((numberp i) (number-to-string i))
                                     (t i)))
                             data "\n"))
        (and (stringp data) (insert data))))
    buf))

(defun helm-compile-source--candidates-in-buffer (source)
  (helm-aif (assoc 'candidates-in-buffer source)
      (append source
              `((candidates . ,(or (cdr it)
                                   (lambda ()
                                     (helm-candidates-in-buffer source))))
                (volatile) (match identity)))
    source))


;;; Resplit helm window
;;
;;
(defun helm-toggle-resplit-window ()
  "Toggle resplit helm window, vertically or horizontally."
  (interactive)
  (when helm-prevent-escaping-from-minibuffer
    (helm-prevent-switching-other-window :enabled nil))
  (unwind-protect
       (with-helm-window
         (if (or helm-full-frame (one-window-p t))
             (message "Error: Attempt to resplit a single window")
           (let ((before-height (window-height)))
             (delete-window)
             (set-window-buffer
              (select-window
               (if (= (window-height) before-height) ; initial split was horizontal.
                   ;; Split window vertically with `helm-buffer' placed
                   ;; on the good side according to actual value of
                   ;; `helm-split-window-default-side'.
                   (prog1
                       (cond ((or (eq helm-split-window-default-side 'above)
                                  (eq helm-split-window-default-side 'left))
                              (split-window
                               (selected-window) nil 'above))
                             (t (split-window-vertically)))
                     (setq helm-split-window-state 'vertical))
                 ;; Split window vertically, same comment as above.
                 (setq helm-split-window-state 'horizontal)
                 (cond ((or (eq helm-split-window-default-side 'left)
                            (eq helm-split-window-default-side 'above))
                        (split-window (selected-window) nil 'left))
                       (t (split-window-horizontally)))))
              helm-buffer)))
         (setq helm--window-side-state (helm--get-window-side-state)))
    (when helm-prevent-escaping-from-minibuffer
      (helm-prevent-switching-other-window :enabled t))))

;; Utility: Resize helm window.
(defun helm-enlarge-window-1 (n)
  "Enlarge or narrow helm window.
If N is positive enlarge, if negative narrow."
  (unless helm-full-frame
    (let ((horizontal-p (eq helm-split-window-state 'horizontal)))
      (with-helm-window
        (enlarge-window n horizontal-p)))))

(defun helm-narrow-window ()
  "Narrow helm window."
  (interactive)
  (helm-enlarge-window-1 -1))

(defun helm-enlarge-window ()
  "Enlarge helm window."
  (interactive)
  (helm-enlarge-window-1 1))

(defun helm-swap-windows ()
  "Swap window holding `helm-buffer' with other window."
  (interactive)
  (if (and helm-full-frame (one-window-p t))
      (error "Error: Can't swap windows in a single window")
    (let* ((w1          (helm-window))
           (split-state (eq helm-split-window-state 'horizontal))
           (w1size      (window-total-size w1 split-state))
           (b1          (window-buffer w1)) ; helm-buffer
           (s1          (window-start w1))
           (cur-frame   (window-frame w1))
           (w2          (with-selected-window (helm-window)
                          ;; Don't try to display helm-buffer
                          ;; in a dedicated window.
                          (get-window-with-predicate
                           (lambda (w) (not (window-dedicated-p w)))
                           1 cur-frame)))
           (w2size      (window-total-size w2 split-state))
           (b2          (window-buffer w2)) ; probably helm-current-buffer
           (s2          (window-start w2))
           resize)
      (with-selected-frame (window-frame w1)
        (helm-replace-buffer-in-window w1 b1 b2)
        (helm-replace-buffer-in-window w2 b2 b1)
        (setq resize
              (cond ( ;; helm-window is smaller than other window.
                     (< w1size w2size)
                     (- (- (max w2size w1size)
                           (min w2size w1size))))
                    ( ;; helm-window is larger than other window.
                     (> w1size w2size)
                     (- (max w2size w1size)
                        (min w2size w1size)))
                    ( ;; windows have probably same size.
                     t nil)))
        ;; Maybe resize the window holding helm-buffer.
        (and resize (window-resize w2 resize split-state))
        (set-window-start w1 s2 t)
        (set-window-start w2 s1 t))
      (setq helm--window-side-state (helm--get-window-side-state)))))

(defun helm--get-window-side-state ()
  "Return the position of `helm-window' from `helm-current-buffer'.
Possible values are 'left 'right 'below or 'above."
  (let ((side-list '(left right below above)))
    (cl-loop for side in side-list
          thereis (and (equal (helm-window)
                              (window-in-direction
                               side (get-buffer-window helm-current-buffer t)
                               t))
                       side))))

(defun helm-replace-buffer-in-window (window buffer1 buffer2)
  "Replace BUFFER1 by BUFFER2 in WINDOW registering BUFFER1."
  (when (get-buffer-window buffer1)
    (unrecord-window-buffer window buffer1)
    (set-window-buffer window buffer2)))

;; Utility: select another action by key
(defun helm-select-nth-action (n)
  "Select the N nth action for the currently selected candidate."
  (setq helm-saved-selection (helm-get-selection))
  (unless helm-saved-selection
    (error "Nothing is selected"))
  (setq helm-saved-action
        (helm-get-nth-action
         n
         (if (get-buffer-window helm-action-buffer 'visible)
             (assoc-default 'candidates (helm-get-current-source))
             (helm-get-actions-from-current-source))))
  (helm-maybe-exit-minibuffer))

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

(defun helm-execute-selection-action-at-nth (linum)
  "Allow to execute default action on candidate at LINUM."
  (let ((prefarg current-prefix-arg))
    (if (>= linum 0)
        (helm-next-line linum)
        (helm-previous-line (lognot (1- linum))))
    (setq current-prefix-arg prefarg)
    (helm-exit-minibuffer)))

;; Utility: Persistent Action
(defmacro with-helm-display-same-window (&rest body)
  "Execute BODY in the window used for persistent action.
Make `pop-to-buffer' and `display-buffer' display in the same window."
  (declare (indent 0) (debug t))
  `(let ((display-buffer-function 'helm-persistent-action-display-buffer))
     ,@body))

(defun helm-initialize-persistent-action ()
  (set (make-local-variable 'helm-persistent-action-display-window) nil))

(cl-defun helm-execute-persistent-action
    (&optional (attr 'persistent-action) split-onewindow)
  "Perform the associated action ATTR without quitting helm.
ATTR default is 'persistent-action', but it can be anything else.
In this case you have to add this new attribute to your source.

When `helm-full-frame' or SPLIT-ONEWINDOW are non--nil,
and `helm-buffer' is displayed in only one window,
the helm window is splitted to display
`helm-select-persistent-action-window' in other window
and keep its visibility."
  (interactive)
  (helm-log "executing persistent-action")
  (let* ((attr-val (assoc-default attr (helm-get-current-source)))
         ;; If attr value is a cons, use its car as persistent function
         ;; and its car to decide if helm window should be splitted.
         (fn       (if (and (consp attr-val)
                            ;; maybe a lambda.
                            (not (functionp attr-val)))
                       (car attr-val) attr-val))
         (no-split (and (consp attr-val)
                        (not (functionp attr-val))
                        (cdr attr-val))))
    (with-helm-window
      (save-selected-window
        (if no-split
            (helm-select-persistent-action-window)
          (helm-select-persistent-action-window
           (or split-onewindow helm-onewindow-p)))
        (helm-log "current-buffer = %S" (current-buffer))
        (let ((helm-in-persistent-action t))
          (with-helm-display-same-window
            (helm-execute-selection-action-1
             nil (or fn (helm-get-actions-from-current-source)) t)
            (helm-log-run-hook 'helm-after-persistent-action-hook))
          ;; A typical case is when a persistent action delete
          ;; the buffer already displayed in
          ;; `helm-persistent-action-display-window' and `helm-full-frame'
          ;; is enabled, we end up with the `helm-buffer'
          ;; displayed in two windows.
          (when (and helm-onewindow-p
                     (> (length (window-list)) 1)
                     (equal (buffer-name
                             (window-buffer
                              helm-persistent-action-display-window))
                            (helm-buffer-get)))
            (delete-other-windows)))))))

(defun helm-persistent-action-display-window (&optional split-onewindow)
  "Return the window that will be used for persistent action.
If SPLIT-ONEWINDOW is non--nil window will be splitted in persistent action."
  (with-helm-window
    (setq helm-persistent-action-display-window
          (cond ((and (window-live-p helm-persistent-action-display-window)
                      (not (member helm-persistent-action-display-window
                                   (get-buffer-window-list helm-buffer))))
                 helm-persistent-action-display-window)
                (split-onewindow (split-window))
                ((get-buffer-window helm-current-buffer))
                (t (next-window (selected-window) 1))))))

(defun helm-select-persistent-action-window (&optional split-onewindow)
  "Select the window that will be used for persistent action.
See `helm-persistent-action-display-window' for how to use SPLIT-ONEWINDOW."
  (select-window (get-buffer-window (helm-buffer-get)))
  (select-window
   (setq minibuffer-scroll-window
         (helm-persistent-action-display-window split-onewindow))))

(defun helm-persistent-action-display-buffer (buf &optional  action)
  "Make `pop-to-buffer' and `display-buffer' display in the same window.
If `helm-persistent-action-use-special-display' is non-nil and
BUF is to be displayed by `special-display-function', use it.
Otherwise ignores `special-display-buffer-names' and `special-display-regexps'.
Argument ACTION if present will be used as second argument of `display-buffer'."
  (let* ((name (buffer-name buf))
         display-buffer-function pop-up-windows pop-up-frames
         ;; Disable `special-display-regexps' and `special-display-buffer-names'
         ;; unless `helm-persistent-action-use-special-display' is non--nil.
         (special-display-buffer-names
          (and helm-persistent-action-use-special-display
               special-display-buffer-names))
         (special-display-regexps
          (and helm-persistent-action-use-special-display
               special-display-regexps))
         (same-window-regexps
          (unless (and helm-persistent-action-use-special-display
                       (or (member name
                                   (mapcar (lambda (x) (or (car-safe x) x))
                                           special-display-buffer-names))
                           (cl-loop for x in special-display-regexps
                                 thereis (string-match (or (car-safe x) x)
                                                       name))))
            '("."))))
    ;; Don't loose minibuffer when displaying persistent window in
    ;; another frame.
    ;; This happen when the displayed persistent buffer-name is one of
    ;; `special-display-buffer-names' or match `special-display-regexps'
    ;; and `helm-persistent-action-use-special-display' is enabled.
    (with-selected-window (if (or special-display-regexps
                                  special-display-buffer-names)
                              (minibuffer-window)
                            (selected-window))
      ;; Be sure window of BUF is not dedicated.
      (set-window-dedicated-p (get-buffer-window buf) nil)
      (display-buffer buf action))))

;; scroll-other-window(-down)? for persistent-action
(defun helm-other-window-base (command &optional scroll-amount)
  (setq scroll-amount (unless (eq scroll-amount 'noscroll)
                        helm-scroll-amount))
  (with-selected-window (helm-persistent-action-display-window)
    (funcall command scroll-amount)))

(defun helm-scroll-other-window ()
  "Scroll other window (not *Helm* window) upward."
  (interactive)
  (helm-other-window-base 'scroll-up))

(defun helm-scroll-other-window-down ()
  "Scroll other window (not *Helm* window) downward."
  (interactive)
  (helm-other-window-base 'scroll-down))

(defun helm-recenter-top-bottom-other-window ()
  "`recenter-top-bottom' in other window (not *Helm* window)."
  (interactive)
  (helm-other-window-base 'recenter-top-bottom 'noscroll))

(defun helm-reposition-window-other-window ()
  "`helm-reposition-window' in other window (not *Helm* window)."
  (interactive)
  (helm-other-window-base 'reposition-window 'noscroll))



;; Utility: Visible Mark

(defun helm-clear-visible-mark ()
  (with-current-buffer (helm-buffer-get)
    (mapc 'delete-overlay helm-visible-mark-overlays)
    (set (make-local-variable 'helm-visible-mark-overlays) nil)))

(defun helm-this-visible-mark ()
  (cl-loop for o in helm-visible-mark-overlays
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
  (let ((o (make-overlay (point-at-bol)
                          (if (helm-pos-multiline-p)
                              (or (helm-get-next-candidate-separator-pos)
                                  (point-max))
                            (1+ (point-at-eol))))))
    (overlay-put o 'priority 0)
    (overlay-put o 'face   'helm-visible-mark)
    (overlay-put o 'source (assoc-default 'name (helm-get-current-source)))
    (overlay-put o 'string (buffer-substring (overlay-start o) (overlay-end o)))
    (overlay-put o 'real   (helm-get-selection))
    (add-to-list 'helm-visible-mark-overlays o))
  (push (cons (helm-get-current-source) (helm-get-selection))
        helm-marked-candidates))

(defun helm-toggle-visible-mark ()
  "Toggle helm visible mark at point."
  (interactive)
  (with-helm-window
    (let ((nomark (assq 'nomark (helm-get-current-source))))
      (if nomark
          (message "Marking not allowed in this source")
        (helm-aif (helm-this-visible-mark)
            (helm-delete-visible-mark it)
          (helm-make-visible-mark))
        (if (helm-end-of-source-p)
            (helm-display-mode-line (helm-get-current-source))
            (helm-next-line))))))

(defun helm-file-completion-source-p ()
  "Return non--nil if current source is a file completion source."
  (or minibuffer-completing-file-name
      (let ((cur-source (cdr (assoc 'name (helm-get-current-source)))))
        (cl-loop for i in helm--file-completion-sources
              thereis (string= cur-source i)))))

(defun helm-mark-all ()
  "Mark all visible unmarked candidates in current source."
  (interactive)
  (with-helm-window
    (let ((nomark (assq 'nomark (helm-get-current-source)))
          (follow (if helm-follow-mode 1 -1)))
      (helm-follow-mode -1)
      (unwind-protect
           (if nomark
               (message "Marking not allowed in this source")
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
                                         (helm-basename cand)))
                            (src-name    (assoc-default 'name
                                                   (helm-get-current-source))))
                       (when (and (not (helm-this-visible-mark))
                                  (not (or (string= prefix "[?]")
                                           (string= prefix "[@]"))))
                         ;; Don't mark possibles directories ending with . or ..
                         ;; autosave files/links and non--existent file.
                         (unless
                             (and (or (helm-file-completion-source-p)
                                      (string=
                                       src-name "Files from Current Directory"))
                                  (or (string-match
                                       "^[.]?#.*#?$\\|[^#]*[.]\\{1,2\\}$" bn)
                                      ;; We need to test here when not using
                                      ;; a transformer that tag prefix
                                      ;; (i.e on tramp).
                                      (not (file-exists-p cand))))
                           (helm-make-visible-mark))))
                     (if (helm-pos-multiline-p)
                         (progn
                           (goto-char
                            (or (helm-get-next-candidate-separator-pos)
                                (point-max)))
                           (forward-line 1))
                         (forward-line 1))
                     (end-of-line))))
               (helm-mark-current-line)
               (message "%s candidates marked" (length helm-marked-candidates)))
        (helm-follow-mode follow) (message nil)))))

(defun helm-unmark-all ()
  "Unmark all candidates in all sources of current helm session."
  (interactive)
  (with-helm-window
    (let ((len (length helm-marked-candidates)))
      (save-excursion
        (helm-clear-visible-mark))
      (setq helm-marked-candidates nil)
      (helm-mark-current-line)
      (helm-display-mode-line (helm-get-current-source))
      (message "%s candidates unmarked" len))))

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

(defun helm--compute-marked (real source wildcard)
  (let* ((coerced (helm-coerce-selection real source))
         (wilds   (and wildcard
                       (condition-case nil
                           (helm-file-expand-wildcards coerced t)
                         (error nil)))))
    ;; Avoid returning a not expanded wilcard fname.
    ;; e.g assuming "/tmp" doesn't contain "*.el"
    ;; return nil when coerced is "/tmp/*.el".
    (unless (or wilds (null wildcard)
                (string-match-p helm--url-regexp coerced)
                (file-exists-p coerced)
                (and (stringp coerced)
                     (null (string-match-p "[[*?]" coerced))))
      (setq coerced nil))
    (or wilds (and coerced (list coerced)))))

(cl-defun helm-marked-candidates (&key with-wildcard)
  "Return marked candidates of current source if any.
Otherwise one element list of current selection.
When key WITH-WILDCARD is specified try to expand a wilcard if some."
  (with-current-buffer helm-buffer
    (let ((candidates
           (cl-loop with current-src = (helm-get-current-source)
                    for (source . real) in (reverse helm-marked-candidates)
                    when (equal (assq 'name source) (assq 'name current-src))
                    append (helm--compute-marked real source with-wildcard) 
                    into cands
                    finally return (or cands
                                       (append
                                        (helm--compute-marked
                                         (helm-get-selection) current-src
                                         with-wildcard)
                                        cands)))))
      (helm-log "Marked candidates = %S" candidates)
      candidates)))

(defun helm-current-source-name= (name)
  (save-excursion
    (goto-char (helm-get-previous-header-pos))
    (equal name (helm-current-line-contents))))

(defun helm-revive-visible-mark ()
  "Restore marked candidates when helm update display."
  (with-current-buffer helm-buffer
    (save-excursion
      (cl-dolist (o helm-visible-mark-overlays)
        (goto-char (point-min))
        (while (and (search-forward (overlay-get o 'string) nil t)
                    (helm-current-source-name= (overlay-get o 'source)))
          ;; Calculate real value of candidate.
          ;; It can be nil if candidate have only a display value.
          (let ((real (get-text-property (point-at-bol 0) 'helm-realvalue)))
            (if real
                ;; Check if real value of current candidate is the same
                ;; than the one stored in overlay.
                ;; This is needed when some cands have same display names.
                ;; Using equal allow testing any type of value for real cand.
                ;; Issue (#706).
                (and (equal (overlay-get o 'real) real)
                     (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0))))
                (move-overlay o (point-at-bol 0) (1+ (point-at-eol 0))))))))))
(add-hook 'helm-update-hook 'helm-revive-visible-mark)

(defun helm-next-point-in-list (curpos points &optional prev)
  (cond
    ;; rule out special cases.
    ((null points) curpos)
    ((and prev (<= curpos (car points)))
     (nth (1- (length points)) points))
    ((< (car (last points)) curpos)
     (if prev (car (last points)) (nth 0 points)))
    ((and (not prev) (>= curpos (car (last points))))
     (nth 0 points))
    (t
     (nth (if prev
              (cl-loop for pt in points
                    for i from 0
                    if (<= curpos pt) return (1- i))
            (cl-loop for pt in points
                  for i from 0
                  if (< curpos pt) return i))
          points))))

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

(defun helm-prev-visible-mark ()
  "Move previous helm visible mark."
  (interactive)
  (helm-next-visible-mark t))

;;; Utility: Selection Paste
;;
(defun helm-yank-selection (arg)
  "Set minibuffer contents to current display selection.
With a prefix arg set to real value of current selection."
  (interactive "P")
  (let ((str (helm-get-selection nil (not arg))))
    (kill-new str)
    (helm-set-pattern str)))

(defun helm-kill-selection-and-quit (arg)
  "Store display value of current selection to kill ring.
With a prefix arg set to real value of current selection.
Display value is what you see in `helm-buffer' and real value
is what is used to perform actions."
  (interactive "P")
  (helm-run-after-exit
   (lambda (sel)
     (kill-new sel)
     ;; Return nil to force `helm-mode--keyboard-quit'
     ;; in `helm-comp-read' otherwise the value "Saved to kill-ring: foo"
     ;; is used as exit value for `helm-comp-read'.
     (prog1 nil (message "Saved to kill-ring: %s" sel) (sit-for 1)))
   (helm-get-selection nil (not arg))))

(defun helm-copy-to-buffer ()
  "Copy selection or marked candidates to `helm-current-buffer'."
  (interactive)
  (with-helm-buffer
    (cl-loop for cand in (helm-marked-candidates)
             do (with-helm-current-buffer
                  (insert (format "%s\n" cand))))))


;;; Follow-mode: Automatical execution of persistent-action
;;
;;
(defun helm-follow-mode (&optional arg)
  "Execute persistent action everytime the cursor is moved when enabled.
The mode is enabled for the current source only, you will have to turn it
on again when you go to next source if you want it there also.
This mode can be enabled or disabled interactively at anytime during
helm session or enabled specifically by source by adding the `follow'
attribute to this source.
Even when the attribute `follow' exists in source, it is still possible
to disable/enable this mode interactively.
Note that when you disable it interactively and `follow' attribute exists,
`helm-follow-mode' will be disabled on next helm session even if `follow'
attribute is specified in source. To avoid this set your `follow' attribute
in source in `helm-before-initialize-hook'.

e.g:

\(add-hook 'helm-before-initialize-hook
          (lambda () (helm-attrset 'follow 1 helm-source-buffers-list)))

This will enable `helm-follow-mode' automatically in `helm-source-buffers-list'."
  (interactive "p")
  (with-current-buffer helm-buffer
    (let* ((src      (helm-get-current-source))
           (name     (assoc-default 'name src))
           (sym      (cl-loop for s in helm-sources
                           for sname = (and (symbolp s)
                                            (assoc-default
                                             'name (symbol-value s)))
                           thereis (and sname (string= sname name) s)))
           (fol-attr (assq 'follow src))
           (enabled  (or
                      ;; If `helm-follow-mode' is called with a negative
                      ;; ARG, assume follow is already enabled.
                      ;; i.e turn it off now.
                      (< arg 0)
                      (eq (cdr fol-attr) 1)
                      helm-follow-mode)))
      (if src
          (progn
            (if (eq (cdr fol-attr) 'never)
                (message "helm-follow-mode not allowed in this source")
                ;; Make follow attr persistent for this emacs session.
                (helm-attrset 'follow (if enabled -1 1) src)
                (setq helm-follow-mode (not enabled))
                (message "helm-follow-mode is %s"
                         (if helm-follow-mode
                             "enabled" "disabled"))
                (helm-display-mode-line src t))
            (unless helm-follow-mode-persistent
              (and sym (set sym (remove (assq 'follow src) src)))))
          (message "Not enough candidates for helm-follow-mode")))))

(defvar helm-follow-input-idle-delay nil
  "`helm-follow-mode' will execute its persistent action after this delay.
Note that if the `follow-delay' attr is present in source,
it will take precedence on this.")
(defun helm-follow-execute-persistent-action-maybe ()
  "Execute persistent action in mode `helm-follow-mode'.
This happen after `helm-input-idle-delay' secs."
  (let ((src (helm-get-current-source)))
    (and (not (get-buffer-window helm-action-buffer 'visible))
         (eq (assoc-default 'follow src) 1)
         (sit-for (or (assoc-default 'follow-delay src)
                      helm-follow-input-idle-delay
                      (and helm-input-idle-delay
                           (max helm-input-idle-delay 0.01))))
         (helm-window)
         (helm-get-selection)
         (save-excursion
           (helm-execute-persistent-action)))))


;;; Auto-resize mode
;;
(defun helm--autoresize-hook (&optional max-height min-height)
  (with-helm-window
    (fit-window-to-buffer nil
                          (/ (* (frame-height)
                                (or max-height helm-autoresize-max-height))
                             100)
                          (/ (* (frame-height)
                                (or min-height helm-autoresize-min-height))
                             100))))

(define-minor-mode helm-autoresize-mode
    "Auto resize helm window when enabled.
Helm window is resized according to values of `helm-autoresize-max-height'
and `helm-autoresize-min-height'.
Note that when this mode is enabled, helm behave like when
`helm-always-two-windows' is enabled.

See `fit-window-to-buffer' for more infos."
  :group 'helm
  :global t
  (if helm-autoresize-mode
      (progn (add-hook 'helm-after-update-hook 'helm--autoresize-hook)
             (add-hook 'helm-window-configuration-hook 'helm--autoresize-hook))
      (remove-hook 'helm-after-update-hook 'helm--autoresize-hook)
      (remove-hook 'helm-window-configuration-hook 'helm--autoresize-hook)))

(defun helm-help ()
  "Help of `helm'."
  (interactive)
  (with-helm-alive-p
    (save-selected-window
      (helm-help-internal
       "*Helm Help*"
       (lambda ()
         (helm-aif (assoc-default 'help-message (helm-get-current-source))
             (insert (substitute-command-keys
                      (helm-interpret-value it)))
           (insert "* No specific help for this source at this time.\n
It may appear after first results popup in helm buffer."))
         (insert "\n\n"
                 (substitute-command-keys
                  (helm-interpret-value helm-help-message))))))))

(defun helm-toggle-truncate-line ()
  "Toggle `truncate-lines' value in `helm-buffer'"
  (interactive)
  (with-helm-alive-p
    (with-helm-buffer
      (setq truncate-lines (not truncate-lines))
      (helm-update (regexp-quote (helm-get-selection nil t))))))

(provide 'helm)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm.el ends here
