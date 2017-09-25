;; Excerpts of Hyperbole mouse support on [mouse-2] and [mouse-3] in a
;; single file for easy test loading.

;; Quiet byte compiler warnings for these free variables.
(eval-when-compile
  (defvar assist-flag nil)
  (defvar hkey-action nil)
  (defvar pred-value nil))

;; (require 'helm)

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defvar action-key-depressed-flag nil "t while Action Key is depressed.")
(defvar assist-key-depressed-flag nil "t while Assist Key is depressed.")
(defvar action-key-depress-args nil
  "List of mouse event args from most recent depress of the Action Key.")
(defvar assist-key-depress-args nil
  "List of mouse event args from most recent depress of the Assist Key.")

(defvar action-key-release-args nil
  "List of mouse event args from most recent release of the Action Key.")
(defvar assist-key-release-args nil
  "List of mouse event args from most recent release of the Assist Key.")

(defvar action-key-depress-window nil
  "The last window in which the Action Key was depressed or nil.")
(defvar assist-key-depress-window nil
  "The last window in which the Assist Key was depressed or nil.")
(defvar action-key-release-window nil
  "The last window in which the Action Key was released or nil.")
(defvar assist-key-release-window nil
  "The last window in which the Assist Key was released or nil.")

(defvar action-key-depress-prev-point nil
  "Marker at point prior to last Action Key depress.
Note that this may be a buffer different than where the depress occurs.")
(defvar assist-key-depress-prev-point nil
  "Marker at point prior to last Assist Key depress.
Note that this may be a buffer different than where the depress occurs.")
(defvar action-key-release-prev-point nil
  "Marker at point prior to last Action Key release.
Note that this may be a buffer different than where the release occurs.")
(defvar assist-key-release-prev-point nil
  "Marker at point prior to last Assist Key release.
Note that this may be a buffer different than where the release occurs.")

(defvar action-key-cancelled nil
  "When non-nil, cancels last Action Key depress.")
(defvar assist-key-cancelled nil
  "When non-nil, cancels last Assist Key depress.")

(defvar action-key-help-flag nil
  "When non-nil, forces display of help for next Action Key release.")
(defvar assist-key-help-flag nil
  "When non-nil, forces display of help for next Assist Key release.")

(defcustom hkey-debug nil
  "If non-nil, displays a message with the context and values from each Smart Key activation.
Default is nil."
  :type 'boolean
  :group 'hyperbole-commands)

(defvar hkey-region nil
  "Used to pass the value of a selected region between a Smart Key depress and release.
This permits the Smart Keys to behave as paste keys.")

(defvar hargs:reading-p nil
  "t only when Hyperbole is prompting user for input, else nil.")

(defvar hmouse-set-point-command 'hmouse-move-point-emacs
  "*Command that sets point to the mouse cursor position.")

(defun action-key-error ()
  (hypb:error "(Action Key): No action defined for this context; try another location."))
(defun assist-key-error ()
  (hypb:error "(Assist Key): No action defined for this context; try another location."))

(defcustom action-key-default-function #'action-key-error
  "*Function run by the Action Key in an unspecified context.
Set it to #'hyperbole if you want it to display the Hyperbole minibuffer menu."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-default-function #'assist-key-error
  "*Function run by the Assist Key in an unspecified context.
Set it to #'hkey-summarize if you want it to display a summary of Smart Key behavior."
  :type 'function
  :group 'hyperbole-keys)

(defcustom action-key-eol-function #'smart-scroll-up
  "*Function run by the Action Key at the end of a line.
Its default value is #'smart-scroll-up."
  :type 'function
  :group 'hyperbole-keys)

(defcustom assist-key-eol-function #'smart-scroll-down
  "*Function run by the Assist Key at the end of a line.
Its default value is #'smart-scroll-down."
  :type 'function
  :group 'hyperbole-keys)

;;; ************************************************************************
;;; Hyperbole context-sensitive keys dispatch table
;;; ************************************************************************

(defvar hkey-value nil
  "Communicates a value between a Smart Key predicate and its actions.")

(defvar hkey-alist nil
  "Alist of predicates and form-conses for the Action and Assist Keyboard Keys.
Each element is: (predicate-form . (action-key-form . assist-key-form)).
When the Action or Assist Key is pressed, the first or second form,
respectively, associated with the first non-nil predicate is evaluated.

See also `hmouse-alist' for a superset of this list utilized by the
Action and Assist Mouse Keys.")

(setq hkey-alist
  '(
    ;; Handle Emacs push buttons in buffers
    ((and (fboundp 'button-at) (button-at (point))) .
     ((push-button) . (smart-push-button-help)))
    ;;
    ;; If click in the minibuffer and reading an argument,
    ;; accept argument or give completion help.
    ((and (> (minibuffer-depth) 0)
	  (eq (selected-window) (minibuffer-window))
	  (not (eq hargs:reading-p 'hmenu))
	  (not (smart-helm-alive-p))) .
	  ((funcall (key-binding (kbd "RET"))) . (smart-completion-help)))
    ;;
    ;; If reading a Hyperbole menu item and nothing is selected, just return.
    ;; Or if in a helm session with point in the minibuffer, quit the
    ;; session and activate the selected item.
    ((and (> (minibuffer-depth) 0)
	  (eq (selected-window) (minibuffer-window))
	  (or (eq hargs:reading-p 'hmenu)
	      (smart-helm-alive-p))) .
	  ((funcall (key-binding (kbd "RET"))) . (funcall (key-binding (kbd "RET")))))
    ;;
    ;; If at the end of a line (eol), invoke the associated Smart Key handler EOL handler.
    ((smart-eolp) .
     ((funcall action-key-eol-function) . (funcall assist-key-eol-function)))
    ;;
    ;; Direct access selection of helm-major-mode completions
    ((setq hkey-value (and (or (eq major-mode 'helm-major-mode)
			       (and (featurep 'helm) (equal helm-action-buffer (buffer-name))))
			   (or (eolp)
			       (smart-helm-at-header)
			       (smart-helm-line-has-action)))) .
     ((smart-helm) . (smart-helm-assist)))
    ;;
    ((eq major-mode 'occur-mode) .
     ((occur-mode-goto-occurrence) . (occur-mode-goto-occurrence)))
    ;;
    ((eq major-mode 'moccur-mode) .
     ((moccur-mode-goto-occurrence) . (moccur-mode-goto-occurrence)))
    ((eq major-mode 'amoccur-mode) .
     ((amoccur-mode-goto-occurrence) . (amoccur-mode-goto-occurrence)))))

(defvar hmouse-alist nil
  "Alist of predicates and form-conses for the Action and Assist Mouse Keys.
When the Action Mouse Key or Assist Mouse Key is pressed, the first or second
form, respectively, associated with the first non-nil predicate is
evaluated.

The `hkey-alist' variable is the subset of this alist used by the
smart keyboard keys.")
(setq hmouse-alist hkey-alist)

(eval-when-compile (defvar assist-flag nil)) ;; Silences free variable compiler warnings

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar action-mouse-key-prev-window nil
  "Window point was in prior to current invocation of `action/assist-mouse-key'.")

(defvar action-mouse-key-prefix-arg nil
  "Prefix argument to pass to `smart-br-cmd-select'.")

(defvar hkey-help-msg "" "Holds last Smart Key help message.")
(defvar hkey--wconfig nil
  "Window configuration within current frame prior to display of a help buffer.")

;;; ************************************************************************
;;; Hyperbole context-sensitive key driver functions
;;; ************************************************************************

;;; Smart Key Depress Functions
(defun action-key-depress (&rest args)
  (interactive)
  (cond (assist-key-depressed-flag
	 (or action-key-help-flag
	     (setq assist-key-help-flag t)))
	((hmouse-save-region)))
  (setq action-key-depress-prev-point (point-marker)
	action-key-depressed-flag t
	action-key-depress-args (hmouse-set-point args)
	action-key-depress-window (or (hmouse-depress-inactive-minibuffer-p args)
				      (selected-window))
	action-key-release-args nil
	action-key-release-window nil
	action-key-release-prev-point nil))

(defun assist-key-depress (&rest args)
  (interactive)
  (cond (action-key-depressed-flag
	 (or assist-key-help-flag
	     (setq action-key-help-flag t)))
	((hmouse-save-region)))
  (setq assist-key-depress-prev-point (point-marker)
	assist-key-depressed-flag t
	assist-key-depress-args (hmouse-set-point args)
	assist-key-depress-window (or (hmouse-depress-inactive-minibuffer-p args)
				      (selected-window))
	assist-key-release-args nil
	assist-key-release-window nil
	assist-key-release-prev-point nil))

(defun action-key-depress-emacs (event)
  (interactive "e")
  (action-key-depress event))

(defun assist-key-depress-emacs (event)
  (interactive "e")
  (assist-key-depress event))

;;; Smart Key Release Functions
(defun action-mouse-key-emacs (event)
  "Set point to the current mouse cursor position and execute 'action-key'.
EVENT will be passed to 'hmouse-function'."
  (interactive "e")
  (apply #'action-mouse-key (hmouse-key-release-args-emacs event)))

(defun assist-mouse-key-emacs (event)
  "Set point to the current mouse cursor position and execute 'action-key'.
EVENT will be passed to 'hmouse-function'."
  (interactive "e")
  (apply #'assist-mouse-key (hmouse-key-release-args-emacs event)))

(defun action-mouse-key (&rest args)
  "Set point to the current mouse cursor position and execute `action-key'.
Any ARGS will be passed to `hmouse-function'."
  (interactive)
  ;; Make this a no-op if some local mouse key binding overrode the global
  ;; action-key-depress command invocation.
  (if action-key-depressed-flag
      (let ((hkey-alist hmouse-alist))
	(setq action-key-depressed-flag nil)
	(cond (action-key-cancelled
		(setq action-key-cancelled nil
		      assist-key-depressed-flag nil))
	      (assist-key-depressed-flag
		(hmouse-function nil nil args))
	      ((hkey-mouse-help nil args))
	      (t (hmouse-function #'action-key-internal nil args)))
	;; Need to clear these variables so that mouse pasting does
	;; not occur repeatedly from a single region selection.
	(setq hkey-region nil
	      hkey-value nil))))

(defun assist-mouse-key (&rest args)
  "Set point to the current mouse cursor position and execute `assist-key'.
Any ARGS will be passed to `hmouse-function'."
  (interactive)
  ;; Make this a no-op if some local mouse key binding overrode the global
  ;; assist-key-depress command invocation.
  (if assist-key-depressed-flag
      (let ((hkey-alist hmouse-alist))
	(setq assist-key-depressed-flag nil)
	(cond (assist-key-cancelled
		(setq assist-key-cancelled nil
		      action-key-depressed-flag nil))
	      (action-key-depressed-flag
		(hmouse-function nil t args))
	      ((hkey-mouse-help t args))
	      (t (hmouse-function #'assist-key-internal t args)))
	;; Need to clear this variable so that mouse pasting does
	;; not occur repeatedly from a single region selection.
	(setq hkey-region nil
	      hkey-value nil))))

;;; Smart Key Commands
(defun action-key ()
  "Use one key to perform functions that vary by context.
If no matching context is found, the default function set with
the `action-key-default-function' variable is run.  Returns t
unless the `action-key-default-function' variable is not bound to
a valid function."
  (interactive)
  ;; Clear all these variables so there can be no confusion between
  ;; mouse presses and keyboard presses.
  (setq action-key-depress-prev-point nil
	action-key-depress-args nil
	action-key-depress-window nil
	action-key-release-args nil
	action-key-release-window nil
	action-key-release-prev-point nil)
  (action-key-internal))

(defun action-key-internal ()
  (setq action-key-depressed-flag nil)
  (if action-key-cancelled
      (setq action-key-cancelled nil
	    assist-key-depressed-flag nil))
  (or (hkey-execute nil)
      (when (fboundp action-key-default-function)
	(funcall action-key-default-function)
	t)))

(defun assist-key ()
  "Use one key to perform functions that vary by context.
If no matching context is found, the default function set with
the `assist-key-default-function' variable is run.  Returns
non-nil unless `assist-key-default-function' variable is not
bound to a valid function."
  (interactive)
  ;; Clear all these variables so there can be no confusion between
  ;; mouse presses and keyboard presses.
  (setq assist-key-depress-prev-point nil
	assist-key-depress-args nil
	assist-key-depress-window nil
	assist-key-release-args nil
	assist-key-release-window nil
	assist-key-release-prev-point nil)
  (assist-key-internal))

(defun assist-key-internal ()
  (setq assist-key-depressed-flag nil)
  (if assist-key-cancelled
      (setq assist-key-cancelled nil
	    action-key-depressed-flag nil))
  (or (hkey-execute t)
      (when (fboundp assist-key-default-function)
	(funcall assist-key-default-function)
	t)))

(defun hkey-either (arg)
  "Executes `action-key' or with non-nil ARG executes `assist-key'."
  (interactive "P")
  (if arg (assist-key) (action-key)))

;;; ************************************************************************
;;; Public support functions
;;; ************************************************************************

(defun hkey-execute (assist-flag)
  "Evaluate Action Key form (or Assist Key form with ASSIST-FLAG non-nil) for first non-nil predicate from `hkey-alist'.
Non-nil ASSIST-FLAG means evaluate second form, otherwise evaluate first form.
Returns non-nil iff a non-nil predicate is found."
  ;; Keep in mind that hkey-alist may be set to hmouse-alist here, with additional predicates.
  (let ((hkey-forms hkey-alist)
	(pred-value) (hkey-action) hkey-form pred)
    (while (and (null pred-value) (setq hkey-form (car hkey-forms)))
      (if (setq hkey-action (if assist-flag (cdr (cdr hkey-form)) (car (cdr hkey-form)))
		pred (car hkey-form)
		pred-value (eval pred))
	  (progn (eval hkey-action))
	(setq hkey-forms (cdr hkey-forms))))
    pred-value))

(defconst hypb:help-buf-prefix "*Help: Hyperbole "
  "Prefix attached to all native Hyperbole help buffer names.
This should end with a space.")

(defun hypb:help-buf-name (&optional suffix)
  "Returns a Hyperbole help buffer name for current buffer.
With optional SUFFIX string, uses it rather than buffer name."
  (let ((bn (or suffix (buffer-name))))
    (if (string-match (regexp-quote hypb:help-buf-prefix) bn)
	(buffer-name (generate-new-buffer bn))
      (concat hypb:help-buf-prefix bn "*"))))

(defun    hattr:report (attrib-list)
  "Pretty prints to standard-output attribute-value pairs from ATTRIB-LIST.
Ignores nil valued attributes.  Returns t unless no attributes are printed."
  (let ((has-attr) attr val len)
    (unless (or (null attrib-list) (not (listp attrib-list))
		;; odd number of elements?
		(= (% (length attrib-list) 2) 1))
      (while (setq attr (car attrib-list))
	(setq val (car (cdr attrib-list))
	      attrib-list (cdr (cdr attrib-list)))
	(when val
	  (setq has-attr t
		attr (symbol-name attr)
		len (number-to-string (max (- 16 (length attr)) 1)))
	  (princ (format (concat "   %s:%" len "s%S\n") attr " "
			 (let (str)
			   (cond ((string-match "time" attr)
				  "GMT")
				 ((and (setq str (if (stringp val) val
						   (prin1-to-string val)))
				       (string-match "\\`actypes::" str))
				  (intern (substring str (match-end 0))))
				 (t val)))))))
      has-attr)))

(defun    hattr:list (obj-symbol)
  "Returns a property list of OBJ-SYMBOL's attributes.
Each pair of elements is: <attrib-name> <attrib-value>."
  (if (symbolp obj-symbol)
      (symbol-plist obj-symbol)
    (error "(hattr:list): Argument not a symbol: %s" obj-symbol)))

(defvar   hbut:current nil
  "The currently selected Hyperbole button.  Available to action routines.")

(defun hkey-help (&optional assist-flag)
  "Display help for the Action Key command in current context.
With optional ASSIST-FLAG non-nil, display help for the Assist Key command.
Returns non-nil iff associated help documentation is found."
  (interactive "P")
  (let ((hkey-forms hkey-alist)
	(hkey-form) (pred-value) (call) (cmd-sym) (doc))
    (while (and (null pred-value) (setq hkey-form (car hkey-forms)))
      (or (setq pred-value (eval (car hkey-form)))
	  (setq hkey-forms (cdr hkey-forms))))
    (if pred-value
	(setq call (if assist-flag (cdr (cdr hkey-form))
		     (car (cdr hkey-form)))
	      cmd-sym (car call))
      (setq cmd-sym
	    (if assist-flag assist-key-default-function action-key-default-function)
	    call cmd-sym))
    (setq hkey-help-msg
	  (if (and cmd-sym (symbolp cmd-sym))
	      (progn
		(setq doc (documentation cmd-sym))
		(let* ((condition (car hkey-form))
		       (temp-buffer-show-hook
			 (lambda (buf)
			   (set-buffer buf)
			   (help-mode)
			   (let ((owind (selected-window)))
			     (display-buffer buf 'other-win)
			     (if (or (and (boundp 'help-window-select)
					  help-window-select)
				     (and (boundp 'help-selects-help-window)
					  help-selects-help-window))
				 (select-window (get-buffer-window buf))
			       (select-window owind)))))
		       (temp-buffer-show-function temp-buffer-show-hook))
		  (with-output-to-temp-buffer
		      (hypb:help-buf-name
		       (format "%s Key" (if assist-flag "Assist" "Action")))
		    (princ (format "A click of the %s Key"
				   (if assist-flag "Assist" "Action")))
		    (terpri)
		    (princ "WHEN  ")
		    (princ
		      (or condition
			  "there is no matching context"))
		    (terpri)
		    (princ "CALLS ") (princ call)
		    (if doc (progn (princ " WHICH:") (terpri) (terpri)
				   (princ doc)))
		    (if (memq cmd-sym '(hui:hbut-act hui:hbut-help))
			(progn
			  (princ (format "\n\nBUTTON SPECIFICS:\n\n%s\n" ""))
			  (hattr:report
			    (nthcdr 2 (hattr:list 'hbut:current)))))
		    (terpri)
		    ))
		"")
	    (message "No %s Key command for current context."
		     (if assist-flag "Assist" "Action"))))
    doc))

(defun hkey-assist-help ()
  "Display doc associated with Assist Key command in current context.
Returns non-nil iff associated documentation is found."
  (interactive)
  (hkey-help 'assist))

;;;###autoload
(defun hkey-help-hide (&optional kill window)
  "Optionally KILLs current buffer (default is bury) and quits WINDOW.
Restores frame to configuration prior to help buffer display.
Point must be in a help buffer.  See `hkey-quit-window' for additional
details."
  (interactive "P")
  (let ((buf (current-buffer)))
    (if (window-configuration-p hkey--wconfig)
	(progn (set-window-configuration hkey--wconfig)
	       (if kill (kill-buffer buf)
		 (bury-buffer buf)))
      (quit-window kill window)))
  (setq hkey--wconfig nil))

(defalias 'quit-window 'hkey-help-hide)

;; Newer versions of Emacs define this variable but older versions,
;; e.g. Emacs 22, do not.  Calls to the `with-help-buffer' macro
;; compiled in Emacs 25 will fail without this, so conditionally
;; define it here.
(unless (boundp 'help-window-point-marker)
  (defvar help-window-point-marker (make-marker)
    "Marker to override default `window-point' in help windows."))

;;;###autoload
(defun hkey-help-show (&optional buffer current-window)
  "Saves prior window configuration if BUFFER displays help.  Displays BUFFER.

Optional second arg CURRENT-WINDOW non-nil forces display of buffer within
the current window."
  (if (bufferp buffer) (setq buffer (buffer-name buffer)))
  (if (null buffer) (setq buffer (buffer-name (current-buffer))))
  (and (stringp buffer)
       (string-match "^\\*Help\\|Help\\*$" buffer)
       (not (memq t (mapcar (lambda (wind)
			      (string-match
			       "^\\*Help\\|Help\\*$"
			       (buffer-name (window-buffer wind))))
			    (window-list nil 'no-mini))))
       (setq hkey--wconfig (current-window-configuration)))
  (unwind-protect
      (let* ((buf (get-buffer-create buffer))
	     ;; Help-mode calls with-temp-buffer which invokes one of these hooks
	     ;; which calls hkey-help-show again, so nullify them before
	     ;; displaying the buffer.
	     (temp-buffer-show-hook)
	     (temp-buffer-show-function)
	     (wind (cond (current-window
			  (switch-to-buffer buf)
			  (selected-window))
			 (t (display-buffer buf)))))
	(when wind
	  (setq minibuffer-scroll-window wind)
	  ;; Don't use help-mode in buffers already set up with a
	  ;; quit-key to bury the buffer, e.g. minibuffer completions,
	  ;; as this will sometimes disable default left mouse key item
	  ;; selection.
	  (unless (or (where-is-internal 'quit-window (current-local-map))
		      (where-is-internal 'hkey-help-hide (current-local-map)))
	    (if (string-match "^\\*Help\\|Help\\*$" (buffer-name))
		(help-mode))
	    (local-set-key "q" #'hkey-help-hide))))
    ;; If in a *Completions* buffer, re-select the window that
    ;; generated the completions.
    (if (buffer-live-p completion-reference-buffer)
	(select-window (get-buffer-window completion-reference-buffer t)))))

(defun hkey-mouse-help (assist-flag args)
  "If a Smart Key help flag is set and the other Smart Key is not down, shows help.
Takes two args:  ASSIST-FLAG should be non-nil iff command applies to the Assist Key.
ARGS is a list of arguments passed to `hmouse-function'.
Returns t if help is displayed, nil otherwise."
  (let ((help-shown)
	(other-key-released (not (if assist-flag
				     action-key-depressed-flag
				   assist-key-depressed-flag))))
    (unwind-protect
	(setq help-shown
	      (cond ((and  action-key-help-flag other-key-released)
		     (setq action-key-help-flag nil)
		     (hmouse-function #'hkey-help assist-flag args)
		     t)
		    ((and  assist-key-help-flag other-key-released)
		     (setq assist-key-help-flag nil)
		     (hmouse-function #'hkey-assist-help assist-flag args)
		     t)))
      (when help-shown
	  ;; Then both Smart Keys have been released. 
	(setq action-key-cancelled nil
	      assist-key-cancelled nil)
	t))))

(defun hypb:error (&rest args)
  "Signals an error typically to be caught by `hyperbole'."
  (let ((msg (apply 'format args)))
    (put 'error 'error-message msg)
    (error msg)))

(defun hyperb:window-system (&optional frame)
  "Returns the string name for window system or term type under which the selected frame is running.
If nil after system initialization, no window system or mouse support is available."
  (unless frame (setq frame (selected-frame)))
  (if window-system (symbol-name window-system)))

(defun hkey-operate (&optional arg)
  "Uses the keyboard to emulate Smart Mouse Key drag actions.
Each invocation alternates between starting a drag and ending it.
Optional prefix ARG non-nil means emulate Assist Key rather than the
Action Key.

Only works when running under a window system, not from a dumb terminal."
  (interactive "P")
  (or (hyperb:window-system)
      (hypb:error "(hkey-operate): Drag actions require mouse support"))
  (if arg
      (if assist-key-depressed-flag
	  (progn (assist-mouse-key)
		 (message "Assist Key released."))
	(assist-key-depress)
	(message
	  "Assist Key depressed; go to release point and hit {%s %s}."
	  (substitute-command-keys "\\[universal-argument]")
	  (substitute-command-keys "\\[hkey-operate]")
	  ))
    (if action-key-depressed-flag
	(progn (action-mouse-key)
	       (message "Action Key released."))
      (action-key-depress)
      (message "Action Key depressed; go to release point and hit {%s}."
	       (substitute-command-keys "\\[hkey-operate]"))
      )))

(defun hmouse-depress-inactive-minibuffer-p (args)
  "Return the minibuffer window if the last Smart Mouse Key depress was in it and it was inactive, else nil."
  (if (= (minibuffer-depth) 0)
      (if (eq (minibuffer-window) (posn-window (event-start args)))
	  (minibuffer-window))))

(defun hmouse-key-release-args-emacs (event)
  (if (integerp event)
      (list event)
    (let ((ev-type-str (and (listp event) (symbol-name (car event)))))
      (if (or (and ev-type-str
		   (string-match "\\(double\\|triple\\)-mouse" ev-type-str))
	      (not (= (length event) 3)))
	  event
	;; Remove depress coordinates and send only release coordinates.
	(list (car event) (nth 2 event))))))

(defun hmouse-save-region (&optional frame)
  "Saves any active region within the current buffer.
Under InfoDock and XEmacs, `zmacs-region' must be t; under GNU Emacs,
`transient-mark-mode' must be t or the function does nothing."
  (if (cond
       ;; Newer GNU Emacs
       ((fboundp 'use-region-p)
	(let ((use-empty-active-region))
	  (use-region-p)))
       ;; InfoDock and XEmacs
       ((fboundp 'region-exists-p)
	(and (fboundp 'region-active-p) (region-active-p) (region-exists-p)))
       ;; Older GNU Emacs
       ((boundp 'transient-mark-mode)
	(and transient-mark-mode mark-active)))
      (setq hkey-region (buffer-substring (region-beginning) (region-end)))
    (setq hkey-region nil)))

;; Based on functions from Emacs mouse.el.
(defun hmouse-move-point-emacs (event &optional promote-to-region)
  "Move point to the position clicked on with the mouse.
This should be bound to a mouse click event type.
If PROMOTE-TO-REGION is non-nil and event is a multiple-click,
select the corresponding element around point."
  (interactive "e\np")
  (let ((start-w-or-f (posn-window (event-start event)))
	(end-w-or-f   (posn-window (event-end event))))
    (if (framep start-w-or-f)
	(with-selected-frame start-w-or-f (setq start-w-or-f (selected-window))))
    (if (framep end-w-or-f)
	(with-selected-frame end-w-or-f (setq end-w-or-f (selected-window))))
    (if (and (window-minibuffer-p start-w-or-f)
	     (not (minibuffer-window-active-p start-w-or-f)))
	;; Select the ending frame only, not the window pressed within.
	(select-frame (window-frame end-w-or-f))
      ;; Give temporary modes such as isearch a chance to turn off.
      (run-hooks 'mouse-leave-buffer-hook)
      (if (and promote-to-region (> (event-click-count event) 1))
	  (mouse-set-region event)
	;; Use event-end in case called from mouse-drag-region.
	;; If EVENT is a click, event-end and event-start give same value.
	(if (and (window-minibuffer-p end-w-or-f)
		 (not (minibuffer-window-active-p end-w-or-f)))
	    ;; Select the ending frame only, not the window pressed within.
	    (select-frame (window-frame end-w-or-f))
	  (condition-case ()
	      (posn-set-point (event-end event))
	    (error (select-frame (window-frame end-w-or-f)))))))))

;; BW - Last confirmed in 1999, for some reason, using this next
;; function in byte-compiled form caused the first character 
;; after a mouse key depress to be dropped from the input queue when running
;; Emacs under X.  The non-byte-compiled form always worked fine.  We
;; assume this is no longer a problem in 2016 but have this note here
;; in case it is.
(defun hmouse-set-point (args)
  "Sets point to Smart Key press/release location given by ARGS.
Returns argument list including x and y frame coordinates in characters and
lines or if ARGS is null and there is no graphical window system,
return current point as a marker."
  (and (car args) (listp (car args)) (setq args (car args)))
  (if (and args (hyperb:window-system))
      (progn (hmouse-set-point-at args)
	     (cond ((featurep 'xemacs)
		    (if (eventp current-mouse-event)
			(copy-event current-mouse-event)))
		   ((equal (hyperb:window-system) "next")
		    (let ((win (car args)))
		      (list win
			    (+ (nth 1 args) (nth 0 (window-edges win)))
			    (+ (nth 2 args) (nth 1 (window-edges win))))))
		   (t args)))
    (point-marker)))

(defun hmouse-set-point-at (set-point-arg-list)
  "Sets point to cursor position using SET-POINT-ARG-LIST and returns t.
If 'hmouse-set-point-command' is not bound to a function, this does nothing
and returns nil."
  (if (fboundp hmouse-set-point-command)
      (or (if set-point-arg-list
	      (funcall hmouse-set-point-command set-point-arg-list)
	    (funcall hmouse-set-point-command))
	  t)))

;; "hsettings.el" contains documentation for this variable.
(or (boundp 'smart-scroll-proportional)
    (defvar smart-scroll-proportional t
      "*Non-nil means Smart Keys should scroll relative to current line when pressed at the end of a line.
Action Key moves current line to top of window.  Assist Key moves current
line to bottom of window.  Repeated presses then scroll up or down a
windowful.  Nil value instead ignores current line and always scrolls up or
down a windowful."))

;; The smart keys scroll buffers when pressed at the ends of lines.
;; These next two functions do the scrolling and keep point at the end
;; of line to simplify repeated scrolls when using keyboard smart keys.
;;
;; These functions may also be used to test whether the scroll action would
;; be successful, no action is taken if it would fail (because the beginning
;; or end of a buffer is already showing) and nil is returned.
;; t is returned whenever scrolling is performed.

(defun hmouse-function (func assist-flag set-point-arg-list)
  "Executes FUNC for Action Key (Assist Key with ASSIST-FLAG non-nil) and sets point from SET-POINT-ARG-LIST.
FUNC may be nil in which case no function is called.
SET-POINT-ARG-LIST is passed to the call of the command bound to
`hmouse-set-point-command'.  Returns nil if `hmouse-set-point-command' variable
is not bound to a valid function."
  (if (fboundp hmouse-set-point-command)
      (let ((release-args (hmouse-set-point set-point-arg-list)))
	(if assist-flag
	    (setq assist-key-release-window (selected-window)
		  assist-key-release-args release-args
		  assist-key-release-prev-point (point-marker))
	  (setq action-key-release-window (selected-window)
		action-key-release-args release-args
		action-key-release-prev-point (point-marker)))
	(and (eq major-mode 'br-mode)
	     (setq action-mouse-key-prev-window (selected-window)))
	(setq action-mouse-key-prefix-arg current-prefix-arg)
	(when func
	  (funcall func)
	  (setq action-mouse-key-prev-window nil
		action-mouse-key-prefix-arg nil))
	t)))

(defun smart-scroll-down ()
  "Scrolls down according to value of smart-scroll-proportional.
If smart-scroll-proportional is nil or if point is on the bottom window line,
scrolls down (backward) a windowful.  Otherwise, tries to bring current line
to bottom of window.  Leaves point at end of line and returns t if scrolled,
nil if not."
  (interactive)
  (let ((rtn t))
    (if smart-scroll-proportional
	;; If selected line is already last in window, then scroll backward
	;; a windowful, otherwise make it last in window.
	(if (>= (point) (save-excursion
			  (goto-char (1- (window-end)))
			  (beginning-of-line) (point)))
	    (if (pos-visible-in-window-p (point-min))
		(setq rtn nil)
	      (scroll-down))
	  (recenter -1))
      (if (pos-visible-in-window-p (point-min))
	  (setq rtn nil)
	(scroll-down)))
    (end-of-line)
    (or rtn (progn (beep) (message "Beginning of buffer")))
    rtn))

(defun smart-scroll-up ()
  "Scrolls up according to value of smart-scroll-proportional.
If smart-scroll-proportional is nil or if point is on the top window line,
scrolls up (forward) a windowful.  Otherwise, tries to bring current line to
top of window.  Leaves point at end of line and returns t if scrolled, nil if
not."
  (interactive)
  (let ((rtn t))
    (if smart-scroll-proportional
	;; If selected line is already first in window, then scroll forward a
	;; windowful, otherwise make it first in window.
	(if (<= (point) (save-excursion
			  (goto-char (window-start))
			  (end-of-line) (point)))
	    (if (pos-visible-in-window-p (point-max))
		(setq rtn nil)
	      (scroll-up))
	  (recenter 0))
      (if (pos-visible-in-window-p (point-max))
	  (setq rtn nil)
	(scroll-up)))
    (end-of-line)
    (or rtn (progn (beep) (message "End of buffer")))
    rtn))

(defun smart-outline-char-invisible-p (&optional pos)
  "Return t if the character after point is invisible/hidden, else nil."
  (or pos (setq pos (point)))
  (when (or
	 ;; New-style Emacs outlines with invisible properties to hide lines
	 (kproperty:get pos 'invisible)
	 (delq nil (mapcar (lambda (o) (overlay-get o 'invisible))
			   (overlays-at (or pos (point)))))
	 ;; Old-style Emacs outlines using \r (^M) characters to hide lines
	 (and selective-display (eq (following-char) ?\r)))
    t))

(defun smart-eolp ()
  "Return t if point is at the end of a visible line but not the end of the buffer."
  ;; smart-helm handles eol for helm buffers
  (unless (and (smart-helm-alive-p) (equal (helm-buffer-get) (buffer-name)))
    (and (not (eobp)) (eolp) (or (not (smart-outline-char-invisible-p))
				 (not (smart-outline-char-invisible-p (1- (point))))))))

(provide 'hmouse-drv)

;;; ************************************************************************
;;; smart-helm functions
;;; ************************************************************************

(defun smart-helm-at-header ()
  "Return t iff Action Mouse Key depress was on the first fixed header line or a helm section header of the current buffer."
  (or (helm-pos-header-line-p)
      (and (eventp action-key-depress-args)
	   (eq (posn-area (event-start action-key-depress-args))
	       'header-line))))

(defun smart-helm-line-has-action ()
  "Marks and returns the actions for the helm selection item at point, or nil if line lacks any action.
Assumes Hyperbole has already checked that helm is active."
  (let ((helm-buffer (if (equal helm-action-buffer (buffer-name)) helm-buffer (buffer-name))))
    (save-excursion
      (with-helm-buffer
	(if hkey-debug (setq cursor-type t)) ; For testing where mouse presses set point.
	(when (not (or (eobp)
		       (smart-helm-at-header)
		       (helm-pos-candidate-separator-p)))
	  (helm-mark-current-line)
	  (helm-get-current-action))))))

(defun smart-helm-alive-p ()
  ;; Handles case where helm-action-buffer is visible but helm-buffer
  ;; is not; fixed in helm with commit gh#emacs-helm/helm/cc15f73.
  (and (featurep 'helm)
       helm-alive-p
       (window-live-p (helm-window))
       (minibuffer-window-active-p (minibuffer-window))))

(defun smart-helm-resume-helm ()
  "Resumes helm session for the current buffer if not already active."
  (unless (smart-helm-alive-p)
    (unless (equal helm-action-buffer (buffer-name))
      ;; helm-resume doesn't seem to set this properly.
      (setq helm-buffer (buffer-name)))
    (helm-resume helm-buffer)
    (sit-for 0.2)))

(defun smart-helm-at (depress-event)
  "Return non-nil iff Smart Mouse DEPRESS-EVENT was on a helm section header, candidate separator or at eob or eol.
If non-nil, returns a property list of the form: (section-header <bool> separator <bool> eob <bool> or eol <bool>).
If a section-header or separator, selects the first following candidate line.
Assumes Hyperbole has already checked that helm is active."
  (and (eventp depress-event)
       ;; Nil means in the buffer text area
       (not (posn-area (event-start depress-event)))
       (with-helm-buffer
	 (let ((opoint (point))
	       things)
	   (mouse-set-point depress-event)
	   (setq things (list 'section-header (helm-pos-header-line-p)
			      'separator (helm-pos-candidate-separator-p)
			      'eob (eobp)
			      'eol (eolp)))
	   (cond ((or (plist-get things 'section-header) (plist-get things 'separator))
		  (helm-next-line 1)
		  things)
		 ((plist-get things 'eol)
		  (helm-mark-current-line)
		  things)
		 ((plist-get things 'eob)
		  things)
		 (t
		  (goto-char opoint)
		  nil))))))

(defun smart-helm()
  "Executes helm actions based on Action Key click locations:
  At the end of the buffer, quits from helm and exits the minibuffer.
  On a candidate line, performs the candidate's first action and remains in the minibuffer;
  On the top, fixed header line, toggles display of the selected candidate's possible actions;
  On an action list line, performs the action after exiting the minibuffer;
  On a source section header, moves to the next source section or first if on last.
  On a candidate separator line, does nothing.
  In the minibuffer window, ends the helm session and performs the selected item's action."
  (unless (hmouse-check-action-key)
    (error "(smart-helm): Hyperbole Action Mouse Key - either depress or release event is improperly configured"))
  (let* ((non-text-area-p (and (eventp action-key-depress-args)
			       (posn-area (event-start action-key-depress-args))))
	 (at-plist (smart-helm-at action-key-depress-args))
	 (section-hdr (plist-get at-plist 'section-header))
	 (separator (plist-get at-plist 'separator))
	 (eob (plist-get at-plist 'eob))
	 (eol (plist-get at-plist 'eol)))
    (smart-helm-resume-helm)
    ;; Handle end-of-line clicks.
    (if (and eol (not eob) (not non-text-area-p))
	(progn (with-helm-buffer (funcall action-key-eol-function))
	       (if (> (minibuffer-depth) 0)
		   (select-window (minibuffer-window))))
      (if (> (minibuffer-depth) 0)
	  (select-window (minibuffer-window)))
      (when (and (smart-helm-alive-p) (not separator))
	(let* ((key (kbd (cond
			  ;; Exit
			  (eob "C-g")
			  ;; Move to next source section or first if on last.
			  (section-hdr "C-o")
			  ;; If line of the key press is the first /
			  ;; header line in the window or outside the
			  ;; buffer area, then use {TAB} command to
			  ;; switch between match list and action list.
			  (non-text-area-p "TAB")
			  ;; RET: Performs action of selection and exits the minibuffer.
			  ;; C-j: Performs action of selection and stays in minibuffer.
			  (hkey-value
			   (if (helm-action-window) "RET" "C-j"))
			  (t "RET"))))
	       (binding (key-binding key)))
	  (if hkey-debug
	      (message "(HyDebug): In smart-helm, key to execute is: {%s}; binding is: %s"
		       (key-description key) binding))
	  (call-interactively binding))))))

(defun smart-helm-assist()
  "Executes helm actions based on Assist Key click locations:
  At the end of the buffer, quits from helm and exits the minibuffer.
  On a candidate line, display's the candidate's first action and remains in the minibuffer;
  On the top, fixed header line, toggles display of the selected candidate's possible actions;
  On an action list line, performs the action after exiting the minibuffer;
  On a source section header, moves to the previous source section or last if on first.
  On a candidate separator line, does nothing.
  In the minibuffer window, ends the helm session and performs the selected item's action."
  ;; Hyperbole has checked that this line has an action prior to
  ;; invoking this function.
  (unless (hmouse-check-assist-key)
    (error "(smart-helm-assist): Hyperbole Assist Mouse Key - either depress or release event is improperly configured"))
  (let* ((non-text-area-p (and (eventp assist-key-depress-args)
			       (posn-area (event-start assist-key-depress-args))))
	 (at-plist (smart-helm-at assist-key-depress-args))
	 (section-hdr (plist-get at-plist 'section-header))
	 (separator (plist-get at-plist 'separator))
	 (eob (plist-get at-plist 'eob))
	 (eol (plist-get at-plist 'eol))
	 (key))
    (unwind-protect
	(smart-helm-resume-helm)
      ;; Handle end-of-line clicks.
      (cond ((and eol (not eob) (not non-text-area-p))
	     (with-helm-buffer (funcall assist-key-eol-function)))
	    ((and (smart-helm-alive-p) (not separator))
	     (setq key (cond
			;; Exit
			(eob "C-g")
			;; Move to previous source section or last if on last.
			(section-hdr "M-o")
			;; If line of the key press is the first /
			;; header line in the window or outside the
			;; buffer area, then use {TAB} command to
			;; switch between match list and action list.
			(non-text-area-p "TAB")
			;; Display action for the current line and
			;; return nil.
			(t (with-help-window "*Helm Help*"
			     (let ((helm-buffer (if (equal helm-action-buffer (buffer-name))
						    helm-buffer (buffer-name))))
			       (with-helm-buffer
				 (princ "The current helm selection item is:\n\t")
				 (princ (helm-get-selection (helm-buffer-get)))
				 (princ "\nwith an action of:\n\t")
				 (princ (helm-get-current-action)))
			       nil)))))
	     (if hkey-debug
		 (message "(HyDebug): In smart-helm-assist, key to execute is: {%s}; binding is: %s"
			  (if key (key-description key) "Help" (if key binding "None"))))))
      (if (> (minibuffer-depth) 0)
	  (select-window (minibuffer-window))))
    (if key (call-interactively (key-binding (kbd key))))))

;;; ************************************************************************
;;; Global mouse key bindings for helm
;;; ************************************************************************

(defun hmouse-check-action-key ()
  "After use of the Action Mouse Key, ensure both depress and release events are assigned to the key.
Returns t iff the key is properly bound, else nil."
  (and (or (and (eventp action-key-depress-args) (eventp action-key-release-args))
	   (not (or action-key-depress-args action-key-release-args)))
       (where-is-internal 'action-key-depress-emacs (current-global-map) t)
       (where-is-internal 'action-mouse-key-emacs (current-global-map) t)))

(defun hmouse-check-assist-key ()
  "After use of the Assist Mouse Key, ensure both depress and release events are assigned to the key.
Returns t iff the key is properly bound, else nil."
  (and (or (and (eventp assist-key-depress-args) (eventp assist-key-release-args))
	   (not (or assist-key-depress-args assist-key-release-args)))
       (where-is-internal 'assist-key-depress-emacs (current-global-map) t)
       (where-is-internal 'assist-mouse-key-emacs (current-global-map) t)))

(defun hmouse-set-key-list (binding key-list)
  (mapc (lambda (key) (global-set-key key binding)) key-list)
  nil)

(defun hmouse-bind-key (mouse-key-number depress-cmd release-cmd)
  "Ensure MOUSE-KEY-NUMBER (1-5), e.g. 1 for [mouse-1], is globally bound to DEPRESS-CMD and RELEASE-CMD (includes depresses and drags).
Use nil as cmd values to unbind a key."
  ;; Works under GNU Emacs only.
  (hmouse-set-key-list
   depress-cmd
   (nth (1- mouse-key-number)
	'(
	  ([down-mouse-1] [header-line down-mouse-1]
	   [left-fringe down-mouse-1]
	   [right-fringe down-mouse-1]
	   [vertical-line down-mouse-1]
	   [mode-line down-mouse-1])

	  ([down-mouse-2] [header-line down-mouse-2]
	   [left-fringe down-mouse-2]
	   [right-fringe down-mouse-2]
	   [vertical-line down-mouse-2]
	   [mode-line down-mouse-2])

	  ([down-mouse-3] [header-line down-mouse-3]
	   [left-fringe down-mouse-3]
	   [right-fringe down-mouse-3]
	   [vertical-line down-mouse-3]
	   [mode-line down-mouse-3])
	  )))
	   
  (hmouse-set-key-list
   release-cmd
   (nth (1- mouse-key-number)
	'(
	  ([drag-mouse-1] [mouse-1]
	   [double-mouse-1] [triple-mouse-1]
	   [header-line drag-mouse-1]
	   [header-line mouse-1]
	   [left-fringe drag-mouse-1]
	   [left-fringe mouse-1]
	   [right-fringe drag-mouse-1]
	   [right-fringe mouse-1]
	   [vertical-line drag-mouse-1]
	   [vertical-line mouse-1]
	   [mode-line drag-mouse-1]
	   [mode-line mouse-1])

	  ([drag-mouse-2] [mouse-2]
	   [double-mouse-2] [triple-mouse-2]
	   [header-line drag-mouse-2]
	   [header-line mouse-2]
	   [left-fringe drag-mouse-2]
	   [left-fringe mouse-2]
	   [right-fringe drag-mouse-2]
	   [right-fringe mouse-2]
	   [vertical-line drag-mouse-2]
	   [vertical-line mouse-2]
	   [mode-line drag-mouse-2]
	   [mode-line mouse-2])

	  ([drag-mouse-3] [mouse-3]
	   [double-mouse-3] [triple-mouse-3]
	   [header-line drag-mouse-3]
	   [header-line mouse-3]
	   [left-fringe drag-mouse-3]
	   [left-fringe mouse-3]
	   [right-fringe drag-mouse-3]
	   [right-fringe mouse-3]
	   [vertical-line drag-mouse-3]
	   [vertical-line mouse-3]
	   [mode-line drag-mouse-3]
	   [mode-line mouse-3])
	  ))))

;; Redefine this function from Emacs to add before and after hooks.
(defun posn-set-point (position)
  "Move point to POSITION.
Select the corresponding window as well."
  (if (not (windowp (posn-window position)))
      (error "Position not in text area of window"))
  (run-hooks 'before-set-point-hook)
  (select-window (posn-window position))
  (prog1 (if (numberp (posn-point position))
	     (goto-char (posn-point position)))
    (run-hooks 'after-set-point-hook)))

;; In helm, make [mouse-1] mark any chosen line that has an action.
(add-hook 'after-set-point-hook
	  (lambda ()
	    (when (or (eq major-mode 'helm-major-mode)
		      (equal "*helm action*" (buffer-name)))
	      (unless (region-active-p)
		;; Marks current line only if it has an action.
		(smart-helm-line-has-action))
	      (when (and (eq last-command 'mouse-set-point)
			 helm-alive-p (> (minibuffer-depth) 0))
		(select-window (minibuffer-window))))))

(hmouse-bind-key 2 #'action-key-depress-emacs #'action-mouse-key-emacs)
(hmouse-bind-key 3 #'assist-key-depress-emacs #'assist-mouse-key-emacs)

;; Ensure setting has changed so helm will reinstall proper keymap in
;; helm buffer.
(setq helm-allow-mouse nil)
(setq helm-allow-mouse 'global-mouse-bindings)
