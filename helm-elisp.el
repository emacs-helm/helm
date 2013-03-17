;;; helm-elisp.el --- Elisp symbols completion for helm.

;; Copyright (C) 2012 ~ 2013 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-utils)
(require 'helm-info)
(require 'helm-eval)
(require 'advice)


(defgroup helm-elisp nil
  "Elisp related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-turn-on-show-completion t
  "Display candidate in buffer while moving selection when non--nil."
  :group 'helm-elisp
  :type  'boolean)

(defcustom helm-show-completion-use-special-display t
  "A special display will be used in lisp completion if non--nil.
All functions that are wrapped in macro `with-helm-show-completion'
will be affected."
  :group 'helm-elisp
  :type  'boolean)

(defcustom helm-show-completion-min-window-height 7
  "Minimum completion window height used in show completion.
This is used in macro `with-helm-show-completion'."
  :group 'helm-elisp
  :type  'integer)


;;; Faces
;;
;;
(defface helm-lisp-show-completion
    '((t (:background "DarkSlateGray")))
  "Face used for showing candidates in `helm-lisp-completion'."
  :group 'helm-elisp)

(defface helm-lisp-completion-info
    '((t (:foreground "red")))
  "Face used for showing info in `helm-lisp-completion'."
  :group 'helm-elisp)


;;; Show completion.
;;
;; Provide show completion with macro `with-helm-show-completion'.

(defvar helm-show-completion-overlay nil)
;; Called each time cursor move in helm-buffer.
(defun helm-show-completion ()
  (with-helm-current-buffer
    (overlay-put helm-show-completion-overlay
                 'display (helm-get-selection))))

(defun helm-show-completion-init-overlay (beg end)
  (and helm-turn-on-show-completion
       (setq helm-show-completion-overlay (make-overlay beg end))
       (overlay-put helm-show-completion-overlay
                    'face 'helm-lisp-show-completion)))

(defun helm-show-completion-display-function (buffer &rest _args)
  "A special resized helm window is used depending on position in BUFFER."
  (with-selected-window (selected-window)
    (let* ((screen-size  (+ (count-screen-lines (window-start) (point) t)
                            1                             ; mode-line
                            (if header-line-format 1 0))) ; header-line
           (def-size     (- (window-height)
                            helm-show-completion-min-window-height))
           (upper-height (max window-min-height (min screen-size def-size)))
           split-window-keep-point)
      (recenter -1)
      (set-window-buffer (if (active-minibuffer-window)
                             (minibuffer-selected-window)
                             (split-window nil upper-height))
                         buffer))))

(defmacro with-helm-show-completion (beg end &rest body)
  "Show helm candidate in an overlay at point.
BEG and END are the beginning and end position of the current completion
in `helm-current-buffer'.
BODY is an helm call where we want to enable show completion.
If `helm-turn-on-show-completion' is nil just do nothing."
  (declare (indent 2) (debug t))
  `(let ((helm-move-selection-after-hook
          (and helm-turn-on-show-completion
               (append (list 'helm-show-completion)
                       helm-move-selection-after-hook))))
     (unwind-protect
          (progn
            (helm-show-completion-init-overlay ,beg ,end)
            (let ((helm-display-function
                   (if helm-show-completion-use-special-display
                       'helm-show-completion-display-function
                       'helm-default-display-buffer)))
              ,@body))
       (and helm-turn-on-show-completion
            (delete-overlay helm-show-completion-overlay)))))


;;; Lisp symbol completion.
;;
;;
;;;###autoload
(defun helm-lisp-completion-at-point ()
  "Helm lisp symbol completion at point."
  (interactive)
  (let* ((data       (lisp-completion-at-point))
         (beg        (car data))
         (end        (point)) ; 'cadr data' is wrong when no space after point.
         (plist      (nthcdr 3 data))
         (pred       (plist-get plist :predicate))
         (lgst-len   0)
         (target     (and beg end (buffer-substring-no-properties beg end)))
         (candidates (and data (all-completions target (nth 2 data) pred)))
         (helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         (helm-match-plugin-enabled
          (member 'helm-compile-source--match-plugin
                  helm-compile-source-functions)))
    (if candidates
        (with-helm-show-completion beg end
          ;; Overlay is initialized now in helm-current-buffer.
          (helm
           :sources
           '((name . "Lisp completion")
             (init . (lambda ()
                       (with-current-buffer (helm-candidate-buffer 'global)
                         (loop for sym in candidates
                               for len = (length sym)
                               when (> len lgst-len) do (setq lgst-len len)
                               do (insert (concat sym "\n"))))))
             (candidates-in-buffer)
             (persistent-action . helm-lisp-completion-persistent-action)
             (persistent-help . "Show brief doc in mode-line")
             (filtered-candidate-transformer helm-lisp-completion-transformer)
             (action . (lambda (candidate)
                         (delete-region beg end)
                         (insert candidate))))
           :input (if helm-match-plugin-enabled (concat target " ") target)
           :resume 'noresume))
        (message "[No Match]"))))

(defun helm-lisp-completion-persistent-action (candidate)
  (let ((cursor-in-echo-area t)
        mode-line-in-non-selected-windows)
    (helm-show-info-in-mode-line
     (propertize
      (helm-get-first-line-documentation
       (intern candidate))
      'face 'helm-lisp-completion-info))))

(defun helm-lisp-completion-transformer (candidates source)
  "Helm candidates transformer for lisp completion."
  (declare (special lgst-len))
  (loop for c in candidates
        for sym = (intern c)
        for annot = (cond ((commandp sym) " (Com)")
                          ((fboundp sym)  " (Fun)")
                          ((boundp sym)   " (Var)")
                          ((facep sym)    " (Face)"))
        for spaces = (make-string (- lgst-len (length c)) ? )
        collect (cons (concat c spaces annot) c)))

(defun helm-get-first-line-documentation (sym)
  "Return first line documentation of symbol SYM.
If SYM is not documented, return \"Not documented\"."
  (let ((doc (cond ((fboundp sym)
                    (documentation sym t))
                   ((boundp sym)
                    (documentation-property sym 'variable-documentation t))
                   ((facep sym)
                    (face-documentation sym))
                   (t nil))))
    (if (and doc (not (string= doc ""))
             ;; `documentation' return "\n\n(args...)"
             ;; for CL-style functions.
             (not (string-match-p "^\n\n" doc)))
        (car (split-string doc "\n"))
        "Not documented")))

;;; File completion.
;;
;; Complete file name at point.
(defun helm-thing-before-point ()
  "Get symbol name before point."
  (save-excursion
    (let ((beg (point)))
      ;; older regexp "\(\\|\\s-\\|^\\|\\_<\\|\r\\|'\\|#'"
      (when (re-search-backward
             "\\_<" (field-beginning nil nil (point-at-bol)) t)
        (buffer-substring-no-properties beg (match-end 0))))))

;;;###autoload
(defun helm-complete-file-name-at-point (&optional force)
  "Complete file name at point."
  (interactive)
  (let* ((tap (thing-at-point 'filename))
         beg
         (init (and tap
                    (or force
                        (save-excursion
                          (end-of-line)
                          (search-backward tap (point-at-bol) t)
                          (setq beg (point))
                          (looking-back "[^'`( ]")))
                    (expand-file-name
                     (substring-no-properties tap))))
         (end  (point))
         (helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         completion)
    (with-helm-show-completion beg end
      (setq completion (helm-read-file-name "FileName: "
                                              :initial-input init)))
    (when (and completion (not (string= completion "")))
      (delete-region beg end) (insert (if (string-match "^~" tap)
                                          (abbreviate-file-name completion)
                                          completion)))))

;;;###autoload
(defun helm-lisp-indent ()
  ;; It is meant to use with `helm-define-multi-key' which
  ;; does not support args for functions yet, so use `current-prefix-arg'
  ;; for now instead of (interactive "P").
  (interactive)
  (let ((tab-always-indent (or (eq tab-always-indent 'complete)
                               tab-always-indent)))
    (indent-for-tab-command current-prefix-arg)))

;;;###autoload
(defun helm-lisp-completion-or-file-name-at-point ()
  "Complete lisp symbol or filename at point.
Filename completion happen if string start after or between a double quote."
  (interactive)
  (let* ((tap (thing-at-point 'filename)))
    (if (and tap (save-excursion
                   (end-of-line)
                   (search-backward tap (point-at-bol) t)
                   (looking-back "[^'`( ]")))
        (helm-complete-file-name-at-point)
        (helm-lisp-completion-at-point))))


;;; Apropos
;;
;;
(defun helm-apropos-init (test default)
  "Init candidates buffer for `helm-apropos' sources."
  (require 'helm-help)
  (with-current-buffer (helm-candidate-buffer 'global)
    (goto-char (point-min))
    (when (and default (stringp default)
               ;; Some defaults args result as
               ;; (symbol-name nil) == "nil".
               ;; e.g debug-on-entry.
               (not (string= default "nil"))
               (funcall test (intern default)))
      (insert (concat default "\n")))
    (loop with all = (all-completions "" obarray test)
          for sym in all
          for s = (intern sym)
          unless (or (and default (string= sym default))
                     (keywordp s))
          do (insert (concat sym "\n")))))

(defun helm-def-source--emacs-variables (&optional default)
  `((name . "Variables")
    (init . (lambda ()
              (helm-apropos-init 'boundp ,default)))
    (candidates-in-buffer)
    (action . (("Describe Variable" . helm-describe-variable)
               ("Find Variable" . helm-find-variable)))))

(defun helm-def-source--emacs-faces (&optional default)
  `((name . "Faces")
    (init . (lambda ()
              (helm-apropos-init 'facep ,default)))
    (candidates-in-buffer)
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (loop for c in candidates
                                              collect (propertize c 'face (intern c)))))
    (action . (lambda (candidate)
                (describe-face (intern candidate))))))

(defun helm-def-source--helm-attributes (&optional default)
  `((name . "Helm attributes")
    (candidates . (lambda ()
                    (mapcar 'symbol-name helm-attributes)))
    (action . (lambda (candidate)
                (let (special-display-buffer-names
                      special-display-regexps
                      helm-persistent-action-use-special-display)
                  (with-output-to-temp-buffer "*Help*"
                    (princ (get (intern candidate) 'helm-attrdoc))))))))

(defun helm-def-source--emacs-commands (&optional default)
  `((name . "Commands")
    (init . (lambda ()
              (helm-apropos-init 'commandp ,default)))
    (candidates-in-buffer)
    (action . (("Describe Function" . helm-describe-function)
               ("Find Function" . helm-find-function)))))

(defun helm-def-source--emacs-functions (&optional default)
  `((name . "Functions")
    (init . (lambda ()
              (helm-apropos-init #'(lambda (x) (and (fboundp x)
                                                 (not (commandp x))))
                                   ,default)))
    (candidates-in-buffer)
    (action . (("Describe Function" . helm-describe-function)
               ("Find Function" . helm-find-function)))))

;;;###autoload
(defun helm-apropos ()
  "Preconfigured helm to describe commands, functions, variables and faces."
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
          (mapcar (lambda (func)
                    (funcall func default))
                  '(helm-def-source--emacs-commands
                    helm-def-source--emacs-functions
                    helm-def-source--emacs-variables
                    helm-def-source--emacs-faces
                    helm-def-source--helm-attributes))
          :buffer "*helm apropos*"
          :preselect (and default (concat "\\_<" (regexp-quote default) "\\_>")))))


;;; Advices
;;
;;
(defvar helm-source-advice
  '((name . "Function Advice")
    (candidates . helm-advice-candidates)
    (action ("Toggle Enable/Disable" . helm-advice-toggle))
    (persistent-action . helm-advice-persistent-action)
    (multiline)
    (persistent-help . "Describe function / C-u C-z: Toggle advice")))

(defun helm-advice-candidates ()
  (loop for (fname) in ad-advised-functions
        for function = (intern fname)
        append
        (loop for class in ad-advice-classes append
              (loop for advice in (ad-get-advice-info-field function class)
                    for enabled = (ad-advice-enabled advice)
                    collect
                    (cons (format
                           "%s %s %s"
                           (if enabled "Enabled " "Disabled")
                           (propertize fname 'face 'font-lock-function-name-face)
                           (ad-make-single-advice-docstring advice class nil))
                          (list function class advice))))))

(defun helm-advice-persistent-action (func-class-advice)
  (if current-prefix-arg
      (helm-advice-toggle func-class-advice)
      (describe-function (car func-class-advice))))

(defun helm-advice-toggle (func-class-advice)
  (destructuring-bind (function class advice) func-class-advice
    (cond ((ad-advice-enabled advice)
           (ad-advice-set-enabled advice nil)
           (message "Disabled"))
          (t
           (ad-advice-set-enabled advice t)
           (message "Enabled")))
    (ad-activate function)
    (and helm-in-persistent-action
         (helm-advice-update-current-display-string))))

(defun helm-advice-update-current-display-string ()
  (helm-edit-current-selection
    (let ((newword (cond ((looking-at "Disabled") "Enabled")
                         ((looking-at "Enabled")  "Disabled")))
          realvalue)
      (when newword
        (delete-region (point) (progn (forward-word 1) (point)))
        (insert newword)))))

;;;###autoload
(defun helm-manage-advice ()
  "Preconfigured `helm' to disable/enable function advices."
  (interactive)
  (helm-other-buffer 'helm-source-advice "*helm advice*"))


;;; Elisp library scan
;;
;;
(defvar helm-source-elisp-library-scan
  '((name . "Elisp libraries (Scan)")
    (init . (helm-elisp-library-scan-init))
    (candidates-in-buffer)
    (action ("Find library"
             . (lambda (candidate) (find-file (find-library-name candidate))))
     ("Find library other window"
      . (lambda (candidate)
          (find-file-other-window (find-library-name candidate))))
     ("Load library"
      . (lambda (candidate) (load-library candidate))))))

(defun helm-elisp-library-scan-init ()
  "Init helm buffer status."
  (let ((helm-buffer (helm-candidate-buffer 'global))
        (library-list (helm-elisp-library-scan-list)))
    (with-current-buffer helm-buffer
      (dolist (library library-list)
        (insert (format "%s\n" library))))))

(defun helm-elisp-library-scan-list (&optional dirs string)
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

(defun helm-set-variable (var)
  "Set value to VAR interactively."
  (interactive)
  (let ((sym (helm-symbolify var)))
    (set sym (eval-minibuffer (format "Set %s: " var)
                              (prin1-to-string (symbol-value sym))))))


;;; Type attributes
;;
;;
(let ((actions '(("Describe command" . describe-function)
                 ("Add command to kill ring" . helm-kill-new)
                 ("Go to command's definition" . find-function)
                 ("Debug on entry" . debug-on-entry)
                 ("Cancel debug on entry" . cancel-debug-on-entry)
                 ("Trace function" . trace-function)
                 ("Trace function (background)" . trace-function-background)
                 ("Untrace function" . untrace-function))))
  (define-helm-type-attribute 'command
      `((action ("Call interactively" . helm-call-interactively)
                ,@actions)
        (coerce . helm-symbolify)
        (persistent-action . describe-function))
    "Command. (string or symbol)")

  (define-helm-type-attribute 'function
      `((action . ,actions)
        (action-transformer helm-transform-function-call-interactively)
        (candidate-transformer helm-mark-interactive-functions)
        (coerce . helm-symbolify))
    "Function. (string or symbol)"))

(define-helm-type-attribute 'variable
    '((action ("Describe variable" . describe-variable)
       ("Add variable to kill ring" . helm-kill-new)
       ("Go to variable's definition" . find-variable)
       ("Set variable" . helm-set-variable))
      (coerce . helm-symbolify))
  "Variable.")



(define-helm-type-attribute 'timer
    '((real-to-display . helm-timer-real-to-display)
      (action ("Cancel Timer" . cancel-timer)
       ("Describe Function" . (lambda (tm) (describe-function (timer--function tm))))
       ("Find Function" . (lambda (tm) (find-function (timer--function tm)))))
      (persistent-action . (lambda (tm) (describe-function (timer--function tm))))
      (persistent-help . "Describe Function"))
  "Timer.")


;;; Elisp Timers.
;;
;;
(defvar helm-source-absolute-time-timers
  '((name . "Absolute Time Timers")
    (candidates . timer-list)
    (type . timer)))

(defvar helm-source-idle-time-timers
  '((name . "Idle Time Timers")
    (candidates . timer-idle-list)
    (type . timer)))

(defun helm-timer-real-to-display (timer)
  (format "%s repeat=%5S %s(%s)"
          (let ((time (timer--time timer)))
            (if (timer--idle-delay timer)
                (format-time-string "idle-for=%5s" time)
              (format-time-string "%m/%d %T" time)))
          (timer--repeat-delay timer)
          (timer--function timer)
          (mapconcat 'prin1-to-string (timer--args timer) " ")))

;;;###autoload
(defun helm-timers ()
  "Preconfigured `helm' for timers."
  (interactive)
  (helm-other-buffer '(helm-source-absolute-time-timers
                       helm-source-idle-time-timers)
                     "*helm timers*"))


;;; Complex command history
;;
;;
(defvar helm-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda () (mapcar 'prin1-to-string command-history)))
    (type . sexp)))

;;;###autoload
(defun helm-complex-command-history ()
  (interactive)
  (helm :sources 'helm-source-complex-command-history
        :buffer "*helm complex commands*"))

(provide 'helm-elisp)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-elisp.el ends here
