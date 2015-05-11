;;; helm-elisp.el --- Elisp symbols completion for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2015 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'helm-utils)
(require 'helm-info)
(require 'helm-eval)
(require 'helm-files)
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

(defcustom helm-lisp-quoted-function-list
  '(funcall apply mapc cl-mapc mapcar cl-mapcar
    callf callf2 cl-callf cl-callf2 fset
    fboundp fmakunbound symbol-function)
  "List of function where quoted function completion happen.
e.g give only function names after \(funcall '."
  :group 'helm-elisp
  :type '(repeat (choice symbol)))

(defcustom helm-lisp-unquoted-function-list
  '(function defadvice)
  "List of function where unquoted function completion happen.
e.g give only function names after \(function ."
  :group 'helm-elisp
  :type '(repeat (choice symbol)))

(defcustom helm-apropos-fuzzy-match nil
  "Enable fuzzy matching for `helm-apropos' when non-nil."
  :group 'helm-elisp
  :type 'boolean)

(defcustom helm-lisp-fuzzy-completion nil
  "Enable fuzzy matching in emacs-lisp completion when non-nil.
NOTE: This enable fuzzy matching in helm native implementation of
elisp completion, but not on helmized elisp completion, i.e
fuzzy completion is not available in `completion-at-point'."
  :group 'helm-elisp
  :type 'boolean)

(defcustom helm-apropos-function-list '(helm-def-source--emacs-commands
                                        helm-def-source--emacs-functions
                                        helm-def-source--eieio-classes
                                        helm-def-source--eieio-generic
                                        helm-def-source--emacs-variables
                                        helm-def-source--emacs-faces
                                        helm-def-source--helm-attributes)
  "A list of functions that build helm sources to use in `helm-apropos'."
  :group 'helm-elisp
  :type '(repeat (choice symbol)))


;;; Faces
;;
;;
(defgroup helm-elisp-faces nil
  "Customize the appearance of helm-elisp."
  :prefix "helm-"
  :group 'helm-elisp
  :group 'helm-faces)

(defface helm-lisp-show-completion
    '((t (:background "DarkSlateGray")))
  "Face used for showing candidates in `helm-lisp-completion'."
  :group 'helm-elisp-faces)

(defface helm-lisp-completion-info
    '((t (:foreground "red")))
  "Face used for showing info in `helm-lisp-completion'."
  :group 'helm-elisp-faces)


;;; Show completion.
;;
;; Provide show completion with macro `with-helm-show-completion'.

(defvar helm-show-completion-overlay nil)

;; Called each time cursor move in helm-buffer.
(defun helm-show-completion ()
  (with-helm-current-buffer
    (overlay-put helm-show-completion-overlay
                 'display (substring-no-properties
                           (helm-get-selection)))))

(defun helm-show-completion-init-overlay (beg end)
  (when (and helm-turn-on-show-completion beg end)
    (setq helm-show-completion-overlay (make-overlay beg end))
    (overlay-put helm-show-completion-overlay
                 'face 'helm-lisp-show-completion)))

(defun helm-show-completion-display-function (buffer &rest _args)
  "A special resized helm window is used depending on position in BUFFER."
  (with-selected-window (selected-window)
    (if (window-dedicated-p)
        (helm-default-display-buffer buffer)
      (let* ((screen-size  (+ (count-screen-lines (window-start) (point) t)
                              1                         ; mode-line
                              (if header-line-format 1 0))) ; header-line
             (def-size     (- (window-height)
                              helm-show-completion-min-window-height))
             (upper-height (max window-min-height (min screen-size def-size)))
             split-window-keep-point)
        (recenter -1)
        (set-window-buffer (if (active-minibuffer-window)
                               (minibuffer-selected-window)
                             (split-window nil upper-height))
                           buffer)))))

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
     (with-helm-temp-hook 'helm-after-initialize-hook
       (with-helm-buffer
         (set (make-local-variable 'helm-display-function)
              (if helm-show-completion-use-special-display
                  'helm-show-completion-display-function
                'helm-default-display-buffer))))
     (unwind-protect
          (progn
            (helm-show-completion-init-overlay ,beg ,end)
            ,@body)
       (when (and helm-turn-on-show-completion
                  helm-show-completion-overlay
                  (overlayp helm-show-completion-overlay))
         (delete-overlay helm-show-completion-overlay)))))


;;; Lisp symbol completion.
;;
;;
(defun helm-lisp-completion--predicate-at-point (beg)
  ;; Return a predicate for `all-completions'.
  (let ((fn-sym-p (lambda ()
                    (or
                     (and (eq (char-before) ?\ )
                          (save-excursion
                            (skip-syntax-backward " " (point-at-bol))
                            (memq (symbol-at-point)
                                  helm-lisp-unquoted-function-list)))
                     (and (eq (char-before) ?\')
                          (save-excursion
                            (forward-char -1)
                            (eq (char-before) ?\#)))))))
    (save-excursion
      (goto-char beg)
      (if (or
           ;; Complete on all symbols in non--lisp modes (logs mail etc..)
           (not (memq major-mode '(emacs-lisp-mode
                                   lisp-interaction-mode
                                   inferior-emacs-lisp-mode)))
           (not (or (funcall fn-sym-p)
                    (and (eq (char-before) ?\')
                         (save-excursion
                           (forward-char (if (funcall fn-sym-p) -2 -1))
                           (skip-syntax-backward " " (point-at-bol))
                           (memq (symbol-at-point)
                                 helm-lisp-quoted-function-list)))
                    (eq (char-before) ?\())) ; no paren before str.
           ;; Looks like we are in a let statement.
           (condition-case nil
               (progn (up-list -2) (forward-char 1)
                      (eq (char-after) ?\())
             (error nil)))
          (lambda (sym)
            (or (boundp sym) (fboundp sym) (symbol-plist sym)))
        #'fboundp))))

(defun helm-thing-before-point (&optional limits regexp)
  "Return symbol name before point.
If REGEXP is specified return what REGEXP find before point.
By default match the beginning of symbol before point.
With LIMITS arg specified return the beginning and end position
of symbol before point."
  (save-excursion
    (let (beg
          (end (point))
          (boundary (field-beginning nil nil (point-at-bol))))
      (if (re-search-backward (or regexp "\\_<") boundary t)
          (setq beg (match-end 0))
        (setq beg boundary))
      (unless (= beg end)
        (if limits
            (cons beg end)
          (buffer-substring-no-properties beg end))))))

(defun helm-bounds-of-thing-before-point (&optional regexp)
  "Get the beginning and end position of `helm-thing-before-point'.
Return a cons \(beg . end\)."
  (helm-thing-before-point 'limits regexp))

(defun helm-insert-completion-at-point (beg end str)
  ;; When there is no space after point
  ;; we are completing inside a symbol or
  ;; after a partial symbol with the next arg aside
  ;; without space, in this case mark the region.
  ;; deleting it would remove the
  ;; next arg which is unwanted.
  (delete-region beg end)
  (insert str)
  (let ((pos (cdr (or (bounds-of-thing-at-point 'symbol)
                      ;; needed for helm-dabbrev.
                      (bounds-of-thing-at-point 'filename)))))
    (when (and pos (< (point) pos))
      (push-mark pos t t))))

(defvar helm-lisp-completion--cache nil)
(defvar helm-lgst-len nil)
;;;###autoload
(defun helm-lisp-completion-at-point ()
  "Helm lisp symbol completion at point."
  (interactive)
  (setq helm-lgst-len 0)
  (let* ((target     (helm-thing-before-point))
         (beg        (car (helm-bounds-of-thing-before-point)))
         (end        (point))
         (pred       (and beg (helm-lisp-completion--predicate-at-point beg)))
         (loc-vars   (and (fboundp 'elisp--local-variables)
                          (ignore-errors
                            (mapcar #'symbol-name (elisp--local-variables)))))
         (glob-syms  (and target pred (all-completions target obarray pred)))
         (candidates (append loc-vars glob-syms))
         (helm-quit-if-no-candidate t)
         (helm-execute-action-at-once-if-one t)
         (enable-recursive-minibuffers t))
    (setq helm-lisp-completion--cache (cl-loop for sym in candidates
                                            for len = (length sym)
                                            when (> len helm-lgst-len)
                                            do (setq helm-lgst-len len)
                                            collect sym))
    (if candidates
        (with-helm-show-completion beg end
          ;; Overlay is initialized now in helm-current-buffer.
          (helm
           :sources (helm-build-in-buffer-source "Lisp completion"
                      :data helm-lisp-completion--cache
                      :persistent-action 'helm-lisp-completion-persistent-action
                      :nomark t
                      :fuzzy-match helm-lisp-fuzzy-completion
                      :persistent-help "Show brief doc in mode-line"
                      :filtered-candidate-transformer 'helm-lisp-completion-transformer
                      :action `(lambda (candidate)
                                 (with-helm-current-buffer
                                   (run-with-timer
                                    0.01 nil
                                    'helm-insert-completion-at-point
                                    ,beg ,end candidate))))
           :input (if helm-lisp-fuzzy-completion
                      target (concat target " "))
           :resume 'noresume
           :buffer "*helm lisp completion*"
           :allow-nest t))
      (message "[No Match]"))))

(defun helm-lisp-completion-persistent-action (candidate)
  (let ((cursor-in-echo-area t)
        mode-line-in-non-selected-windows)
    (helm-show-info-in-mode-line
     (propertize
      (helm-get-first-line-documentation
       (intern candidate))
      'face 'helm-lisp-completion-info))))

(defun helm-lisp-completion-transformer (candidates _source)
  "Helm candidates transformer for lisp completion."
  (cl-loop for c in candidates
        for sym = (intern c)
        for annot = (cl-typecase sym
                      (command " (Com)")
                      (class   " (Class)")
                      (generic " (Gen)")
                      (fbound  " (Fun)")
                      (bound   " (Var)")
                      (face    " (Face)"))
        for spaces = (make-string (- helm-lgst-len (length c)) ? )
        collect (cons (concat c spaces annot) c) into lst
        finally return (sort lst #'helm-generic-sort-fn)))

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

;;;###autoload
(defun helm-complete-file-name-at-point (&optional force)
  "Complete file name at point."
  (interactive)
  (require 'helm-mode)
  (let* ((tap (thing-at-point 'filename))
         beg
         (init (and tap
                    (or force
                        (save-excursion
                          (end-of-line)
                          (search-backward tap (point-at-bol) t)
                          (setq beg (point))
                          (looking-back "[^'`( ]" (1- (point)))))
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
                   (looking-back "[^'`( ]" (1- (point)))))
        (helm-complete-file-name-at-point)
      (helm-lisp-completion-at-point))))


;;; Apropos
;;
;;
(defun helm-apropos-init (test default)
  "Init candidates buffer for `helm-apropos' sources."
  (require 'helm-help)
  (helm-init-candidates-in-buffer 'global
    (let ((default-symbol (and (stringp default)
                               (intern-soft default)))
          (symbols (all-completions "" obarray test)))
      (if (and default-symbol (funcall test default-symbol))
          (cons default-symbol symbols)
        symbols))))

(defun helm-apropos-init-faces (default)
  "Init candidates buffer for faces for `helm-apropos'."
  (require 'helm-help)
  (with-current-buffer (helm-candidate-buffer 'global)
    (goto-char (point-min))
    (let ((default-symbol (and (stringp default)
                               (intern-soft default)))
          (faces (face-list)))
      (when (and default-symbol (facep default-symbol))
        (insert (concat default "\n")))
      (insert
       (mapconcat #'prin1-to-string
                  (if default
                      (cl-remove-if (lambda (sym) (string= sym default)) faces)
                    faces)
                  "\n")))))

(defun helm-apropos-default-sort-fn (candidates _source)
  (if (string= helm-pattern "")
      candidates
      (sort candidates #'helm-generic-sort-fn)))

(defun helm-def-source--emacs-variables (&optional default)
  (helm-build-in-buffer-source "Variables"
    :init `(lambda ()
             (helm-apropos-init
              (lambda (x) (and (boundp x) (not (keywordp x)))) ,default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                         'helm-apropos-default-sort-fn)
    :nomark t
    :action '(("Describe Variable" . helm-describe-variable)
              ("Find Variable" . helm-find-variable)
              ("Info lookup" . helm-info-lookup-symbol)
              ("Set variable" . helm-set-variable))))

(defun helm-def-source--emacs-faces (&optional default)
  (helm-build-in-buffer-source "Faces"
    :init (lambda () (helm-apropos-init-faces default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer
    (append (and (null helm-apropos-fuzzy-match)
                 '(helm-apropos-default-sort-fn))
            (list
             (lambda (candidates _source)
               (cl-loop for c in candidates
                        collect (propertize c 'face (intern c))))))
    :nomark t
    :action (lambda (candidate)
              (describe-face (intern candidate)))))

(defun helm-def-source--helm-attributes (&optional _default)
  (helm-build-sync-source "Helm attributes"
    :candidates (lambda ()
                  (mapcar 'symbol-name helm-attributes))
    :fuzzy-match helm-apropos-fuzzy-match
    :nomark t
    :action (lambda (candidate)
              (let (special-display-buffer-names
                    special-display-regexps
                    helm-persistent-action-use-special-display)
                (with-output-to-temp-buffer "*Help*"
                  (princ (get (intern candidate) 'helm-attrdoc)))))))

(defun helm-def-source--emacs-commands (&optional default)
  (helm-build-in-buffer-source "Commands"
    :init `(lambda ()
             (helm-apropos-init 'commandp ,default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                         'helm-apropos-default-sort-fn)
    :nomark t
    :action '(("Describe Function" . helm-describe-function)
              ("Find Function" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol))))

(defun helm-def-source--emacs-functions (&optional default)
  (helm-build-in-buffer-source "Functions"
    :init `(lambda ()
             (helm-apropos-init #'(lambda (x)
                                    (and (fboundp x)
                                         (not (commandp x))
                                         (not (generic-p x))
                                         (not (class-p x))))
                                ,default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                         'helm-apropos-default-sort-fn)
    :nomark t
    :action '(("Describe Function" . helm-describe-function)
              ("Find Function" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol))))

(defun helm-def-source--eieio-classes (&optional default)
  (helm-build-in-buffer-source "Classes"
    :init `(lambda ()
             (helm-apropos-init #'(lambda (x)
                                    (class-p x))
                                ,default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                         'helm-apropos-default-sort-fn)
    :nomark t
    :action '(("Describe Function" . helm-describe-function)
              ("Find Function" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol))))

(defun helm-def-source--eieio-generic (&optional default)
  (helm-build-in-buffer-source "Generic functions"
    :init `(lambda ()
             (helm-apropos-init #'(lambda (x)
                                    (generic-p x))
                                ,default))
    :fuzzy-match helm-apropos-fuzzy-match
    :filtered-candidate-transformer (and (null helm-apropos-fuzzy-match)
                                         'helm-apropos-default-sort-fn)
    :nomark t
    :action '(("Describe Function" . helm-describe-function)
              ("Find Function" . helm-find-function)
              ("Info lookup" . helm-info-lookup-symbol))))

(defun helm-info-lookup-symbol-1 (c)
  (let ((helm-execute-action-at-once-if-one t)
        (helm-quit-if-no-candidate
         `(lambda ()
            (message "`%s' Not Documented as a symbol" ,c))))
    (helm :sources '(helm-source-info-elisp
                     helm-source-info-cl
                     helm-source-info-eieio)
          :resume 'noresume
          :buffer "*helm lookup*"
          :input c)))

(defun helm-info-lookup-symbol (candidate)
  (run-with-timer 0.01 nil #'helm-info-lookup-symbol-1 candidate))

;;;###autoload
(defun helm-apropos ()
  "Preconfigured helm to describe commands, functions, variables and faces."
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
          (mapcar (lambda (func)
                    (funcall func default))
                  helm-apropos-function-list)
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
    (nomark)
    (multiline)
    (persistent-help . "Describe function / C-u C-j: Toggle advice")))

(defun helm-advice-candidates ()
  (cl-loop for (fname) in ad-advised-functions
        for function = (intern fname)
        append
        (cl-loop for class in ad-advice-classes append
              (cl-loop for advice in (ad-get-advice-info-field function class)
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
  (cl-destructuring-bind (function _class advice) func-class-advice
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
                         ((looking-at "Enabled")  "Disabled"))))
      (when newword
        (delete-region (point) (progn (forward-word 1) (point)))
        (insert newword)))))

;;;###autoload
(defun helm-manage-advice ()
  "Preconfigured `helm' to disable/enable function advices."
  (interactive)
  (helm-other-buffer 'helm-source-advice "*helm advice*"))


;;; Locate elisp library
;;
;;
(defun helm-locate-library-scan-list ()
  (cl-loop for dir in load-path
        when (file-directory-p dir)
        append (directory-files dir t (concat (regexp-opt (get-load-suffixes))
                                              "\\'"))
        into lst
        finally return (helm-fast-remove-dups lst :test 'equal)))

;;;###autoload
(defun helm-locate-library ()
  (interactive)
  (helm :sources (helm-build-in-buffer-source  "Elisp libraries (Scan)"
                   :data (lambda () (helm-locate-library-scan-list))
                   :fuzzy-match t
                   :keymap helm-generic-files-map
                   :match-part (lambda (candidate)
                                 (if helm-ff-transformer-show-only-basename
                                     (helm-basename candidate) candidate))
                   :filter-one-by-one (lambda (c)
                                        (if helm-ff-transformer-show-only-basename
                                            (cons (helm-basename c) c) c))
                   :action (helm-actions-from-type-file))
        :buffer "*helm locate library*"))

(defun helm-set-variable (var)
  "Set value to VAR interactively."
  (let* ((sym (helm-symbolify var))
         (val (default-value sym)))
    (set-default sym (eval-minibuffer (format "Set `%s': " var)
                                      (if (or (stringp val) (memq val '(nil t)))
                                          (prin1-to-string val)
                                          (format "'%s" (prin1-to-string val)))))))


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
    '((action
       ("Describe variable" . describe-variable)
       ("Add variable to kill ring" . helm-kill-new)
       ("Go to variable's definition" . find-variable)
       ("Set variable" . helm-set-variable))
      (coerce . helm-symbolify))
  "Variable.")

(define-helm-type-attribute 'timer
    '((action
       ("Cancel Timer" . (lambda (_timer)
                           (let ((mkd (helm-marked-candidates)))
                             (cl-loop for timer in mkd
                                   do (cancel-timer timer)))))
       ("Describe Function" . (lambda (tm) (describe-function (timer--function tm))))
       ("Find Function" . (lambda (tm) (find-function (timer--function tm)))))
      (persistent-action . (lambda (tm) (describe-function (timer--function tm))))
      (persistent-help . "Describe Function"))
  "Timer.")


;;; Elisp Timers.
;;
;;
(defclass helm-absolute-time-timers-class (helm-source-sync helm-type-timers)
  ((candidates :initform timer-list)
   (filtered-candidate-transformer
    :initform
    (lambda (candidates _source)
      (cl-loop for timer in candidates
               collect (cons (helm-elisp--format-timer timer) timer))))
   (allow-dups :initform t)
   (volatile :initform t)))

(defvar helm-source-absolute-time-timers
  (helm-make-source "Absolute Time Timers" 'helm-absolute-time-timers-class))

(defclass helm-idle-time-timers-class (helm-source-sync helm-type-timers)
  ((candidates :initform timer-idle-list)
   (allow-dups :initform t)
   (volatile :initform t)
   (filtered-candidate-transformer
    :initform
    (lambda (candidates _source)
      (cl-loop for timer in candidates
               collect (cons (helm-elisp--format-timer timer) timer))))))

(defvar helm-source-idle-time-timers
  (helm-make-source "Idle Time Timers" 'helm-idle-time-timers-class))

(defun helm-elisp--format-timer (timer)
  (format "%s repeat=%s %s(%s)"
          (let ((time (timer--time timer)))
            (if (timer--idle-delay timer)
                (format-time-string "idle-for=%5s" time)
              (format-time-string "%m/%d %T" time)))
          (or (timer--repeat-delay timer) "nil")
          (mapconcat 'identity (split-string
                                (prin1-to-string (timer--function timer))
                                "\n") " ")
          (mapconcat 'prin1-to-string (timer--args timer) " ")))

;;;###autoload
(defun helm-timers ()
  "Preconfigured `helm' for timers."
  (interactive)
  (helm :sources '(helm-source-absolute-time-timers
                   helm-source-idle-time-timers)
        :buffer "*helm timers*"))


;;; Complex command history
;;
;;
(defun helm-btf--usable-p ()
  "Return t if current version of `backtrace-frame' accept 2 arguments."
  (condition-case nil
      (and (backtrace-frame 1 'condition-case) t)
    (wrong-number-of-arguments nil)))

(if (helm-btf--usable-p)        ; Check if BTF accept more than one arg.
    ;; Emacs 24.4.
    (dont-compile
      (defvar helm-sexp--last-sexp nil)
      ;; This wont work compiled.
      (defun helm-sexp-eval-1 ()
        (interactive)
        (unwind-protect
             (progn
               ;; Trick called-interactively-p into thinking that `cand' is
               ;; an interactive call, See `repeat-complex-command'.
               (add-hook 'called-interactively-p-functions
                         #'helm-complex-command-history--called-interactively-skip)
               (eval (read helm-sexp--last-sexp)))
          (remove-hook 'called-interactively-p-functions
                       #'helm-complex-command-history--called-interactively-skip)))

      (defun helm-complex-command-history--called-interactively-skip (i _frame1 frame2)
        (and (eq 'eval (cadr frame2))
             (eq 'helm-sexp-eval-1
                 (cadr (backtrace-frame (+ i 2) #'called-interactively-p)))
             1))

      (defun helm-sexp-eval (_candidate)
        (call-interactively #'helm-sexp-eval-1)))
  ;; Emacs 24.3
  (defun helm-sexp-eval (cand)
    (let ((sexp (read cand)))
      (condition-case err
          (if (> (length (remove nil sexp)) 1)
              (eval sexp)
            (apply 'call-interactively sexp))
        (error (message "Evaluating gave an error: %S" err)
               nil)))))

(defvar helm-source-complex-command-history
  (helm-build-sync-source "Complex Command History"
    :candidates (lambda ()
                  ;; Use cdr to avoid adding
                  ;; `helm-complex-command-history' here.
                  (cl-loop for i in command-history
                           unless (equal i '(helm-complex-command-history))
                           collect (prin1-to-string i)))
    :action (helm-make-actions
             "Eval" (lambda (candidate)
                      (and (boundp 'helm-sexp--last-sexp)
                           (setq helm-sexp--last-sexp candidate))
                      (run-with-timer 0.1 nil #'helm-sexp-eval candidate))
             "Edit and eval"
             (lambda (cand)
               (minibuffer-with-setup-hook
                   (lambda () (insert cand))
                 (call-interactively #'eval-expression))))
    :persistent-action #'helm-sexp-eval))

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
