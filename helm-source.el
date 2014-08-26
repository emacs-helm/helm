;;; helm-source.el  -- Source creation.

;; Copyright (C) 2014  Thierry Volpiatto <thierry.volpiatto@gmail.com>

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
(require 'eieio)

(defclass helm-source ()
  ((name
    :initarg :name
    :initform ""
    :custom string
    :documentation
    "The name of the source.
A string which is also the heading which appears
above the list of matches from the source. Must be unique.")

   (header-name
    :initarg :header-name
    :initform nil
    :custom function
    :documentation
    "A function returning the display string of the header.
Its argument is the name of the source. This attribute is useful to
add an additional information with the source name.
It doesn't modify the name of the source.")
   
   (init
    :initarg :init
    :initform nil
    :custom function
    :documentation
    "Function called with no parameters when helm is started.
It is useful for collecting current state information which can be
used to create the list of candidates later.
Initialization of `candidates-in-buffer' is done here
with `helm-init-candidates-in-buffer'.")

   (update
    :initarg :update
    :initform nil
    :custom function
    :documentation
    "Function called with no parameters at end of reinitialization
when `helm-force-update' is called.")

   (cleanup
    :initarg :cleanup
    :initform nil
    :custom function)
   
   (delayed
    :initarg :delayed
    :initform nil
    :custom (choice null integer))
   
   (keymap
    :initarg :keymap
    :initform nil
    :custom sexp)
   
   (action
    :initarg :action
    :initform 'identity
    :custom (alist :key-type string
                   :value-type function))

   (persistent-action
    :initarg :persistent-action
    :initform nil
    :custom function)

   (persistent-help
    :initarg :persistent-help
    :initform nil
    :custom string)

   (help-message
    :initarg :help-message
    :initform nil
    :custom (choice string function))
   
   (type
    :initarg :type
    :initform nil
    :type symbol)

   (multiline
    :initarg :multiline
    :initform nil
    :custom boolean)
   
   (requires-pattern
    :initarg :requires-pattern
    :initform nil
    :custom integer)

   (candidate-transformer
    :initarg :candidate-transformer
    :initform nil
    :custom (choice function list))

   (filtered-candidate-transformer
    :initarg :filtered-candidate-transformer
    :initform nil
    :custom (choice function list))

   (filter-one-by-one
    :initarg :filter-one-by-one
    :initform nil
    :custom (choice function list))

   (display-to-real
    :initarg :display-to-real
    :initform nil
    :custom function)

   (real-to-display
    :initarg :real-to-display
    :initform nil
    :custom function)

   (action-transformer
    :initarg :action-transformer
    :initform nil
    :custom (choice function list))

   (pattern-transformer
    :initarg :pattern-transformer
    :initform nil
    :custom (choice function list))

   (candidate-number-limit
    :initarg :candidate-number-limit
    :initform nil
    :custom integer)

   (nomark
    :initarg :nomark
    :initform nil
    :custom boolean)
   
   (nohighlight
    :initarg :nohighlight
    :initform nil
    :custom boolean)
   
   (no-matchplugin
    :initarg :no-matchplugin
    :initform nil
    :custom boolean)

   (allow-dups
    :initarg :allow-dups
    :initform nil
    :custom boolean)

   (recenter
    :initarg :recenter
    :initform nil
    :custom boolean)

   (history
    :initarg :history
    :initform nil
    :custom symbol)
   
   (coerce
    :initarg :coerce
    :initform nil
    :custom function)
   
   (mode-line
    :initarg :mode-line
    :initform nil
    :custom (choice string sexp))

   (header-line
    :initarg :header-line
    :initform 'helm-persistent-help-string
    :custom (choice string function))

   (resume
    :initarg :resume
    :initform nil
    :custom function)
   
   (follow
    :initarg :follow
    :initform nil
    :custom integer)

   (follow-delay
    :initarg :follow-delay
    :initform nil
    :custom integer)

   (dont-plug
    :initarg :dont-plug
    :initform nil
    :custom list
    :documentation
    "A list of compile functions plugin to ignore."))
  
  "Main interface to define helm sources."
  :abstract t)

(defclass helm-source-sync (helm-source)
  ((candidates
    :initarg :candidates
    :initform nil
    :custom (choice function list)
    :documentation
    "  Specifies how to retrieve candidates from the source.
It can either be a variable name, a function called with no parameters
or the actual list of candidates.

The list must be a list whose members are strings, symbols
or (DISPLAY . REAL) pairs.

In case of (DISPLAY . REAL) pairs, the DISPLAY string is shown
in the Helm buffer, but the REAL one is used as action
argument when the candidate is selected. This allows a more
readable presentation for candidates which would otherwise be,
for example, too long or have a common part shared with other
candidates which can be safely replaced with an abbreviated
string for display purposes.

Note that if the (DISPLAY . REAL) form is used then pattern
matching is done on the displayed string, not on the real
value.")

   (volatile
    :initarg :volatile
    :initform nil
    :custom boolean)
   
   (match
    :initarg :match
    :initform nil
    :custom function)

   (match-strict
    :initarg :match-strict
    :initform nil
    :custom function)))

(defclass helm-source-async (helm-source)
  ((candidates-process
    :initarg :candidates-process
    :initform nil
    :custom function
    :documentation
    "You should use this attribute when using a function involving
an async process instead of `candidates'.
The function must return a process.")))

(defclass helm-source-in-buffer (helm-source)
  ((candidates-in-buffer
    :initarg :candidates-in-buffer
    :initform t
    :custom boolean
    :documentation
    "It is just here to notify the match-plugin we are using `candidates-in-buffer',
so there is no need to change the value of this slot.")

   (dont-plug
    :initform '(helm-compile-source--candidates-in-buffer))
   
   (candidates
    :initarg :candidates
    :initform 'helm-candidates-in-buffer
    :custom function)

   (volatile
    :initarg :volatile
    :initform t
    :custom boolean)
   
   (match
    :initarg :match
    :initform '(identity)
    :custom (choice function list))
   
   (get-line
    :initarg :get-line
    :initform 'buffer-substring-no-properties
    :custom function)

   (search
    :initarg :search
    :initform '(helm-candidates-in-buffer-search-from-start)
    :custom (choice function list))

   (search-from-end
    :initarg :search-from-end
    :initform nil
    :custom boolean)

   (search-strict
    :initarg :search-strict
    :initform nil
    :custom function)

   (match-part
    :initarg :match-part
    :initform nil
    :custom function)))

(defclass helm-source-dummy (helm-source)
  ((candidates
    :initarg :candidates
    :initform '("dummy")
    :custom list)

   (filtered-candidate-transformer
    :initform 'helm-dummy-candidate)
   
   (accept-empty
    :initarg :accept-empty
    :initform t
    :custom boolean)

   (match
    :initarg :match
    :initform 'identity
    :custom function)
   
   (volatile
    :initarg :volatile
    :initform t
    :custom boolean)))

(defun helm--create-source (object class)
  "[INTERNAL] Build a helm source from a CLASS OBJECT."
  (cl-loop for s in (object-slots object)
           for slot = (class-slot-initarg class s)
           for slot-val = (slot-value object slot)
           when slot-val
           collect (cons s (unless (eq t slot-val) slot-val))))

(defun helm--make-source (name class &rest args)
  "Build a `helm' source named NAME with ARGS for CLASS.
Argument NAME is a string which define the source name, so no need to use
the keyword :name in your source, NAME will be used instead.
Argument CLASS is an eieio class object.
Arguments ARGS are keyword value pairs as defined in CLASS which see."
  (let ((source (apply #'make-instance class name args)))
    (oset source :name name)
    (helm--create-source source class)))

(defmacro helm-build-sync-source (name &rest args)
  `(helm--make-source ,name 'helm-source-sync ,@args))

(defmacro helm-build-async-source (name &rest args)
  `(helm--make-source ,name 'helm-source-async ,@args))

(defmacro helm-build-in-buffer-source (name &rest args)
  `(helm--make-source ,name 'helm-source-in-buffer ,@args))

(defmacro helm-build-dummy-source (name &rest args)
  `(helm--make-source ,name 'helm-source-dummy ,@args))

(provide 'helm-source)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-source ends here
