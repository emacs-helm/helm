;;; helm-packages.el --- helm interface to manage packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2012 ~ 2025 Thierry Volpiatto

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
(require 'package)
(require 'finder)
(require 'helm-utils) ; For with-helm-display-marked-candidates.
(require 'async-package)

(declare-function dired-async-mode-line-message "ext:dired-async.el")


;; Urls for cloning.
(defvar helm-packages-recipes-alist '(("melpa" . "https://melpa.org/packages/elpa-packages.eld")
                                      ("gnu"   . "https://elpa.gnu.org/packages/elpa-packages.eld")
                                      ("nongnu" . "https://elpa.nongnu.org/nongnu/elpa-packages.eld"))
  "Adresses where to find recipes or urls for packages.")

;; Caches for recipes (elpa-packages.eld contents).
(defvar helm-packages--gnu-elpa-recipes-cache nil)
(defvar helm-packages--nongnu-elpa-recipes-cache nil)
(defvar helm-packages--melpa-recipes-cache nil)

;; EmacsMirror
(defvar helm-packages-fallback-url-for-cloning "https://github.com/emacsmirror/%s")


(defgroup helm-packages nil
  "Helm interface for package.el."
  :group 'helm)

(defclass helm-packages-class (helm-source-in-buffer)
  ((coerce :initform #'helm-symbolify)
   (find-file-target :initform #'helm-packages-quit-an-find-file)
   (filtered-candidate-transformer
    :initform
    `(helm-packages-transformer
      ,(lambda (candidates _source)
         (sort candidates #'helm-generic-sort-fn))))
   (update :initform #'helm-packages--refresh-contents))
  "A class to define `helm-packages' sources.")

(defcustom helm-packages-async t
  "Install packages async when non nil."
  :type 'boolean)

(defcustom helm-package-install-upgrade-built-in
  (bound-and-true-p package-install-upgrade-built-in)
  "Allow upgrading builtin packages when non nil."
  :type 'boolean)

(defcustom helm-packages-isolate-fn (if (fboundp 'package-isolate)
                                        #'package-isolate
                                      #'helm-packages-isolate-1)
  "The function to isolate package.
`package-isolate' is available only in emacs-30+."
  :type 'function)

(defcustom helm-packages-default-clone-directory nil
  "Default directory where to clone packages."
  :type 'string)

(defcustom helm-packages-clone-after-hook nil
  "Hook that run after cloning a package.
It is called with two args respectively PACKAGE as a string and DIRECTORY."
  :type 'hook)

;;; Actions
;;
;;
(defun helm-packages-upgrade (_candidate)
  "Helm action for upgrading marked packages."
  (let ((mkd (helm-marked-candidates))
        (error-file (expand-file-name
                     "helm-packages-upgrade-error.txt"
                     temporary-file-directory)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Upgrade %s packages? " (length mkd)))
        (if helm-packages-async
            (async-package-do-action 'upgrade mkd error-file)
          (mapc #'package-upgrade mkd))))))

(defun helm-packages-describe (candidate)
  "Helm action for describing package CANDIDATE."
  (describe-package candidate))

(defun helm-packages-get-homepage-url (candidate)
  "Get package CANDIDATE home page url."
  (let* ((id     (package-get-descriptor candidate))
         (extras (package-desc-extras id)))
    (and (listp extras) (cdr-safe (assoc :url extras)))))

(defun helm-packages-visit-homepage (candidate)
  "Helm action for visiting package CANDIDATE home page."
  (let ((url (helm-packages-get-homepage-url candidate)))
    (if (stringp url)
        (browse-url url)
      (message "Package `%s' has no homepage" candidate))))

(defun helm-packages-package-reinstall (_candidate)
  "Helm action for reinstalling marked packages."
  (let ((mkd (helm-marked-candidates))
        (error-file (expand-file-name
                     "helm-packages-reinstall-error.txt"
                     temporary-file-directory)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Reinstall %s packages? " (length mkd)))
        (if helm-packages-async
            (async-package-do-action 'reinstall mkd error-file)
          (mapc #'package-reinstall mkd))))))

(defun helm-packages-delete-1 (packages &optional force)
  "Run `package-delete' on PACKAGES.
If FORCE is non nil force deleting packages."
  (mapc (lambda (x)
          (package-delete (package-get-descriptor x) force))
        packages))

(defun helm-packages-uninstall (_candidate)
  "Helm action for uninstalling marked packages.
Unlike `helm-packages-delete' this will refuse to delete packages when they are
needed by others packages as dependencies."
  (let ((mkd (helm-marked-candidates)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Uninstall %s packages? " (length mkd)))
        (helm-packages-delete-1 mkd)))))

(defun helm-packages-delete (_candidate)
  "Helm action for deleting marked packages.
Unlike `helm-packages-uninstall' this delete packages even when they are needed
as dependencies."
  (let ((mkd (helm-marked-candidates)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Delete %s packages? " (length mkd)))
        (helm-packages-delete-1 mkd 'force)))))

(defun helm-packages-recompile (_candidate)
  "Helm action for recompiling marked packages."
  (let ((mkd (helm-marked-candidates)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Recompile %s packages? " (length mkd)))
        (mapc #'package-recompile mkd)))))

(defun helm-packages-install--sync (packages)
  (condition-case err
      (mapc #'package-install packages)
    (error "%S:\n Please refresh package list before installing" err)))

(defun helm-packages-install (_candidate)
  "Helm action for installing marked packages."
  (let ((mkd (helm-marked-candidates))
        (error-file (expand-file-name
                     "helm-packages-install-error.txt"
                     temporary-file-directory)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      (mapcar #'symbol-name mkd)
      (when (y-or-n-p (format "Install %s packages? " (length mkd)))
        (if helm-packages-async
            (async-package-do-action 'install mkd error-file)
          (helm-packages-install--sync mkd))))))

(defun helm-packages--get-deps (pkg)
  "Recursively find PKG dependencies."
  (let ((desc (cadr (assq pkg package-archive-contents))))
    (when desc
      (cl-loop for req in (package-desc-reqs desc) ; (foo (1 2 3))
               for sym = (car req)
               nconc (cons sym (helm-packages--get-deps sym)) into pkgs
               finally return (helm-fast-remove-dups pkgs)))))

(defun helm-packages-isolate-1 (packages &optional _ignore)
    "Start an Emacs with only PACKAGES loaded.
Arg PACKAGES is a list of strings."
    (let* ((name (concat "package-isolate-" (mapconcat #'identity packages "_")))
           (deps (cl-loop for p in packages
                          for sym = (intern p)
                          nconc (helm-packages--get-deps sym))))
      (apply #'start-process name nil
             (list (expand-file-name invocation-name invocation-directory)
                   "-Q" "--debug-init"
                   (format "--eval=%S"
                           `(progn
                              (require 'package)
                              (setq package-load-list
                                    ',(append (mapcar (lambda (p) (list (intern p) t))
                                                      packages)
                                              (mapcar (lambda (p) (list p t)) deps))
                                    package-user-dir ,package-user-dir
                                    package-directory-list ',package-directory-list)
                              (package-initialize)))))))

(defun helm-packages-isolate (_candidate)
  "Start a new Emacs with only marked packages loaded."
  (let* ((mkd (helm-marked-candidates))
         (pkg-names (mapcar #'symbol-name mkd)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      pkg-names
      (when (y-or-n-p "Start a new Emacs with only package(s)? ")
        (funcall helm-packages-isolate-fn pkg-names helm-current-prefix-arg)))))

(defun helm-packages-quit-an-find-file (source)
  "`find-file-target' function for `helm-packages'."
  (let* ((sel (helm-get-selection nil nil source))
         (pkg (package-get-descriptor (intern sel))))
    (if (and pkg (package-installed-p pkg))
        (expand-file-name (package-desc-dir pkg))
      package-user-dir)))

;;; Cloning packages
;;
(defun helm-packages-fetch-recipe (url)
  "Fetch package recipes from URL.
They are generally contained in a file named elpa-packages.eld on remote which
contains the urls needed for cloning packages, each entry is like
\(foo :url \"somewhere\") and may contain as well :branch."
  (with-temp-buffer
    (url-insert-file-contents url)
    (goto-char (point-min))
    (let ((data (read (current-buffer))))
      ;; Recipes do not have the same form depending from
      ;; where we fetch them, they may be like
      ;; ((foo :url "somewhere")
      ;;  (bar :url "somewhere"))
      ;; or
      ;; (((foo :url "somewhere")
      ;;   (bar :url "somewhere"))
      ;;  :version "1" :else "")
      (if (keywordp (cadr data)) (car data) data))))

(defun helm-packages-get-package-url (package provider)
  "Get PACKAGE url from PROVIDER's recipe.
Returns a plist like (:url <url> :branch <branch>).
PROVIDER can be one of \"melpa\", \"gnu\" or \"nongnu\"."
  (let* ((address (assoc-default provider helm-packages-recipes-alist))
         (cache (helm-acase provider
                  ("gnu"    'helm-packages--gnu-elpa-recipes-cache)
                  ("nongnu" 'helm-packages--nongnu-elpa-recipes-cache)
                  ("melpa"  'helm-packages--melpa-recipes-cache)))
         (recipe  (or (symbol-value cache)
                      (set cache (helm-packages-fetch-recipe address))))
         (package-recipe (assq package recipe))
         (core (plist-get (cdr package-recipe) :core))
         (url (or (plist-get (cdr package-recipe) :url)
                  ;; Assume that when :url = nil the package is maintained in
                  ;; elpa or nongnu. When recipe is fetched from package-archives
                  ;; addresses the url is always specified or the package if not
                  ;; clonable not present at all e.g. cond-star.
                  (helm-acase provider
                    ("gnu"    "https://git.sv.gnu.org/git/emacs/elpa.git")
                    ("nongnu" "https://git.sv.gnu.org/git/emacs/nongnu.git"))))
         (branch (plist-get (cdr package-recipe) :branch)))
    (cl-assert package-recipe nil (format "Couldn't find package '%s'" package))
    (cl-assert (null core) nil
               (format "Package '%s' already provided in Emacs at '%s'"
                       package core))
    (unless (stringp url)
      ;; Sometimes the recipe for a package refers to the same url as another
      ;; package by setting :url to the name of this package (symbol) e.g.
      ;; In nongnu recipe, we have:
      ;; (helm :url "https://...") and (helm-core :url helm)
      (setq url (and url (plist-get (cdr (assq url recipe)) :url))))
    `(:url ,url :branch ,branch)))

(defun helm-packages-get-provider (package)
  (let ((desc (assq package package-archive-contents)))
    (package-desc-archive (cadr desc))))

(defun helm-packages-get-recipe-for-cloning (package)
  (let ((provider (helm-packages-get-provider package)))
    (helm-packages-get-package-url package provider)))

(defun helm-packages-clone-package (package)
  "Git clone PACKAGE."
  (let* ((name      (symbol-name package))
         (directory (read-directory-name
                     "Clone in directory: "
                     helm-packages-default-clone-directory nil t))
         (recipe (helm-packages-get-recipe-for-cloning package))
         (url (plist-get recipe :url))
         (branch (plist-get recipe :branch))
         (fix-url (if (or (string-match "\\.git\\'" url)
                          ;; For git-remote-hg.
                          (string-match "\\`hg::" url))
                      url
                    (concat url ".git")))
         ;; In gnu archive all packages maintained on Elpa are pointing to
         ;; "https://git.sv.gnu.org/git/emacs/elpa.git", to be able to clone a
         ;; package from such url we have to use:
         ;; git clone --single-branch -b externals/<PACKAGE> URL PACKAGE-NAME.
         ;; This create a directory named PACKAGE-NAME with only PACKAGE inside.
         ;; If PACKAGE-NAME is ommited this create a repo named elpa which clash if
         ;; such a dir already exists.  Another case is packages coming either
         ;; from nongnu or melpa giving the nongnu url as :url and specifying a
         ;; branch, example:
         ;; (ws-butler :url "https://git.savannah.gnu.org/git/emacs/nongnu.git"
         ;;            :branch "elpa/ws-butler")
         (switches (append
                    (if (string-match "\\(elpa\\|nongnu\\)\\.git\\'" url)
                        `("clone" "--single-branch"
                                  "-b" ,(or branch (format "externals/%s" package)))
                      (delq nil `("clone" ,(and branch "-b") ,branch)))
                    `(,fix-url ,name))))
    (cl-assert (not (file-directory-p (expand-file-name name directory)))
               nil (format "Package already exists in %s" directory))
    (with-helm-default-directory directory
      (let (process-connection-type
            (proc (apply #'start-process
                         "git" "*helm packages clone*"
                         "git" switches)))
        (save-selected-window
          (display-buffer (process-buffer proc)
                          '(display-buffer-below-selected
                            (window-height . fit-window-to-buffer)
                            (preserve-size . (nil . t)))))
        (set-process-filter proc #'helm-packages--clone-filter-process)
        (set-process-sentinel
         proc (lambda (proc event)
                (let ((status (process-exit-status proc)))
                  (if (string= event "finished\n")
                      (message "Cloning package %s done" package)
                    (message "Cloning package %s failed" package))
                  (when (= status 0)
                    (quit-window t (get-buffer-window (process-buffer proc)))
                    (run-hook-with-args
                     'helm-packages-clone-after-hook
                     (symbol-name package) directory)))))
        (message "Cloning package %s..." package)))))

(defun helm-packages--clone-filter-process (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (erase-buffer)
      (insert (car (split-string string ""))))))

;;; Transformers
;;
;;
(defun helm-packages-transformer (candidates _source)
  "Transformer function for `helm-packages'."
  (cl-loop with lgst_arch = (cl-loop for (arch . _) in package-archives
                                     maximize (length arch))
           for c in candidates
           for sym = (intern-soft c)
           for archive = (assq sym package-archive-contents)
           for id = (package-get-descriptor sym)
           for provider = (if archive
                              (package-desc-archive (cadr archive))
                            (and (assq sym package--builtins) "emacs"))
           for status = (if id
                            (package-desc-status id)
                          (and (assq sym package--builtins) "Built-in"))
           for version = (if id
                             (mapconcat #'number-to-string (package-desc-version id) ".")
                           (or (helm-aand (assq sym package--builtins)
                                          (aref (cdr it) 0)
                                          (package-version-join it))
                               "---"))
           for description = (if id
                                 (package-desc-summary id)
                               (helm-aand (assq sym package--builtins)
                                          (aref (cdr it) 2)))
           for disp = (format "%s%s%s%s%s%s%s%s%s"
                              ;; Package name.
                              (propertize
                               c
                               'face
                               (helm-acase status
                                 ("dependency" 'font-lock-type-face)
                                 ("disabled" 'default)
                                 (t 'font-lock-keyword-face))
                               'match-part c)
                              ;; Separator.
                              (helm-make-separator c)
                              ;; Package status.
                              (propertize
                               (or status "")
                               'face (helm-acase status
                                       ("dependency" 'bold-italic)
                                       ("disabled" 'font-lock-property-name-face)
                                       (t 'default)))
                              ;; Separator.
                              (helm-make-separator status 10)
                              ;; Package provider.
                              (or provider "")
                              ;; Separator.
                              (helm-make-separator provider lgst_arch)
                              ;; Package version.
                              (or version "")
                              ;; Separator.
                              (helm-make-separator version 20)
                              ;; Package description.
                              (if description
                                  (propertize description 'face 'font-lock-warning-face)
                                ""))
           collect (cons disp c)))

(defun helm-packages-transformer-1 (candidates _source)
  "Transformer function for `helm-packages' upgrade and delete sources."
  (cl-loop for c in candidates
           collect (cons (propertize c 'face 'font-lock-keyword-face) c)))

(defvar helm-packages--updated nil)
(defun helm-packages--refresh-contents ()
  (unless helm-packages--updated
    (setq helm-packages--gnu-elpa-recipes-cache nil
          helm-packages--nongnu-elpa-recipes-cache nil
          helm-packages--melpa-recipes-cache nil)
    (setq package-menu--old-archive-contents package-archive-contents)
    (setq package-menu--new-package-list nil)
    (package-refresh-contents)
    (package-menu--populate-new-package-list))
  (helm-set-local-variable 'helm-packages--updated t))

(defun helm-finder--list-matches (key)
  (let* ((id (intern key))
         (built-in (gethash id finder-keywords-hash))
    (exts (cl-loop for p in package-archive-contents
                        for sym = (car p)
                        when (package--has-keyword-p
                              (package-get-descriptor sym)
                              (list key))
                        collect sym)))
    (unless (or exts built-in)
      (error "No packages matching key `%s'" key))
    (nconc exts built-in)))

(defun helm-finder-packages-from-keyword (candidate)
  (if (string-match "\\.el$" candidate)
      (finder-commentary candidate)
    (helm :sources
          (helm-make-source "packages" 'helm-packages-class
            :header-name (lambda (name) (format "%s (%s)" name candidate))
            :init (lambda ()
                    (helm-init-candidates-in-buffer
                        'global (helm-fast-remove-dups
                                 (helm-finder--list-matches candidate))))
            :filtered-candidate-transformer
            `(helm-packages-transformer
              ,(lambda (candidates _source)
                 (sort candidates #'helm-generic-sort-fn)))
            :action-transformer
            (lambda (actions candidate)
              (if (package-installed-p candidate)
                  actions
                (append actions
                        '(("Install packages(s)"
                           . helm-packages-install)))))
            :action '(("Describe package" . helm-packages-describe)
                      ("Visit homepage" . helm-packages-visit-homepage)
                      ("Clone package" . helm-packages-clone-package)))
          :buffer "*helm finder results*")))

(defun helm-package--upgradeable-packages (&optional include-builtins)
  ;; Initialize the package system to get the list of package
  ;; symbols for completion.
  (package--archives-initialize)
  (let ((pkgs (if include-builtins
                  (append package-alist
                          (cl-loop for (sym . vec) in package--builtins
                                   when (not (assq sym package-alist))
                                   nconc (list (list sym (package--from-builtin
                                                          (cons sym vec))))))
                package-alist)))
    (cl-loop for (sym desc) in pkgs
             for pkg = (assq sym package-archive-contents)
             for cversion = (and pkg (package-desc-version desc))
             for available = (and pkg (not (package-disabled-p sym cversion)) pkg)
             ;; Exclude packages installed with package-vc (issue#2692).
             when (and available
                       (not (package-vc-p desc))
                       (or (and include-builtins (not cversion))
                           (and cversion
                                (version-list-<
                                 cversion
                                 (package-desc-version (cadr available))))))
             collect sym)))

;;;###autoload
(defun helm-packages (&optional arg)
  "Helm interface to manage packages.

With a prefix arg ARG refresh package list.

When installing or upgrading ensure to refresh the package list
to avoid errors with outdated packages no more availables."
  (interactive "P")
  (package-initialize)
  (when arg (helm-packages--refresh-contents))
  (let ((upgrades (helm-package--upgradeable-packages
                   helm-package-install-upgrade-built-in))
        (removables (package--removable-packages))
        (standard-actions '(("Describe package" . helm-packages-describe)
                            ("Visit homepage" . helm-packages-visit-homepage)
                            ("Install packages(s)" . helm-packages-install)
                            ("Clone package" . helm-packages-clone-package))))
    (helm :sources (list
                    (helm-make-source "Availables for upgrade" 'helm-packages-class
                      :init (lambda ()
                              (helm-init-candidates-in-buffer 'global upgrades))
                      :filtered-candidate-transformer #'helm-packages-transformer-1
                      :action '(("Upgrade package(s)"
                                 . helm-packages-upgrade)))
                    (helm-make-source "Packages to delete" 'helm-packages-class
                      :init (lambda ()
                              (helm-init-candidates-in-buffer 'global removables))
                      :filtered-candidate-transformer #'helm-packages-transformer-1
                      :action '(("Delete package(s)" . helm-packages-delete)))
                    (helm-make-source "Installed packages" 'helm-packages-class
                      :init (lambda ()
                              (helm-init-candidates-in-buffer 'global
                                (mapcar #'car package-alist)))
                      :action '(("Describe package" . helm-packages-describe)
                                ("Visit homepage" . helm-packages-visit-homepage)
                                ("Reinstall package(s)"
                                 . helm-packages-package-reinstall)
                                ("Recompile package(s)" . helm-packages-recompile)
                                ("Uninstall package(s)" . helm-packages-uninstall)
                                ("Isolate package(s)" . helm-packages-isolate)
                                ("Clone package" . helm-packages-clone-package)))
                    (helm-make-source "New packages" 'helm-packages-class
                      :data (lambda ()
                              (cl-loop for p in package-archive-contents
                                       for sym = (car p)
                                       for id = (package-get-descriptor sym)
                                       for status = (and id (package-desc-status id))
                                       when (equal status "new")
                                       nconc (list (car p))))
                      :action standard-actions)
                    (helm-make-source "Available external packages" 'helm-packages-class
                      :data (lambda ()
                              (cl-loop for p in package-archive-contents
                                       for sym = (car p)
                                       for id = (package-get-descriptor sym)
                                       for status = (package-desc-status id)
                                       unless (or (and id (member
                                                           status
                                                           '("new" "installed"
                                                             "dependency" "source")))
                                                  (and id (assoc sym package--builtins)))
                                       nconc (list (car p))))
                      :action standard-actions)
                    (helm-make-source "Available built-in packages" 'helm-packages-class
                      :data (lambda ()
                              (cl-loop for p in package--builtins
                                       ;; Show only builtins that are available as
                                       ;; well on (m)elpa. Other builtins don't
                                       ;; have a package-descriptor, the format is
                                       ;; (sym . [version reqs summary]).
                                       when (package-desc-p (package-get-descriptor (car p)))
                                       collect (car p)))
                      :action (remove (assoc "Clone package" standard-actions)
                                      standard-actions)))
          :buffer "*helm packages*")))

;;;###autoload
(defun helm-finder (&optional arg)
  "Helm interface to find packages by keywords with `finder'.
To have more actions on packages, use `helm-packages'."
  (interactive "P")
  (package-initialize)
  (when arg (helm-packages--refresh-contents))
  (helm :sources
        (helm-build-in-buffer-source "helm finder"
          :data (cl-loop for p in package-archive-contents
                         for sym = (car p)
                         for desc = (package-get-descriptor sym)
                         nconc (copy-sequence (package-desc--keywords desc)) into keywords
                         finally return (helm-fast-remove-dups keywords :test 'equal))
          :filtered-candidate-transformer
          (list
           (lambda (candidates _source)
             (cl-loop for cand in candidates
                      for desc = (or (assoc-default (intern-soft cand) finder-known-keywords)
                                     cand)
                      for sep = (helm-make-separator cand)
                      for disp = (helm-aand (propertize desc 'face 'font-lock-warning-face)
                                            (propertize " " 'display (concat sep it))
                                            (concat cand it))
                      collect (cons disp cand)))
           (lambda (candidates _source)
                    (sort candidates #'helm-generic-sort-fn)))
          :action (helm-make-actions
                   "Packages from keyword" 'helm-finder-packages-from-keyword)
          :persistent-action 'ignore
          :persistent-help "Do nothing")
        :buffer "*helm finder*"))

(provide 'helm-packages)

;;; helm-packages.el ends here
