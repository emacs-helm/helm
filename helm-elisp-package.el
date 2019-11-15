;;; helm-elisp-package.el --- helm interface for package.el -*- lexical-binding: t -*-

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
(require 'helm-help)
(require 'package)

(defgroup helm-el-package nil
  "helm elisp packages."
  :group 'helm)

(defcustom helm-el-package-initial-filter 'all
  "Show only installed, upgraded or all packages at startup."
  :group 'helm-el-package
  :type '(radio :tag "Initial filter for elisp packages"
          (const :tag "Show all packages" all)
          (const :tag "Show installed packages" installed)
          (const :tag "Show not installed packages" uninstalled)
          (const :tag "Show upgradable packages" upgrade)))

(defcustom helm-el-truncate-lines t
  "Truncate lines in helm-buffer when non--nil."
  :group 'helm-el-package
  :type 'boolean)

;; internals vars
(defvar helm-el-package--show-only 'all)
(defvar helm-el-package--initialized-p nil)
(defvar helm-el-package--tabulated-list nil)
(defvar helm-el-package--upgrades nil)
(defvar helm-el-package--removable-packages nil)

;; Shutup bytecompiler for emacs-24*
(defvar package-menu-async) ; Only available on emacs-25.
(declare-function async-byte-recompile-directory "ext:async-bytecomp.el")

(defun helm-el-package--init ()
  ;; In emacs-27 package-show-package-list returns an empty buffer
  ;; until package-initialize have been called.
  (unless (or package--initialized
              (null (boundp 'package-quickstart)))
    (package-initialize))
  (let (package-menu-async
        (inhibit-read-only t))
    (when (null package-alist)
      (setq helm-el-package--show-only 'all))
    (when (setq helm-el-package--removable-packages
                (package--removable-packages))
      (package-autoremove))
    (unwind-protect
         (progn
           (save-selected-window
             (if helm-el-package--initialized-p
                 ;; Use this as `list-packages' doesn't work
                 ;; properly (empty buffer) when called from lisp
                 ;; with 'no-fetch (emacs-25 WA).
                 (package-show-package-list)
               (when helm--force-updating-p (message "Refreshing packages list..."))
               (list-packages helm-el-package--initialized-p))
             (setq helm-el-package--initialized-p t)
             (message nil))
           (helm-init-candidates-in-buffer
               'global
             (with-current-buffer (get-buffer "*Packages*")
               (setq helm-el-package--tabulated-list tabulated-list-entries)
               (remove-text-properties (point-min) (point-max)
                                       '(read-only button follow-link category))
               (goto-char (point-min))
               (while (re-search-forward "^[ \t]+" nil t)
                 (replace-match ""))
               (buffer-string)))
           (setq helm-el-package--upgrades (helm-el-package-menu--find-upgrades))
           (if helm--force-updating-p
               (if helm-el-package--upgrades
                   (message "Refreshing packages list done, [%d] package(s) to upgrade"
                            (length helm-el-package--upgrades))
                 (message "Refreshing packages list done, no upgrades available"))
             (setq helm-el-package--show-only (if helm-el-package--upgrades
                                                  'upgrade
                                                helm-el-package-initial-filter))))
      (kill-buffer "*Packages*"))))

(defun helm-el-package-describe (candidate)
  (let ((id (get-text-property 0 'tabulated-list-id candidate)))
    (describe-package (package-desc-name id))))

(defun helm-el-package-visit-homepage (candidate)
  (let* ((id (get-text-property 0 'tabulated-list-id candidate))
         (pkg (package-desc-name id))
         (desc (cadr (assoc pkg package-archive-contents)))
         (extras (package-desc-extras desc))
         (url (and (listp extras) (cdr-safe (assoc :url extras)))))
    (if (stringp url)
        (browse-url url)
      (message "Package %s has no homepage"
               (propertize (symbol-name pkg)
                           'face 'font-lock-keyword-face)))))

(defun helm-el-run-visit-homepage ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-visit-homepage)))
(put 'helm-el-run-visit-homepage 'helm-only t)

(defun helm-elisp-package--pkg-name (pkg)
  (if (package-desc-p pkg)
      (package-desc-name pkg)
    pkg))

(defun helm-el-package-install-1 (pkg-list)
  (cl-loop with mkd = pkg-list
           for p in mkd
           for id = (get-text-property 0 'tabulated-list-id p)
           for name = (helm-elisp-package--pkg-name id)
           do (package-install id t)
           when (helm-aand (assq name package-alist)
                           (package-desc-dir (cadr it))
                           (file-exists-p it))
           collect id into installed-list and
           do (unless (package--user-selected-p name)
                (package--save-selected-packages
                 (cons name package-selected-packages)))
           finally do (message (format "%d packages installed:\n(%s)"
                                       (length installed-list)
                                       (mapconcat #'package-desc-full-name
                                                  installed-list ", ")))))

(defun helm-el-package-install (_candidate)
  (helm-el-package-install-1 (helm-marked-candidates)))

(defun helm-el-run-package-install ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-install)))
(put 'helm-el-run-package-install 'helm-only t)

(defun helm-el-package-uninstall-1 (pkg-list &optional force)
  (cl-loop with mkd = pkg-list
        for p in mkd
        for id = (get-text-property 0 'tabulated-list-id p)
        do
        (condition-case-unless-debug err
            (package-delete id force)
          (error (message (cadr err))))
        ;; Seems like package-descs are symbols with props instead of
        ;; vectors in emacs-27, use package-desc-name to ensure
        ;; compatibility in all emacs versions.
        unless (assoc (package-desc-name id) package-alist)
        collect id into delete-list
        finally do (if delete-list
                       (message (format "%d packages deleted:\n(%s)"
                                        (length delete-list)
                                        (mapconcat #'package-desc-full-name
                                                   delete-list ", ")))
                     "No package deleted")))

(defun helm-el-package-uninstall (_candidate)
  (helm-el-package-uninstall-1 (helm-marked-candidates) helm-current-prefix-arg))

(defun helm-el-run-package-uninstall ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-uninstall)))
(put 'helm-el-run-package-uninstall 'helm-only t)

(defun helm-el-package-menu--find-upgrades ()
  (cl-loop for entry in helm-el-package--tabulated-list
           for pkg-desc = (car entry)
           for status = (package-desc-status pkg-desc)
           ;; A dependency.
           when (string= status "dependency")
           collect pkg-desc into dependencies
           ;; An installed package used as dependency (user have
           ;; installed this package explicitely).
           when (package--used-elsewhere-p pkg-desc)
           collect pkg-desc into installed-as-dep
           ;; An installed package.
           when (member status '("installed" "unsigned"))
           collect pkg-desc into installed
           when (member status '("available" "new"))
           collect (cons (package-desc-name pkg-desc) pkg-desc) into available
           finally return
           ;; Always try to upgrade dependencies before installed.
           (cl-loop with all = (append dependencies installed-as-dep installed)
                    for pkg in all
                    for name = (package-desc-name pkg)
                    for avail-pkg = (assq name available)
                    when (and avail-pkg
                              (version-list-<
                               (package-desc-version pkg)
                               (package-desc-version (cdr avail-pkg))))
                    collect avail-pkg)))

(defun helm-el-package--user-installed-p (package)
  "Return non-nil if PACKAGE is a user-installed package."
  (let* ((assoc (assq package package-alist))
         (pkg-desc (and assoc (cadr assoc)))
         (dir (and pkg-desc (package-desc-dir pkg-desc))))
    (when dir
      (file-in-directory-p dir package-user-dir))))

(defun helm-el-package-upgrade-1 (pkg-list)
  (cl-loop for p in pkg-list
           for pkg-desc = (car p)
           for pkg-name = (package-desc-name pkg-desc)
           for upgrade = (cdr (assq pkg-name
                                    helm-el-package--upgrades))
           do
           (cond (;; Install.
                  (equal pkg-desc upgrade)
                  (message "Installing package `%s'" pkg-name)
                  (package-install pkg-desc t))
                 (;; Do nothing.
                  (or (null upgrade)
                      ;; This may happen when a Elpa version of pkg
                      ;; is installed and need upgrade and pkg is as
                      ;; well a builtin package.
                      (package-built-in-p pkg-name))
                  (ignore))
                 (;; Delete.
                  t
                  (message "Deleting package `%s'" pkg-name)
                  (package-delete pkg-desc t t)))))

(defun helm-el-package-upgrade (_candidate)
  (helm-el-package-upgrade-1
   (cl-loop with pkgs = (helm-marked-candidates)
            for p in helm-el-package--tabulated-list
            for pkg = (car p)
            if (member (symbol-name (package-desc-name pkg)) pkgs)
            collect p)))

(defun helm-el-run-package-upgrade ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-upgrade)))
(put 'helm-el-run-package-upgrade 'helm-only t)

(defun helm-el-package-upgrade-all ()
  (if helm-el-package--upgrades
      (with-helm-display-marked-candidates
        helm-marked-buffer-name (helm-fast-remove-dups
                                 (mapcar (lambda (x) (symbol-name (car x)))
                                         helm-el-package--upgrades)
                                 :test 'equal)
        (when (y-or-n-p "Upgrade all packages? ")
          (helm-el-package-upgrade-1 helm-el-package--tabulated-list)))
      (message "No packages to upgrade actually!")))

(defun helm-el-package-upgrade-all-action (_candidate)
  (helm-el-package-upgrade-all))

(defun helm-el-run-package-upgrade-all ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-upgrade-all-action)))
(put 'helm-el-run-package-upgrade-all 'helm-only t)

(defun helm-el-package--transformer (candidates _source)
  (cl-loop for c in candidates
           for disp = (concat "  " c)
           for id = (get-text-property 0 'tabulated-list-id c)
           for name = (and id (package-desc-name id))
           for desc = (package-desc-status id)
           for built-in-p = (and (package-built-in-p name)
                                 (not (member desc '("available" "new"
                                                     "installed" "dependency"))))
           for installed-p = (member desc '("installed" "dependency"))
           for upgrade-p = (assq name helm-el-package--upgrades)
           for user-installed-p = (memq name package-selected-packages)
           do (when (and user-installed-p (not upgrade-p))
                (put-text-property 0 2 'display "S " disp))
           do (when (or (memq name helm-el-package--removable-packages)
                        (and upgrade-p installed-p))
                (put-text-property 0 2 'display "U " disp)
                (put-text-property
                 2 (+ (length (symbol-name name)) 2)
                 'face 'font-lock-variable-name-face disp))
           do (when (and upgrade-p (not installed-p) (not built-in-p))
                (put-text-property 0 2 'display "I " disp))
           for cand = (cons disp (car (split-string disp)))
           when (or (and built-in-p
                         (eq helm-el-package--show-only 'built-in))
                    (and upgrade-p
                         (eq helm-el-package--show-only 'upgrade))
                    (and installed-p
                         (eq helm-el-package--show-only 'installed))
                    (and (not installed-p)
                         (not built-in-p)
                         (eq helm-el-package--show-only 'uninstalled))
                    (eq helm-el-package--show-only 'all))
           collect cand))

(defun helm-el-package-show-built-in ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'built-in)
    (helm-update)))
(put 'helm-el-package-show-built-in 'helm-only t)

(defun helm-el-package-show-upgrade ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'upgrade)
    (helm-update)))
(put 'helm-el-package-show-upgrade 'helm-only t)

(defun helm-el-package-show-installed ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'installed)
    (helm-update)))
(put 'helm-el-package-show-installed 'helm-only t)

(defun helm-el-package-show-all ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'all)
    (helm-update)))
(put 'helm-el-package-show-all 'helm-only t)

(defun helm-el-package-show-uninstalled ()
  (interactive)
  (with-helm-alive-p
    (setq helm-el-package--show-only 'uninstalled)
    (helm-update)))
(put 'helm-el-package-show-uninstalled 'helm-only t)

(defvar helm-el-package-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-I")   'helm-el-package-show-installed)
    (define-key map (kbd "M-O")   'helm-el-package-show-uninstalled)
    (define-key map (kbd "M-U")   'helm-el-package-show-upgrade)
    (define-key map (kbd "M-B")   'helm-el-package-show-built-in)
    (define-key map (kbd "M-A")   'helm-el-package-show-all)
    (define-key map (kbd "C-c i") 'helm-el-run-package-install)
    (define-key map (kbd "C-c r") 'helm-el-run-package-reinstall)
    (define-key map (kbd "C-c d") 'helm-el-run-package-uninstall)
    (define-key map (kbd "C-c u") 'helm-el-run-package-upgrade)
    (define-key map (kbd "C-c U") 'helm-el-run-package-upgrade-all)
    (define-key map (kbd "C-c @") 'helm-el-run-visit-homepage)
    map))

(defvar helm-source-list-el-package nil)
(defclass helm-list-el-package-source (helm-source-in-buffer)
  ((init :initform 'helm-el-package--init)
   (get-line :initform 'buffer-substring)
   (filtered-candidate-transformer :initform 'helm-el-package--transformer)
   (action-transformer :initform 'helm-el-package--action-transformer)
   (help-message :initform 'helm-el-package-help-message)
   (keymap :initform helm-el-package-map)
   (update :initform 'helm-el-package--update)
   (candidate-number-limit :initform 9999)
   (action :initform '(("Describe package" . helm-el-package-describe)
                       ("Visit homepage" . helm-el-package-visit-homepage)))
   (group :initform 'helm-el-package)))

(defun helm-el-package--action-transformer (actions candidate)
  (let* ((pkg-desc (get-text-property 0 'tabulated-list-id candidate))
         (status (package-desc-status pkg-desc))
         (pkg-name (package-desc-name pkg-desc))
         (built-in (and (package-built-in-p pkg-name)
                        (not (member status '("available" "new"
                                              "installed" "dependency")))))
         (acts (if helm-el-package--upgrades
                   (append actions '(("Upgrade all packages"
                                      . helm-el-package-upgrade-all-action)))
                   actions)))
    (cond (built-in '(("Describe package" . helm-el-package-describe)))
          ((and (package-installed-p pkg-name)
                (cdr (assq pkg-name helm-el-package--upgrades))
                (member status '("installed" "dependency")))
           (append '(("Upgrade package(s)" . helm-el-package-upgrade)
                     ("Uninstall package(s)" . helm-el-package-uninstall))
                   acts))
          ((and (package-installed-p pkg-name)
                (cdr (assq pkg-name helm-el-package--upgrades))
                (string= status "available"))
           (append '(("Upgrade package(s)" . helm-el-package-upgrade))
                   acts))
          ((and (package-installed-p pkg-name)
                (or (null (package-built-in-p pkg-name))
                    (and (package-built-in-p pkg-name)
                         (assq pkg-name package-alist))))
           (append acts '(("Reinstall package(s)" . helm-el-package-reinstall)
                          ("Recompile package(s)" . helm-el-package-recompile)
                          ("Uninstall package(s)" . helm-el-package-uninstall))))
          (t (append acts '(("Install packages(s)" . helm-el-package-install)))))))

(defun helm-el-package--update ()
  (setq helm-el-package--initialized-p nil))

(defun helm-el-package-recompile (_pkg)
  (cl-loop for p in (helm-marked-candidates)
           do (helm-el-package-recompile-1 p)))

(defun helm-el-package-recompile-1 (pkg)
  (let* ((pkg-desc (get-text-property 0 'tabulated-list-id pkg))
         (dir (package-desc-dir pkg-desc)))
    (async-byte-recompile-directory dir)))

(defun helm-el-package-reinstall (_pkg)
  (cl-loop for p in (helm-marked-candidates)
           for pkg-desc = (get-text-property 0 'tabulated-list-id p)
           do (helm-el-package-reinstall-1 pkg-desc)))

(defun helm-el-package-reinstall-1 (pkg-desc)
  (let ((name (package-desc-name pkg-desc)))
    (package-delete pkg-desc 'force 'nosave)
    ;; pkg-desc contain the description
    ;; of the installed package just removed
    ;; and is BTW no more valid.
    ;; Use the entry in package-archive-content
    ;; which is the non--installed package entry.
    ;; For some reason `package-install'
    ;; need a pkg-desc (package-desc-p) for the build-in
    ;; packages already installed, the name (as symbol)
    ;; fails with such packages.
    (package-install
     (cadr (assq name package-archive-contents)) t)))

(defun helm-el-run-package-reinstall ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'helm-el-package-reinstall)))
(put 'helm-el-run-package-reinstall 'helm-only t)

;;;###autoload
(defun helm-list-elisp-packages (arg)
  "Preconfigured helm for listing and handling emacs packages."
  (interactive "P")
  (when arg (setq helm-el-package--initialized-p nil))
  (unless helm-source-list-el-package
    (setq helm-source-list-el-package
          (helm-make-source "list packages" 'helm-list-el-package-source)))
  (helm :sources 'helm-source-list-el-package
        :truncate-lines helm-el-truncate-lines
        :full-frame t
        :buffer "*helm list packages*"))

;;;###autoload
(defun helm-list-elisp-packages-no-fetch (arg)
  "Preconfigured helm for emacs packages.

Same as `helm-list-elisp-packages' but don't fetch packages on remote.
Called with a prefix ARG always fetch packages on remote."
  (interactive "P")
  (let ((helm-el-package--initialized-p (null arg)))
    (helm-list-elisp-packages nil)))

(provide 'helm-elisp-package)

;;; helm-elisp-package.el ends here
