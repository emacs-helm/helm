;;; helm-x-icons.el --- Provide compatibility between icons providers -*- lexical-binding: t -*-

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

(require 'helm-lib)

(defgroup helm-x-icons nil
  "Compatibility functions between icons provider packages."
  :group 'helm)

(defcustom helm-x-icons-provider 'all-the-icons
  "The icons provider package.
The currently supported providers are `all-the-icons' and `nerd-icons'.
Do not use setq to set this variable but customize."
  :type '(choice
          (const :tag "Use `all-the-icons' package" all-the-icons)
          (const :tag "Use `nerd-icons' package" nerd-icons))
  :set (lambda (var val)
         (when val (require val nil t))
         (set var val)))

(defun helm-x-icons-match-to-alist (file type)
  "Match FILE against an entry in ALIST using `string-match-p'.
Supported TYPE are ext, regexp, mode, url and dir."
  (cl-loop with alist = (helm-x-icons-resolve-alist type)
           for (elm . rest) in alist
           for target = (if (eq type 'url) file (helm-basename file))
           when (string-match-p (helm-stringify elm) target)
           return rest))

(defun helm-x-icons-resolve-alist (type)
  "Return the icon alist corresponding to TYPE.
The returned alist is computed according to `helm-x-icons-provider'."
  (let* ((provider-name (symbol-name helm-x-icons-provider))
         (alist-name (helm-acase type
                      (ext "extension-icon-alist")
                      (regexp "regexp-icon-alist")
                      (dir "dir-icon-alist")
                      (url "url-alist")
                      (mode "mode-icon-alist"))))
    (symbol-value
     (intern-soft (concat provider-name "-" alist-name)))))

(defun helm-x-icons-icon-for-file (&rest args)
  "Compatibility function for `*-icon-for-file'."
  (let ((fn (helm-acase helm-x-icons-provider
              (all-the-icons 'all-the-icons-icon-for-file)
              (nerd-icons 'nerd-icons-icon-for-file))))
    (when fn (apply fn args))))

(defvar helm-x-icons-nerd-icons-compat-alist
  '(("file-symlink-directory" . (nerd-icons-codicon . "nf-cod-file_symlink_directory"))
    ("file-directory" . (nerd-icons-sucicon . "nf-custom-folder_oct"))
    ("star" . (nerd-icons-mdicon . "nf-md-star"))
    ("mail-read" . (nerd-icons-codicon . "nf-cod-mail_read"))
    ("info" . (nerd-icons-faicon . "nf-fa-info"))
    ("link-external" . (nerd-icons-faicon . "nf-fa-external_link"))
    ("mail" . (nerd-icons-mdicon . "nf-md-email"))
    ("note_add" . (nerd-icons-codicon . "nf-cod-new_file"))
    ("create_new_folder" . (nerd-icons-codicon . "nf-cod-new_folder"))
    ("firefox" . (nerd-icons-faicon . "nf-fa-firefox"))
    ("globe" . (nerd-icons-faicon . "nf-fa-globe"))
    ("man-page" . (nerd-icons-octicon . "nf-oct-command_palette"))
    ("crop" . (nerd-icons-faicon . "nf-fa-crop"))
    ("package" . (nerd-icons-octicon . "nf-oct-package"))
    ("color_lens" . (nerd-icons-mdicon . "nf-md-color_lens"))
    ("cube" . (nerd-icons-mdicon . "nf-md-cube"))
    ("three-bars" . (nerd-icons-codicon . "nf-code-three_bars"))
    ("cog" . (nerd-icons-mdicon . "nf-md-cog"))
    ("lightning" . (nerd-icons-mdicon . "nf-md-lightning_bolt"))
    ("file" . (nerd-icons-mdicon . "nf-md-file"))
    ("folder" . (nerd-icons-mdicon . "nf-md-folder"))
    ("key" . (nerd-icons-faicon . "nf-fa-key"))
    ("angle-double-right" . (nerd-icons-faicon . "nf-fa-angle-double-right"))
    ("calculator" . (nerd-icons-mdicon . "nf-md-calculator"))
    ("book" . (nerd-icons-codicon . "nf-cod-book"))
    ("border_style" . (nerd-icons-mdicon . "nf-md-border_style"))
    ("text_fields" . (nerd-icons-mdicon . "nf-md-text"))
    ("code" . (nerd-icons-faicon . "nf-fa-code"))
    ("bar-chart" . (nerd-icons-faicon . "nf-fa-bar_chart"))
    ("clone" . (nerd-icons-faicon . "nf-fa-clone")))
  "The `nerd-icons' counterpart for icon names.
The `helm-x-icons-generic' function uses this alist to find `nerd-icons'
functions and names to display icons.
To each icon added here, its all-the-icons counterpart have to be added in
`helm-x-icons-all-the-icons-compat-alist'.")

(defvar helm-x-icons-all-the-icons-compat-alist
  '(("file-symlink-directory" . (all-the-icons-octicon . "file-symlink-directory"))
    ("file-directory" . (all-the-icons-octicon . "file-directory"))
    ("star" . (all-the-icons-octicon . "star"))
    ("mail-read" . (all-the-icons-octicon . "mail-read"))
    ("info" . (all-the-icons-octicon . "info"))
    ("link-external" . (all-the-icons-octicon . "link-external"))
    ("mail" . (all-the-icons-octicon . "mail"))
    ("note_add" . (all-the-icons-material . "note_add"))
    ("create_new_folder" . (all-the-icons-material . "create_new_folder"))
    ("firefox" . (all-the-icons-faicon . "firefox"))
    ("globe" . (all-the-icons-faicon . "globe"))
    ("man-page" . (all-the-icons-fileicon . "man-page"))
    ("crop" . (all-the-icons-material . "crop"))
    ("package" . (all-the-icons-octicon . "package"))
    ("color_lens" . (all-the-icons-material . "color_lens"))
    ("cube" . (all-the-icons-faicon . "cube"))
    ("three-bars" . (all-the-icons-octicon . "three-bars"))
    ("cog" . (all-the-icons-faicon . "cog"))
    ("lightning" . (all-the-icons-wicon . "lightning"))
    ("file" . (all-the-icons-faicon . "file"))
    ("folder" . (all-the-icons-faicon . "folder"))
    ("key" . (all-the-icons-octicon . "key"))
    ("angle-double-right" . (all-the-icons-faicon . "angle-double-right"))
    ("calculator" . (all-the-icons-faicon . "calculator"))
    ("book" . (all-the-icons-octicon . "book"))
    ("border_style" . (all-the-icons-material . "border_style"))
    ("text_fields" . (all-the-icons-material . "text_fields"))
    ("code" . (all-the-icons-material . "code"))
    ("bar-chart" . (all-the-icons-faicon . "bar-chart"))
    ("clone" . (all-the-icons-faicon . "clone")))
  "The `all-the-icons' counterpart for icon names.
The `helm-x-icons-generic' function uses this alist to find `all-the-icons'
functions and names to display icons.
To each icon added here, its all-the-icons counterpart have to be added in
`helm-x-icons-nerd-icons-compat-alist'.")

(defun helm-x-icons-generic (icon-name &rest args)
  "Compatibility function for icons.
Run an `all-the-icons' or `nerd-icons' function according to
`helm-x-icons-provider'and ICON-NAME.
Functions and icon names are found in `helm-x-icons-all-the-icons-compat-alist'
and `helm-x-icons-nerd-icons-compat-alist'."
  (let (fn)
    (helm-acase helm-x-icons-provider
      (nerd-icons
       (helm-acase (assoc-default icon-name helm-x-icons-nerd-icons-compat-alist)
        ((dst* (sym . name))
         (setq fn sym icon-name name))))
      (all-the-icons
       (helm-acase (assoc-default icon-name helm-x-icons-all-the-icons-compat-alist)
        ((dst* (sym . name))
         (setq fn sym icon-name name)))))
    (when fn (apply fn icon-name args))))


(provide 'helm-x-icons)

;;; helm-x-icons.el ends here
