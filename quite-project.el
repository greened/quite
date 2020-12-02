;;; quite-project.el ---  QUIck Transparent Execution.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  David A. Greene

;; Author: David Greene <greened@obbligato.org>
;; Keywords: processes, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; This package provides a set of utilities for executing comands at
;;; a project level.  A project is defined by a root directory and
;;; "key files," the presence of which in the directory form a valid
;;; project.

;;; Code:

(require 'quite)

;;;###autoload
(defcustom quite-project-descriptors nil
  "List of project descriptors.  A descriptor is a plist
  consisting of:

:project-dir  Project directory name, appended to members of :root-list to form a potential project path.
:root-list    List of directories to search for :project-dir.
:key-files    List of files to look for in /:root-list/:project-dir.  Presence of a file signals a valid project directory.
:function     Function to invoke."
  :type '(repeat
	  (plist
	   :options ((:project-dir string)
		     (:root-list (repeat string))
		     (:key-files (repeat string))
		     (:function function))))
  :group 'quite-project)

(defun quite-project--file-exists-p (project-root key-files)
  "See if one of KEY-FILES exists under PROJECT-ROOT and return
the full path to it, nil otherwise."
  (catch 'found
    (dolist (key-file key-files)
      (let ((root-key-file (concat project-root "/" key-file)))
	(when (file-exists-p root-key-file)
	  (throw 'found root-key-file))))))

(defun quite-project--path-for-buffer (project-dir key-files)
  "Return the project root source path for the current buffer, or
nil if the buffer isn't associated with a project source file.
KEY-FILES is a list of files to look for in PROJECT-DIR.
Intermediate directories between PROJECT-DIR and KEY-FILES are
allowed."
  (let ((buffer-file (buffer-file-name)))
    (if buffer-file
        (if (string-match (format "\\(.*/%s\\)/" project-dir)
                          buffer-file)
            (catch 'found
              (dolist (key-file key-files)
                (let ((found-file (locate-dominating-file buffer-file key-file)))
                  (when found-file
                    (throw 'found (file-name-directory found-file))))))
          nil)
      nil)))

(defvar quite-project--root-list nil)

(defun quite-project--prompt-for-root ()
  "Prompt the user for a project root, with completion."
  (quite--read-string "Project root: "
		      'quite-project--root-list))

;; User-facing utilities

(defun quite-project-find-key-files-buffer (project-root key-files)
  "See if one of KEY-FILES exists under PROJECT-ROOT and return a
buffer for it.  Return nil otherwise."
  (catch 'found
    (dolist (key-file key-files)
      (let* ((root-key-file (concat project-root "/" key-file))
	     (buffer (find-file root-key-file)))
	(when buffer (throw 'found buffer))))))

(defun quite-project-find-project (project-dir host root-list key-files)
  "Check directories in ROOT-LIST on HOST for PROJECT_DIR and return one if found,
prompt otherwise.  Ensure that one of KEY-FILES is in the
returned root.  The returned ROOT is a path on the remote HOST,
without the remote prefix."
  (let ((the-root
         (quite-project--path-for-buffer project-dir key-files)))
    (if (not the-root)
        (let ((remote-prefix
               ;; FIXME: Don't hard-code method.
               (when (not (string-equal host (system-name)))
                 (concat "/ssh:" host ":"))))
          (let ((found-root
                 (catch 'found
                   (dolist (root root-list)
                     (dolist (file key-files)
                       (let* ((try-root (concat root "/" project-dir))
                              (remote-try-root (concat remote-prefix try-root)))
                         (when (file-exists-p (concat remote-try-root "/" file))
                           (throw 'found try-root)))))
                   ;; Did not find a project in the given remote, prompt for one.
                   (let* ((root (quite-project--prompt-for-root))
                          (try-root (concat root "/" project-dir))
                          (remote-try-root (concat remote-prefix try-root)))
                     (dolist (file key-files)
                       (when (file-exists-p (concat remote-try-root "/" file))
                         (throw 'found try-root)))))))
            (if (not found-root)
                (error (format "%s does not exist in %s with %s on %s"
                               project-dir root-list key-files host)))))
      the-root)))

(defun quite-project-parse-descriptor (descriptor)
  "Parse DESCRIPTOR, returning a list (project-dir root-list
key-files-list)"
  (let ((project-dir (plist-get descriptor :project-dir))
	(root-list (plist-get descriptor :root-list))
	(key-files-list (plist-get descriptor :key-files)))
    (list project-dir root-list key-files-list)))

(provide 'quite-project)

;;; quite-project.el ends here
