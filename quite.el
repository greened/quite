;;; quite.el ---  QUIck Transparent Execution.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  David A. Greene

;; Author: David Greene <greened@obbligato.org>
;; Keywords: processes, tools
;; Version: 0.0.1
;; URL: http://github.com/greened/quite

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

;; This package provides two functions.  The first, lower-level function is
;; quite-execute.

;; among a list of commands to run based on the given prefix argument.
;; For example C-u C-u M-x `quite-dispatch' will run the second
;; command in the list.  C-u 3 M-x `quite-dispatch' will run the third
;; command, as will C-u C-u C-u M-x `quite-dispatch'.
;;
;; While this is mostly intended to be used as a library to invoke
;; from interactive commands, it is possible to set up a default list
;; of commands and invoke execution via C-c C-q (`quite-execute').
;;
;; `quite-dispatch' will pass the prefix argument and the host
;; associated with the curent buffer.  The host is either a remote
;; host if the buffer is associated with a remote, the local host if
;; the buffer is local or a host determined by a default-providing
;; function.

;;; Code:
;;; Custom variables

(defgroup quite nil
  "Convenient command invocations."
  :tag "Quite"
  :group 'tools
  :group 'processes)

;;;###autoload
(defcustom quite-descriptors nil
  "List of function descriptors.  A descriptor is a plist
consisting of:

:function           Function to invoke

Host detection is based on the current buffer.  If the current
buffer is associated with a remote, the host of the remote is
used.  Otherwise if the current buffer is associated with a file,
the local host is used.  Otherwise if :prompt is non-nil, the user
is prompted for a host to use.  Otherwise :default-host-func is
invoked with the current buffer to determine the host."
  :type '(repeat
	  (plist
	   :options ((:function function))))
  :group 'quite)

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

;;; Implementation

(defvar quite-remote--host-list nil)

(defun quite-remote--prompt-for-host ()
  "Prompt the user for a host, with completion."
  (quite--read-string "Host: " 'quite-remote--host-list))

(defun quite-remote--strip-host (path)
  "Remove the method/host prefix from PATH if present."
  ;; FIXME: Don't hard-code the method.
  (replace-regexp-in-string
   "^\\(/ssh:\\([[:alnum:]]+@\\)?[[:alnum:]]+:\\)"
   ""
   path))

;;; User-facing utlities

(defun quite--prefix-arg-index (arg)
  "Calculate the index into the command list given the raw prefix
ARG."
  (if (consp arg)
      (truncate
       (log (car arg) 4))
    (if arg
	arg
      0)))

(defun quite--read-string (prompt history)
  "Use PROMPT to prompt the user for some input, with history
from HISTORY.  HISTORY has the same use as in `read-string'."
  (read-string prompt nil history history))

(defun quite--extract-subdir (root project-dir)
  "Return the path component below PROJECT-DIR in ROOT."
  (when (string-match (format "%s/\\([^/]+/\\)*" project-dir) root)
    (let ((result (match-string 1 root)))
      ;; Remove the trailing /.
      (substring result 0 (1- (length result))))))

(defun quite--doit (tag func)
  "Run FUNC passing tag TAG."
  (funcall func tag))

(defun quite--dispatch (parg &rest dispatch-entries)
  "Invoke a function from DISPATCH-ENTRIES chosen by prefix
argument PARG.  DISPATCH-ENTRIES is a plist of the form:

(:function FUNC :tag TAG)

where FUNC is a function accepting TAG.  The chosen
function is determined by PARG:

nil/0 -> invoke function #1
4     -> invoke function #2
16    -> invoke function #3
64    -> invoke function #4

This corresponds to prefix argument values passed by '' 'C-u' 'C-u C-u' and so
on.

Typical use of `quite--dispatch' is:
  (defun command1-func (tag)
    (message \"command1 %s\" tag))
  (defun command2-func (tag)
    (message \"command2 %s\" tag))

  (setq command1
        '(:function 'command1-func :tag \"command1\"))
  (setq command2
        '(:function 'command2-func :tag \"command2\"))

  (defun do-quite (parg)
    (interactive \"P\")
    (quite-dispatch parg
                    command1
                    command2))

"
  (save-excursion
    (let* ((index (quite--prefix-arg-index parg))
	   (entry (nth index dispatch-entries)))
      (when (not entry)
	(error (format "No entry for prefix %s" index)))
      (let ((func (plist-get entry :function))
            (tag (plist-get entry :tag)))
	(quite--doit tag func)))))

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
      (quite-remote--strip-host the-root))))

(defun quite-project-parse-descriptor (descriptor)
  "Parse DESCRIPTOR, returning a list (project-dir root-list
key-files-list)"
  (let ((project-dir (plist-get descriptor :project-dir))
	(root-list (plist-get descriptor :root-list))
	(key-files-list (plist-get descriptor :key-files)))
    (list project-dir root-list key-files-list)))

(defun quite--run-project-remote (func descriptor tag)
  "Run FUNC passing args from project DESCRIPTOR and TAG on the
remote associated with the current buffer.  FUNC is expected to
take host, project root a possibly-nil subdir and buffer
arguments before TAG:

(FUNC host project-root subdir buffer TAG)

The host, project-root subdir and buffer are determined from a combination of
project descriptor entires and the oath of the current buffer."
  (let* ((project-config (quite-project-parse-descriptor descriptor))
	 (project-dir (nth 0 project-config))
	 (root-list (nth 1 project-config))
	 (key-files-list (nth 2 project-config))
	 (host (quite-remote-host-for-current-buffer
		t 'quite-remote-localhost))
	 (root (quite-project-find-project
		project-dir host root-list key-files-list))
	 (buffer (quite-project-find-key-files-buffer
		  (quite-remote-create-remote-path host root) key-files-list))
         (subdir (quite--extract-subdir root project-dir)))
    (funcall func host root subdir buffer tag)))

(defun quite--generate-invoker (descriptor func)
  "Return a function to invoke FUNC passing the contents of
DESCRIPTOR and a tag as arguments."
  (lexical-let ((loc-descriptor descriptor)
                (loc-func func))
    (lambda (tag)
      (quite--run-project-remote
       loc-func
       loc-descriptor
       tag))))

(defun quite--generate-dispatch-table (project-descriptor tag-function-alist)
  "Given a PROJECT-DESCRIPTOR and a TAG-FUNCTION-ALIST, create a
  dispatch table that will be indexed by a prefix argument.
  Functions in FUNCTION-LIST should accept PROJECT-DESCRIPTOR
  values as well as the dispatch tag as arguments.
  TAG-FUNCTION-ALIST associates a dispatch tag with each
  function."
  (mapcar (lambda (tag-func-pair)
            (lexical-let* ((tag (nth 0 tag-func-pair))
                           (func (nth 1 tag-func-pair))
                           (invoker (quite--generate-invoker
                                     project-descriptor
                                     func)))
              (list :function (lambda (tag)
                                (funcall invoker tag))
                    :tag tag)))
          tag-function-alist))

(defun quite--run-in-buffer-context (func buffer buffer-name)
  "Invoke FUNC within a buffer named BUFFER-NAME.  If BUFFER-NAME exists,
re-use it, otherwise run in the context of BUFFER.  FUNC may
create a new buffer in which case the buffer will be renamed to
BUFFER-NAME.  FUNC should return any new buffer created,
otherwise nil."
  (let ((existing-buffer (get-buffer buffer-name)))
    (if existing-buffer
	(progn
	  (set-buffer existing-buffer)
	  (funcall func))
      ;; Use the provided buffer.
      (progn

	(set-buffer buffer)
	(let ((new-buffer (funcall func)))
	  (if new-buffer
	      (set-buffer new-buffer)))
	(rename-buffer buffer-name)))))

(defun quite--generate-buffer-action (command-func buffer-name-func)
  "Given COMMAND-FUNC and BUFFER-NAME-FUNC, invoke
COMMAND-FUNC within a buffer named by the return value of
BUFFER-NAME-FUNC.  Both functions should have the following
signature:

(func host rootdir subdir buffer tag)
"
  (lexical-let ((loc-command-func command-func)
                (loc-name-func buffer-name-func))
    (lambda (host root subdir buffer tag)
      ;; Needed?
      (lexical-let ((loc-host host)
                    (loc-root root)
                    (loc-subdir subdir)
                    (loc-buffer buffer)
                    (loc-tag tag))
        (quite--run-in-buffer-context
         ;; Function to run
         (lambda ()
           (funcall loc-command-func host root subdir buffer tag))
         ;; Buffer to run in
         buffer
         ;; Buffer name
         (funcall loc-name-func host root subdir buffer tag))))))

(defun quite--buffer-format (string spec-alist)
  "Format STRING using SPEC-ALIST.
SPEC-ALIST associates a string placeholder with a string or a
function that generates a string.  Wherever the placeholder is
used in STRING, the mapped string or value created by the mapped
generating function will be substituted.  For example:

(quite-buffer-format \"*%h-compile*\" ((?H \"my.host.com\") (?h \"my\") (?p 'buffer-file-name)))"
  (let ((mapped-spec-alist (mapcar (lambda (spec)
				     (let* ((char (nth 0 spec))
					    (value (nth 1 spec))
					    (mapped-value (if (functionp value)
							      (funcall functionp)
							    value)))
				       (list char mapped-value)))
				   spec-alist)))
    (format-spec string mapped-spec-alist)))


;;; User-facing utilities

;;;###autoload
(defun quite-remote-create-remote-path (host path)
  "Take local path PATH and create a remote path for it on HOST."
  ;; FIXME: Don't hard-code the method.
  (concat "/ssh:" host ":" path))

;;;###autoload
(defun quite-remote-host-for-current-buffer (prompt default-host-func)
  "Return the host if the current buffer is associated with a
remote file, the local host if the current buffer is associated
with a local file, prompt for user input otherwise."
  (let ((buffer-file (buffer-file-name)))
    (if buffer-file
	(let ((host (file-remote-p buffer-file 'host)))
	  (if host
	      host
	    (system-name)))
      (if prompt
	  (quite-remote--prompt-for-host)
	(funcall default-host-func (current-buffer))))))

;;;###autoload
(defun quite-remote-localhost (buffer)
  "Return the local host name.  This is a convenience function
for use as a :default-host-func when specifying
`quite-remote-descriptors'"
  (system-name))

;;;###autoload
(defun quite-generate-dispatcher (project-descriptor tag-function-alist)
  "Given a dispatcher TAG-FUNCTION-ALIST, generate a function
  that dispatches to the appropriate function when passed a
  prefix argument. TAG-FUNCTION-ALIST assoicates a tag with a
  function to call, passing that tag as its last argument."
  (lexical-let ((dispatch-list
                 (quite--generate-dispatch-table
                  project-descriptor tag-function-alist)))
    (lambda (parg)
      (interactive "P")
      (save-excursion
       (apply #'quite--dispatch
              parg
              dispatch-list)))))

;;;###autoload
(defun quite-generate-buffer-dispatcher (project-descriptor
                                         buffer-name-func
                                         tag-function-alist)
  "Given a dispatcher TAG-FUNCTION-ALIST, generate a function
  that dispatches to the appropriate function when passed a
  prefix argument. The function is called in the context of a
  buffer which will be named by BUFFER-NAME-FUNC.
  TAG-FUNCTION-ALIST assoicates a tag with a function to call,
  passing that tag as its last argument.

Both functions should have the following signature:

(func host rootdir subdir buffer tag)
"
  (lexical-let ((new-tag-function-alist
                  ;; tag-function-alist contains the function to ultimately
                  ;; execute.  Wrap it in a routine that executes it in a buffer
                  ;; context.
                  (mapcar (lambda (tag-func-pair)
                            (lexical-let ((tag (nth 0 tag-func-pair))
                                          (command-func (nth 1 tag-func-pair)))
                              `(,tag
                                ,(quite--generate-buffer-action
                                  command-func buffer-name-func))))
                          tag-function-alist)))
    (quite-generate-dispatcher project-descriptor new-tag-function-alist)))

;;;###autoload
(defun quite-execute (parg)
  "Invoke a function according to prefix argument PARG,
using a list of descriptors configured in
`quite-descriptors'.

quite-execute examines a "dispatch table" and selects one
accoring to the prefix argument passed to it.  It then invokes
the selected function, passing the prefix argument to it.  The
dispatch table is of the form:
"
  (interactive "P")
  (quite-dispatch parg quite-descriptors))

(provide 'quite)

;;; quite.el ends here
