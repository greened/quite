;;; quite.el ---  QUIck Transparent Execution.  -*- lexical-binding: t; -*-

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

;; This package provides a function `quite-dispatch' to choose from
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
(require 'quite-buffer)
(require 'quite-project)
(require 'quite-remote)

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

;;; Implementation

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

(defun quite--doit (parg func)
  "Run FUNC passing prefix argument PARG."
  (funcall func parg))

;;; User-facing utilities

;;;###autoload
(defun quite-dispatch (parg &rest descriptors)
  "Invoke a build on a descriptor from DESCRIPTORS chosen by
prefix argument PARG.  See `quite-descriptors' for
an in-depth explanation of the descriptor format.

Typical use of `quite-dispatch' is:
  (defun command1-func ()
    (message \"command1\"))
  (defun command2-func ()
    (message \"command2\"))

  (setq command1
        '(:function 'command1-func))
  (setq command2
        '(:function 'command2-func))

  (defun do-quite (parg)
    (interactive \"P\")
    (quite-dispatch parg
                    command1
                    command2)))

"
  (save-excursion
    (let* ((index (quite--prefix-arg-index parg))
	   (descriptor (nth index descriptors)))
      (when (not descriptor)
	(error (format "No entry for prefix %s" index)))
      (let ((func (plist-get descriptor :function)))
	(quite--doit parg func)))))

;;;###autoload
(defun quite-execute (parg)
  "Invoke a function according to prefix argument PARG,
using a list of descriptors configured in
`quite-descriptors'."
  (interactive "P")
  (quite-dispatch parg quite-descriptors))

(defun quite-run-project-remote (func descriptor &rest args)
  "Run FUNC passing args from project DESCRIPTOR and ARGS on the
remote associated with the current buffer.  FUNC is expected to
take host, project root a possibly-nil subdir and buffer
arguments before ARGS."
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
         (subdir (extract-subdir root project-dir)))
    (apply func host root subdir buffer args)))


(provide 'quite)

;;; quite.el ends here
