;;; quite-remote.el ---  QUIck Transparent Execution.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  David Greene

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

;; This package provides a function `quite-remote-dispatch' to choose from
;; among a list of commands to run based on the given prefix argument.
;; For example C-u C-u M-x `quite-remote-dispatch' will run the second
;; command in the list.  C-u 3 M-x `quite-remote-dispatch' will run the third
;; command, as will C-u C-u C-u M-x `quite-remote-dispatch'.
;;
;; While this is mostly intended to be used as a library to invoke
;; from interactive commands, it is possible to set up a default list
;; of commands and invoke execution via C-c C-q (`quite-remote-execute').
;;
;; `quite-remote-dispatch' will pass the prefix argument and the host
;; associated with the curent buffer.  The host is either a remote
;; host if the buffer is associated with a remote, the local host if
;; the buffer is local or a host determined by a default-providing
;; function.

;;; Code:

(require 'quite)

;;; Implementation

(defvar quite-remote--host-list nil)

(defun quite-remote--prompt-for-host ()
  "Prompt the user for a host, with completion."
  (quite--read-string "Host: " 'quite-remote--host-list))

;;; User-facing utlities

(defun quite-remote-create-remote-path (host path)
  "Take local path PATH and create a remote path for it on HOST."
  ;; FIXME: Don't hard-code the method.
  (concat "/ssh:" host ":" path))

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

(defun quite-remote-localhost (buffer)
  "Return the local host name.  This is a convenience function
for use as a :default-host-func when specifying
`quite-remote-descriptors'"
  (system-name))

(provide 'quite-remote)

;;; quite-remote.el ends here
