;;; quite-buffer.el ---  QUIck Transparent Execution.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  David Greene

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

;;; This package provides a set of utilities for executing comands
;;; that display output in a buffer.  The buffer is named according to
;;; a provided function.  If a buffer with the given name already
;;; exists, it is reused.

;;; Code:

;;; Custom variables

(require 'quite)

;;; User-facing utilities

(defun quite-buffer-run-in-buffer-context (func buffer buffer-name)
  "Invoke FUNC within a buffer named BUFFER-NAME.  If BUFFER-NAME exists,
re-use it, otherwise run in the context of BUFFER.  FUNC may
create a new buffer in which case the buffer will be renamed to
BUFFER-NAME.  FUNC should return any new buffer created,
otherwise nil."
  (message (format "Running %s in buffer %s with name %s" func buffer buffer-name))
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

(defun quite-buffer-format (string spec-alist)
  "Format STRING using SPEC-ALIST.
SPEC-ALIST associates a string placeholder with a string or a
function that generates a string.  Wherever the placeholder is
used in STRING, the mapped string or value created by the mapped
generating function will be substituted.  For example:

(quite-buffer-format \"*%h-compile*\" ((?H \"my.host.com\") (?h \"my\") (?p 'buffer-file-name)))"
  (message (format "spec-alist: %s" spec-alist))
  (let ((mapped-spec-alist (mapcar (lambda (spec)
				     (let* ((char (nth 0 spec))
					    (value (nth 1 spec))
					    (mapped-value (if (functionp value)
							      (funcall functionp)
							    value)))
				       (list char mapped-value)))
				   spec-alist)))
    (message (format "mapped-spec-alist: %s" mapped-spec-alist))
    (format-spec string mapped-spec-alist)))

(provide 'quite-buffer)

;;; quite-buffer.el ends here
