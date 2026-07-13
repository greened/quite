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

:project-dir  Project directory name, appended to members of :root-list
              to form a potential project path.
:root-list    List of directories to search for :project-dir.
:key-files    List of files to look for in /:root-list/:project-dir.
              Presence of a file signals a valid project directory.
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
   "^\\(/ssh:\\([-._[:alnum:]]+@\\)?[-._[:alnum:]]+:\\)"
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
  "Return the path below PROJECT-DIR within ROOT.
Return the empty string when ROOT ends at PROJECT-DIR, or nil when
PROJECT-DIR does not appear in ROOT.  A trailing slash on ROOT is
ignored.  With PROJECT-DIR \"project\":
  \"/a/project/sub\" -> \"sub\"
  \"/a/project/x/y\" -> \"x/y\"
  \"/a/project\"     -> \"\"."
  (let ((root (directory-file-name root)))
    (when (string-match (format "\\(?:^\\|/\\)%s\\(?:/\\(.*\\)\\)?$"
                                (regexp-quote project-dir))
                        root)
      (or (match-string 1 root) ""))))

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

This corresponds to prefix arguments: no prefix, C-u, C-u C-u, and so on.

Typical use of `quite--dispatch' is:
  (defun command1-func (tag)
    (message \"command1 %s\" tag))
  (defun command2-func (tag)
    (message \"command2 %s\" tag))

  (setq command1
        (list :function (function command1-func) :tag \"command1\"))
  (setq command2
        (list :function (function command2-func) :tag \"command2\"))

  (defun do-quite (parg)
    (interactive \"P\")
    (quite--dispatch parg
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
  (lambda (tag)
    (quite--run-project-remote func descriptor tag)))

(defun quite--generate-dispatch-table (project-descriptor tag-function-alist)
  "Given a PROJECT-DESCRIPTOR and a TAG-FUNCTION-ALIST, create a
  dispatch table that will be indexed by a prefix argument.
  Functions in FUNCTION-LIST should accept PROJECT-DESCRIPTOR
  values as well as the dispatch tag as arguments.
  TAG-FUNCTION-ALIST associates a dispatch tag with each
  function."
  (mapcar (lambda (tag-func-pair)
            (let* ((tag (nth 0 tag-func-pair))
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
  (lambda (host root subdir buffer tag)
    (quite--run-in-buffer-context
     ;; Function to run
     (lambda ()
       (funcall command-func host root subdir buffer tag))
     ;; Buffer to run in
     buffer
     ;; Buffer name
     (funcall buffer-name-func host root subdir buffer tag))))

(defun quite--buffer-format (string spec-alist)
  "Format STRING using SPEC-ALIST.
SPEC-ALIST associates a string placeholder with a string or a
function that generates a string.  Wherever the placeholder is
used in STRING, the mapped string or value created by the mapped
generating function will be substituted.  For example:

(quite--buffer-format \"*%h-compile*\" ((?H \"my.host.com\") (?h \"my\") (?p (function buffer-file-name))))"
  (let ((mapped-spec-alist (mapcar (lambda (spec)
				     (let* ((char (nth 0 spec))
					    (value (nth 1 spec))
					    (mapped-value (if (functionp value)
							      (funcall value)
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
(defun quite-remote-localhost (_buffer)
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
  (let ((dispatch-list
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
  (let ((new-tag-function-alist
         ;; tag-function-alist contains the function to ultimately
         ;; execute.  Wrap it in a routine that executes it in a buffer
         ;; context.
         (mapcar (lambda (tag-func-pair)
                   (let ((tag (nth 0 tag-func-pair))
                         (command-func (nth 1 tag-func-pair)))
                     `(,tag
                       ,(quite--generate-buffer-action
                         command-func buffer-name-func))))
                 tag-function-alist)))
    (quite-generate-dispatcher project-descriptor new-tag-function-alist)))

;;;###autoload
(defun quite-execute (parg)
  "Invoke a function chosen by prefix argument PARG.
Dispatches among the entries of `quite-descriptors' with
`quite--dispatch': PARG selects which descriptor's :function to
invoke (see `quite--dispatch' for the prefix-argument-to-index
mapping)."
  (interactive "P")
  (apply #'quite--dispatch parg quite-descriptors))



;;; Project build composition

;; The functions below turn a compact PROJECT spec into (a) bindings in
;; `quite-command-map' and (b) hydra heads for a project's build commands.  A PROJECT is a
;; plist; see `quite-define-project' for the keys.  `quite-bind-project-commands'
;; and `quite-project-hydra-heads' are independent -- neither depends on the
;; other having run -- because each derives its own dispatcher from the spec
;; (dispatchers only need to be equivalent, not the same object).

(defun quite--make-build-command (command git-project-name &optional prefix postfix)
  "Return a build function that compiles a git-project COMMAND.
The returned function has the quite command signature
\(HOST ROOT SUBDIR BUFFER TAG) and, when invoked, runs
\"PREFIX git GIT-PROJECT-NAME COMMAND TAG POSTFIX\" via `compile'."
  (let ((template (format "%s git %s %%s %%s %s"
                          (or prefix "") git-project-name (or postfix ""))))
    (lambda (_host _root _subdir _buffer tag)
      (compile (format template command tag)))))

(defun quite--make-buffer-name (project-name name)
  "Return a function naming the compilation buffer for build NAME.
The returned function has the quite command signature
\(HOST ROOT SUBDIR BUFFER TAG); HOST is shortened to its first dotted
component."
  (lambda (host _root subdir _buffer tag)
    (format "*%s-%s-%s-%s-%s*"
            project-name name subdir tag (car (split-string host "\\.")))))

(defun quite--broadcast-to-flavors (command-func flavors)
  "Return a tag-function alist mapping every flavor in FLAVORS to COMMAND-FUNC."
  (mapcar (lambda (flavor) (list flavor command-func)) flavors))

(defcustom quite-flavor-abbreviations nil
  "Alist of (REGEXP . REPLACEMENT) abbreviations for hydra head descriptions.
Each flavor (tag) name is passed through every pair in order via
`replace-regexp-in-string' before being shown in a hydra head.  When
nil no abbreviation is done.  Projects/overlays typically set this."
  :type '(alist :key-type regexp :value-type string)
  :group 'quite)

(defun quite--abbreviate-flavor (flavors)
  "Abbreviate each flavor name in FLAVORS via `quite-flavor-abbreviations'."
  (mapcar (lambda (flavor)
            (let ((result flavor))
              (dolist (pair quite-flavor-abbreviations result)
                (setq result (replace-regexp-in-string (car pair) (cdr pair) result)))))
          flavors))

;; Ordering convention: the POSITION of a prefix in a project's :prefixes list
;; is its prefix-argument index -- element 0 runs with no prefix arg, element 1
;; with one C-u, element 2 with two C-u, and so on (see
;; `quite--prefix-arg-index').  Position encodes the C-u count; there is
;; deliberately no explicit index field.
(defun quite--project-flavors (target transform-name prefixes)
  "Return the ordered list of flavor (tag) names for TARGET and TRANSFORM-NAME.
One flavor is produced per plist in PREFIXES, named
\"TARGET-PREFIXNAME-TRANSFORMNAME\".  List order is significant: it is
the prefix-argument dispatch order."
  (mapcar (lambda (prefix)
            (format "%s-%s-%s" target prefix transform-name))
          prefixes))

(defun quite--project-command-key (command transform)
  "Return the variant key string for COMMAND under TRANSFORM.
COMMAND and TRANSFORM are plists; the transform's :func maps the
command's :key to its variant (e.g. `identity' or `upcase')."
  (funcall (plist-get transform :func) (plist-get command :key)))

(defun quite--project-command-dispatcher (project command transform)
  "Return the buffer dispatcher for COMMAND under TRANSFORM in PROJECT.
The dispatcher (see `quite-generate-buffer-dispatcher') runs COMMAND
for the flavor selected by the prefix argument.  Pure: called
identically by `quite-bind-project-commands' and
`quite-project-hydra-heads'."
  (let ((flavors (quite--project-flavors (plist-get project :target)
                                         (plist-get transform :name)
                                         (plist-get project :prefixes)))
        (build-func (quite--make-build-command (plist-get command :command)
                                               (plist-get project :git-name)
                                               (plist-get project :command-prefix)
                                               (plist-get project :command-postfix)))
        (buffer-name-func (quite--make-buffer-name (plist-get project :name)
                                                   (plist-get command :name))))
    (quite-generate-buffer-dispatcher
     (plist-get project :descriptor)
     buffer-name-func
     (quite--broadcast-to-flavors build-func flavors))))

(defvar quite-command-map (make-sparse-keymap)
  "Keymap of project build-command bindings populated by
`quite-bind-project-commands'.  Bind it to a prefix key in your
configuration to reach the commands, for example
  (global-set-key (kbd ...) quite-command-map)
Within the map each command is bound at its project PREFIX-KEY
concatenated with the command's variant key.")

;;;###autoload
(defun quite-bind-project-commands (project)
  "Bind PROJECT's build commands into `quite-command-map'.
For every command x transform, bind PREFIX-KEY concatenated with the
command's variant key to that command's dispatcher.  Returns the map.
Independent of
`quite-project-hydra-heads'.  PROJECT is a plist; see
`quite-define-project'."
  (let ((prefix-key (plist-get project :prefix-key)))
    (dolist (command (plist-get project :commands))
      (dolist (transform (plist-get project :transforms))
        (define-key quite-command-map
                    (kbd (concat prefix-key
                                 (quite--project-command-key command transform)))
                    (quite--project-command-dispatcher project command transform))))
    quite-command-map))

;;;###autoload
(defun quite-project-hydra-heads (project)
  "Return hydra heads describing PROJECT's build commands.
Each head is (VARIANT-KEY DISPATCHER DESCRIPTION :column COLUMN).  Pure:
it builds its own dispatchers and does not depend on
`quite-bind-project-commands' having run.  PROJECT is a plist; see
`quite-define-project'."
  (let ((target (plist-get project :target))
        (project-name (plist-get project :name))
        (prefixes (plist-get project :prefixes))
        (heads nil))
    (dolist (command (plist-get project :commands))
      (dolist (transform (plist-get project :transforms))
        (let* ((flavors (quite--project-flavors target (plist-get transform :name)
                                                prefixes))
               (description (mapconcat #'identity
                                       (quite--abbreviate-flavor flavors) " "))
               (column (format "%s %s" project-name (plist-get command :name))))
          (push (list (quite--project-command-key command transform)
                      (quite--project-command-dispatcher project command transform)
                      description
                      :column column)
                heads))))
    (nreverse heads)))

;;;###autoload
(defun quite-define-project (project)
  "Install PROJECT's build commands and return its hydra heads.
Binds every command via `quite-bind-project-commands' and returns the
heads from `quite-project-hydra-heads'.  Usual entry point for overlays.

PROJECT is a plist:
  :git-name        git-project name for the compile command
  :name            project name (buffer names, hydra columns)
  :descriptor      a `quite-project-descriptors' plist
  :prefix-key      key prefix inserted after \"C-c \"
  :target          target string used in flavor (tag) names
  :commands        list of (:name :command :key) plists
  :prefixes        list of prefix-name strings; list ORDER is the C-u index
                   (position 0 = no prefix, 1 = one C-u, 2 = two C-u, ...)
  :transforms      list of (:name :func) plists (:func maps a command key)
  :command-prefix  optional shell text before the compile command
  :command-postfix optional shell text after the compile command"
  (quite-bind-project-commands project)
  (quite-project-hydra-heads project))

(provide 'quite)

;;; quite.el ends here
