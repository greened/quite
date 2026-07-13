;;; quite-tests.el ---  QUIck Transparent Execution tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026  David A. Greene

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

;; Buttercup specs for quite.  Functions that touch TRAMP or the
;; filesystem are exercised by spying on the underlying Emacs primitives
;; (`buffer-file-name', `file-remote-p', `file-exists-p', `system-name',
;; `compile', ...) rather than requiring a real remote host -- so both the
;; remote-buffer and local-buffer code paths can be tested deterministically.

;;; Code:

(require 'buttercup)
(require 'quite)

;;; Pure helpers.

(describe "quite-remote--strip-host"
  (it "strips a /ssh:user@host: prefix"
    (expect (quite-remote--strip-host "/ssh:me@myhost:/path/to/test")
            :to-equal "/path/to/test"))
  (it "strips a /ssh:host: prefix with no user name"
    (expect (quite-remote--strip-host "/ssh:myhost:/path/to/test")
            :to-equal "/path/to/test"))
  (it "strips a host with a dotted domain"
    (expect (quite-remote--strip-host "/ssh:user@myhost.mine.com:/path/to/test")
            :to-equal "/path/to/test"))
  (it "strips a dotted-domain host with no user name"
    (expect (quite-remote--strip-host "/ssh:myhost.mine.com:/path/to/test")
            :to-equal "/path/to/test"))
  (it "strips a host with -, . and _ characters"
    (expect (quite-remote--strip-host "/ssh:dg@dgreene-vm.cerebras.aws:/cb/x")
            :to-equal "/cb/x"))
  (it "leaves a plain local path unchanged"
    (expect (quite-remote--strip-host "/path/to/test") :to-equal "/path/to/test")))

(describe "quite--prefix-arg-index"
  (it "maps no prefix (nil) to index 0"
    (expect (quite--prefix-arg-index nil) :to-equal 0))
  (it "maps raw C-u prefix args to successive indices"
    (expect (quite--prefix-arg-index '(4)) :to-equal 1)
    (expect (quite--prefix-arg-index '(16)) :to-equal 2)
    (expect (quite--prefix-arg-index '(64)) :to-equal 3))
  (it "passes an integer prefix arg through unchanged"
    (expect (quite--prefix-arg-index 0) :to-equal 0)
    (expect (quite--prefix-arg-index 1) :to-equal 1)
    (expect (quite--prefix-arg-index 2) :to-equal 2)))

(describe "quite--extract-subdir"
  (it "extracts the subdir below the project dir"
    (expect (quite--extract-subdir "/path/to/project/subdir" "project")
            :to-equal "subdir"))
  (it "extracts a multi-level subpath"
    (expect (quite--extract-subdir "/path/to/project/a/b" "project")
            :to-equal "a/b"))
  (it "ignores a trailing slash on the root"
    (expect (quite--extract-subdir "/path/to/project/subdir/" "project")
            :to-equal "subdir"))
  (it "returns the empty string at the project root"
    (expect (quite--extract-subdir "/path/to/project" "project")
            :to-equal ""))
  (it "returns nil when the project dir is absent"
    (expect (quite--extract-subdir "/path/to/other" "project")
            :to-equal nil)))

(describe "quite-project-parse-descriptor"
  (it "splits a descriptor into (project-dir root-list key-files)"
    (let ((parsed (quite-project-parse-descriptor
                   '(:project-dir "project"
                                  :root-list ("/path/to" "/home")
                                  :key-files ("README.md")))))
      (expect (nth 0 parsed) :to-equal "project")
      (expect (nth 1 parsed) :to-equal '("/path/to" "/home"))
      (expect (nth 2 parsed) :to-equal '("README.md")))))

(describe "quite-remote-create-remote-path"
  (it "builds an /ssh: path for a host"
    (expect (quite-remote-create-remote-path "myhost" "/path/to/x")
            :to-equal "/ssh:myhost:/path/to/x")))

(describe "quite--doit"
  (it "funcalls FUNC with TAG"
    (let (captured)
      (quite--doit "TAG" (lambda (tag) (setq captured tag)))
      (expect captured :to-equal "TAG"))))

(describe "quite--dispatch"
  :var (log entries)
  (before-each
    (setq log nil)
    (setq entries
          (list (list :function (lambda (tag) (push tag log)) :tag "t1")
                (list :function (lambda (tag) (push tag log)) :tag "t2")
                (list :function (lambda (tag) (push tag log)) :tag "t3")
                (list :function (lambda (tag) (push tag log)) :tag "t4"))))
  (it "invokes the entry selected by a raw prefix arg, passing its tag"
    (apply #'quite--dispatch nil entries)
    (expect (car log) :to-equal "t1")
    (apply #'quite--dispatch '(4) entries)
    (expect (car log) :to-equal "t2")
    (apply #'quite--dispatch '(16) entries)
    (expect (car log) :to-equal "t3")
    (apply #'quite--dispatch '(64) entries)
    (expect (car log) :to-equal "t4"))
  (it "invokes the entry selected by an integer prefix arg"
    (apply #'quite--dispatch 1 entries)
    (expect (car log) :to-equal "t2"))
  (it "errors when no entry exists for the prefix"
    (expect (apply #'quite--dispatch 9 entries) :to-throw)))

;;; Remote vs. local: host resolution.

(describe "quite-remote-localhost"
  (it "returns the local system name"
    (spy-on 'system-name :and-return-value "localbox")
    (expect (quite-remote-localhost (current-buffer)) :to-equal "localbox")))

(describe "quite-remote-host-for-current-buffer"
  (it "returns the remote host for a buffer visiting a remote file"
    (spy-on 'buffer-file-name :and-return-value "/ssh:me@remote:/p/f.c")
    (spy-on 'file-remote-p :and-return-value "remote")
    (expect (quite-remote-host-for-current-buffer nil #'ignore) :to-equal "remote"))
  (it "returns the local system name for a buffer visiting a local file"
    (spy-on 'buffer-file-name :and-return-value "/home/me/p/f.c")
    (spy-on 'file-remote-p :and-return-value nil)
    (spy-on 'system-name :and-return-value "localbox")
    (expect (quite-remote-host-for-current-buffer nil #'ignore) :to-equal "localbox"))
  (it "prompts when there is no file and PROMPT is non-nil"
    (spy-on 'buffer-file-name :and-return-value nil)
    (spy-on 'quite-remote--prompt-for-host :and-return-value "prompted")
    (expect (quite-remote-host-for-current-buffer t #'ignore) :to-equal "prompted"))
  (it "calls DEFAULT-HOST-FUNC when there is no file and no prompt"
    (spy-on 'buffer-file-name :and-return-value nil)
    (expect (quite-remote-host-for-current-buffer nil (lambda (_buf) "defaulted"))
            :to-equal "defaulted")))

;;; Remote vs. local: project discovery.

(describe "quite-project--file-exists-p"
  (it "returns the full path of the first existing key file"
    (spy-on 'file-exists-p :and-call-fake
            (lambda (p) (string-suffix-p "Makefile" p)))
    (expect (quite-project--file-exists-p "/root" '("build.sh" "Makefile"))
            :to-equal "/root/Makefile"))
  (it "returns nil when no key file exists"
    (spy-on 'file-exists-p :and-return-value nil)
    (expect (quite-project--file-exists-p "/root" '("Makefile")) :to-equal nil)))

(describe "quite-project--path-for-buffer"
  (it "finds the project root for a local buffer inside the project"
    (spy-on 'buffer-file-name :and-return-value "/home/me/project/src/f.c")
    (spy-on 'locate-dominating-file :and-return-value "/home/me/project/")
    (expect (quite-project--path-for-buffer "project" '("Makefile"))
            :to-equal "/home/me/project/"))
  (it "finds the project root for a remote buffer inside the project"
    (spy-on 'buffer-file-name :and-return-value "/ssh:host:/work/project/src/f.c")
    (spy-on 'locate-dominating-file :and-return-value "/ssh:host:/work/project/")
    (expect (quite-project--path-for-buffer "project" '("Makefile"))
            :to-equal "/ssh:host:/work/project/"))
  (it "returns nil when the buffer is not under the project dir"
    (spy-on 'buffer-file-name :and-return-value "/home/me/other/f.c")
    (expect (quite-project--path-for-buffer "project" '("Makefile")) :to-equal nil))
  (it "returns nil for a non-file buffer"
    (spy-on 'buffer-file-name :and-return-value nil)
    (expect (quite-project--path-for-buffer "project" '("Makefile")) :to-equal nil)))

(describe "quite-project-find-project"
  (it "returns the stripped buffer root when the buffer is in the project"
    (spy-on 'quite-project--path-for-buffer
            :and-return-value "/ssh:host:/work/project/")
    (expect (quite-project-find-project "project" "host" '("/work") '("Makefile"))
            :to-equal "/work/project/"))
  (it "finds a project on the LOCAL host with no remote prefix"
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'system-name :and-return-value "localbox")
    (spy-on 'file-exists-p :and-call-fake
            (lambda (p) (equal p "/work/project/Makefile")))
    (expect (quite-project-find-project "project" "localbox" '("/work") '("Makefile"))
            :to-equal "/work/project"))
  (it "finds a project on a REMOTE host using an /ssh: prefix for the check"
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'system-name :and-return-value "localbox")
    (spy-on 'file-exists-p :and-call-fake
            (lambda (p) (equal p "/ssh:remote:/work/project/Makefile")))
    (expect (quite-project-find-project "project" "remote" '("/work") '("Makefile"))
            :to-equal "/work/project"))
  (it "errors when the project cannot be found"
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'system-name :and-return-value "localbox")
    (spy-on 'file-exists-p :and-return-value nil)
    (spy-on 'quite-project--prompt-for-root :and-return-value "/nope")
    (expect (quite-project-find-project "project" "localbox" '("/work") '("Makefile"))
            :to-throw)))

(describe "quite--run-project-remote"
  (it "passes host, root, subdir, buffer and tag through to FUNC"
    (let (captured)
      (spy-on 'quite-remote-host-for-current-buffer :and-return-value "localbox")
      (spy-on 'quite-project-find-project :and-return-value "/r/project")
      (spy-on 'quite-remote-create-remote-path :and-return-value "/r/project")
      (spy-on 'quite-project-find-key-files-buffer :and-return-value :the-buffer)
      (spy-on 'quite--extract-subdir :and-return-value "sub")
      (quite--run-project-remote
       (lambda (h r s b tag) (setq captured (list h r s b tag)))
       '(:project-dir "project" :root-list ("/r") :key-files ("Makefile"))
       "TAG")
      (expect captured :to-equal '("localbox" "/r/project" "sub" :the-buffer "TAG")))))

;;; Build-command composition.

(describe "quite--make-build-command"
  (it "compiles \"git PROJECT COMMAND TAG\""
    (spy-on 'compile)
    (funcall (quite--make-build-command "build" "be") "h" "r" "s" "b" "all-devrel-local")
    (expect 'compile :to-have-been-called-with " git be build all-devrel-local "))
  (it "wraps the command in PREFIX and POSTFIX"
    (spy-on 'compile)
    (funcall (quite--make-build-command "build" "be" "PRE" "POST") "h" "r" "s" "b" "TAG")
    (expect 'compile :to-have-been-called-with "PRE git be build TAG POST")))

(describe "quite--make-buffer-name"
  (it "names the buffer, shortening the host to its first dotted component"
    (expect (funcall (quite--make-buffer-name "clang" "build")
                     "host.dom.com" "r" "sub" "buf" "TAG")
            :to-equal "*clang-build-sub-TAG-host*")))

(describe "quite--broadcast-to-flavors"
  (it "maps every flavor to the same command function"
    (let ((f (lambda (&rest _) :x)))
      (expect (quite--broadcast-to-flavors f '("a" "b"))
              :to-equal (list (list "a" f) (list "b" f))))))

(describe "quite--abbreviate-flavor"
  (it "applies quite-flavor-abbreviations in order"
    (let ((quite-flavor-abbreviations '(("dev" . "d") ("local" . "lo"))))
      (expect (quite--abbreviate-flavor '("dev-local" "dev-cluster"))
              :to-equal '("d-lo" "d-cluster"))))
  (it "returns names unchanged when no abbreviations are configured"
    (let ((quite-flavor-abbreviations nil))
      (expect (quite--abbreviate-flavor '("dev-local")) :to-equal '("dev-local")))))

(describe "quite--project-flavors"
  (it "builds TARGET-PREFIX-TRANSFORM names in prefix order"
    (expect (quite--project-flavors "all" "local" '("devrel" "devdbg"))
            :to-equal '("all-devrel-local" "all-devdbg-local"))))

(describe "quite--project-command-key"
  (it "applies the transform's :func to the command's :key"
    (expect (quite--project-command-key '(:key "b") (list :func #'upcase))
            :to-equal "B")
    (expect (quite--project-command-key '(:key "b") (list :func #'identity))
            :to-equal "b")))

;;; Project definition: bindings + hydra heads.

(describe "quite project definition"
  :var (project saved-map)
  (before-each
    ;; Use a throwaway command map so we never clobber the live one.
    (setq saved-map quite-command-map)
    (setq quite-command-map (make-sparse-keymap))
    (setq project
          (list :git-name "be" :name "P"
                :descriptor '(:project-dir "P" :root-list ("/r") :key-files ("k"))
                :prefix-key "p" :target "all"
                :commands '((:name "build" :command "build" :key "b"))
                :prefixes '("devrel" "devdbg")
                :transforms (list (list :name "local" :func #'identity)
                                  (list :name "cluster" :func #'upcase)))))
  (after-each
    (setq quite-command-map saved-map))

  (describe "quite-project-hydra-heads"
    (it "returns one head per command x transform"
      (let ((heads (quite-project-hydra-heads project)))
        (expect (length heads) :to-equal 2)
        (expect (mapcar #'car heads) :to-equal '("b" "B"))))
    (it "each head carries a callable dispatcher (no keybinding readback)"
      (let ((heads (quite-project-hydra-heads project)))
        (expect (functionp (nth 1 (car heads))) :to-be-truthy)))
    (it "does not require any binding to have run first"
      ;; quite-command-map is empty here; heads must still be complete.
      (expect (length (quite-project-hydra-heads project)) :to-equal 2)))

  (describe "quite-bind-project-commands"
    (it "binds prefix+variant keys into quite-command-map"
      (quite-bind-project-commands project)
      (expect (commandp (lookup-key quite-command-map (kbd "pb"))) :to-be-truthy)
      (expect (commandp (lookup-key quite-command-map (kbd "pB"))) :to-be-truthy))
    (it "returns the command map"
      (expect (quite-bind-project-commands project) :to-be quite-command-map)))

  (describe "quite-define-project"
    (it "binds commands AND returns the hydra heads"
      (let ((heads (quite-define-project project)))
        (expect (length heads) :to-equal 2)
        (expect (commandp (lookup-key quite-command-map (kbd "pb"))) :to-be-truthy))))

  (describe "end-to-end dispatch"
    (it "a bound command compiles the flavor chosen by the prefix arg"
      (quite-bind-project-commands project)
      (spy-on 'quite-remote-host-for-current-buffer :and-return-value "localbox")
      (spy-on 'quite-project-find-project :and-return-value "/r/P")
      (spy-on 'quite-project-find-key-files-buffer
              :and-return-value (get-buffer-create " *quite-test-buffer*"))
      (spy-on 'compile)
      (let ((current-prefix-arg nil))     ; no prefix -> first (devrel) flavor
        (call-interactively (lookup-key quite-command-map (kbd "pb"))))
      (expect 'compile :to-have-been-called-with " git be build all-devrel-local "))
    (it "one C-u selects the second (devdbg) flavor"
      (quite-bind-project-commands project)
      (spy-on 'quite-remote-host-for-current-buffer :and-return-value "localbox")
      (spy-on 'quite-project-find-project :and-return-value "/r/P")
      (spy-on 'quite-project-find-key-files-buffer
              :and-return-value (get-buffer-create " *quite-test-buffer*"))
      (spy-on 'compile)
      (let ((current-prefix-arg '(4)))    ; C-u -> second (devdbg) flavor
        (call-interactively (lookup-key quite-command-map (kbd "pb"))))
      (expect 'compile :to-have-been-called-with " git be build all-devdbg-local "))))

(provide 'quite-tests)
;;; quite-tests.el ends here
