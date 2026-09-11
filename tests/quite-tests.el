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

;; Buttercup specs for quite.  Functions that touch the filesystem or need a
;; particular buffer are exercised by spying on the underlying Emacs
;; primitives (`buffer-file-name', `file-exists-p', `system-name', `compile',
;; ...) rather than requiring a real remote host.
;;
;; TRAMP name parsing is the exception, and deliberately so.  Those specs call
;; the REAL `file-remote-p' and `file-local-name', because parsing needs no
;; network and stubbing them would stub the exact behavior quite's connection
;; model rests on.  A hop spec must go through `expand-file-name' first, since
;; quite reads `buffer-file-name', which is canonical -- and it must bind
;; `tramp-default-proxies-alist', because TRAMP records an ad-hoc route there.

;;; Code:

(require 'buttercup)
(require 'quite)
;; TRAMP must be loaded before any spec gates on `tramp-show-ad-hoc-proxies',
;; or the gate reads "absent" merely because TRAMP has not loaded yet.
(require 'tramp)

;;; Pure helpers.

;; These specs deliberately call the REAL `file-remote-p' and
;; `file-local-name'.  Parsing a TRAMP name needs no network, and spying on
;; them would stub the exact behavior the connection model depends on.

(describe "quite-remote-localname"
  (it "strips a /ssh:user@host: prefix"
    (expect (quite-remote-localname "/ssh:me@myhost:/path/to/test")
            :to-equal "/path/to/test"))
  (it "strips a /ssh:host: prefix with no user name"
    (expect (quite-remote-localname "/ssh:myhost:/path/to/test")
            :to-equal "/path/to/test"))
  (it "strips a host with a dotted domain"
    (expect (quite-remote-localname "/ssh:user@myhost.mine.com:/path/to/test")
            :to-equal "/path/to/test"))
  (it "strips a host with -, . and _ characters"
    (expect (quite-remote-localname "/ssh:dg@dev-vm.example.com:/srv/x")
            :to-equal "/srv/x"))
  (it "strips a prefix carrying a port"
    ;; The old regexp could not: # is outside its [-._[:alnum:]] class, so
    ;; the prefix survived and was then prefixed a second time.
    (expect (quite-remote-localname "/ssh:myhost#2222:/a/b") :to-equal "/a/b"))
  (it "strips a prefix carrying both a user and a port"
    (expect (quite-remote-localname "/ssh:me@myhost#2222:/a/b")
            :to-equal "/a/b"))
  (it "leaves a plain local path unchanged"
    (expect (quite-remote-localname "/path/to/test") :to-equal "/path/to/test"))
  (it "strips a prefix naming any method, not only the configured one"
    ;; Inverted deliberately.  Stripping used to be scoped to
    ;; `quite-remote-method'; a real buffer's prefix is real whoever wrote it.
    (let ((quite-remote-method "sshx"))
      (expect (quite-remote-localname "/ssh:myhost:/path/to/test")
              :to-equal "/path/to/test")
      (expect (quite-remote-localname "/sudo:myhost:/path/to/test")
              :to-equal "/path/to/test"))))

(describe "quite-remote-connection"
  (it "returns nil for a local path"
    (expect (quite-remote-connection "/path/to/test") :to-be nil))
  (it "returns the prefix for a plain remote path"
    (expect (quite-remote-connection "/ssh:myhost:/a/b")
            :to-equal "/ssh:myhost:"))
  (it "keeps the user"
    (expect (quite-remote-connection "/ssh:me@myhost:/a/b")
            :to-equal "/ssh:me@myhost:"))
  (it "keeps the port"
    (expect (quite-remote-connection "/ssh:myhost#2222:/a/b")
            :to-equal "/ssh:myhost#2222:"))
  (it "keeps both a user and a port"
    (expect (quite-remote-connection "/ssh:me@myhost#2222:/a/b")
            :to-equal "/ssh:me@myhost#2222:"))
  (it "keeps a non-ssh method"
    (expect (quite-remote-connection "/sudo:myhost:/a/b")
            :to-equal "/sudo:myhost:"))
  (it "round-trips through quite-remote-path"
    (dolist (p '("/ssh:me@myhost#2222:/a/b" "/sudo:h:/x" "/ssh:myhost:/a/b"))
      (expect (quite-remote-path (quite-remote-connection p)
                                 (quite-remote-localname p))
              :to-equal p)))
  (it "round-trips a local path, where the connection is nil"
    (let ((p "/a/b/c"))
      (expect (quite-remote-path (quite-remote-connection p)
                                 (quite-remote-localname p))
              :to-equal p))))

(describe "quite-remote-connection on an inline hop"
  ;; Isolate the ad-hoc proxy state, so one spec cannot leak a route into the
  ;; next, and so neither touches the user's real configuration.
  (it "extracts whatever the CANONICAL name carries"
    ;; Pin the option too, not just the proxy alist.  Otherwise this spec
    ;; asserts default behavior while inheriting whatever the ambient Emacs
    ;; has set, and fails on a correct implementation.
    (let* ((tramp-default-proxies-alist nil)
           (tramp-show-ad-hoc-proxies nil)
           ;; quite reads `buffer-file-name', which is canonical -- so the
           ;; name must go through `expand-file-name' first.  Asserting
           ;; against a literal hop string would test a path that never runs.
           (canonical (expand-file-name "/ssh:bastion|ssh:target:/a/b"))
           (conn (quite-remote-connection canonical)))
      ;; Version-gated: Emacs 28.x keeps the hop in the canonical name.
      ;; `tramp-show-ad-hoc-proxies' arrived in 29.2 and drops it by default.
      (if (boundp 'tramp-show-ad-hoc-proxies)
          (expect conn :to-equal "/ssh:target:")
        (expect conn :to-equal "/ssh:bastion|ssh:target:"))
      ;; Either way the connection must rebuild the name it came from.
      (expect (quite-remote-path conn (quite-remote-localname canonical))
              :to-equal canonical)))
  (it "keeps an inline hop when TRAMP is told to show one"
    (if (not (boundp 'tramp-show-ad-hoc-proxies))
        (expect t :to-be t)          ; nothing to opt into before 29.2
      (let* ((tramp-default-proxies-alist nil)
             (tramp-show-ad-hoc-proxies t)
             (canonical (expand-file-name "/ssh:bastion2|ssh:target2:/a/b")))
        (expect (quite-remote-connection canonical)
                :to-equal "/ssh:bastion2|ssh:target2:")))))

(describe "quite-remote-display-host"
  (it "returns the host for a remote connection"
    (expect (quite-remote-display-host "/ssh:me@myhost:") :to-equal "myhost"))
  (it "drops the user but keeps the port, as file-remote-p does"
    (expect (quite-remote-display-host "/ssh:me@myhost#2222:")
            :to-equal "myhost#2222"))
  (it "returns the local system name for nil"
    (spy-on 'system-name :and-return-value "localbox")
    (expect (quite-remote-display-host nil) :to-equal "localbox")))

(describe "quite--connection-token"
  (it "gives a bare short host for a local (nil) connection"
    (spy-on 'system-name :and-return-value "localbox.example.com")
    (expect (quite--connection-token nil) :to-equal "localbox"))
  (it "shortens the host and appends a hash for a remote connection"
    (expect (quite--connection-token "/ssh:myhost.example.com:")
            :to-match "\\`myhost-[0-9a-f]\\{12\\}\\'"))
  (it "distinguishes local from a remote of the same name"
    (spy-on 'system-name :and-return-value "myhost")
    (expect (quite--connection-token nil)
            :not :to-equal (quite--connection-token "/ssh:myhost:")))
  (it "distinguishes two methods on one host"
    (expect (quite--connection-token "/ssh:myhost:")
            :not :to-equal (quite--connection-token "/sudo:myhost:")))
  (it "distinguishes two users on one host"
    (expect (quite--connection-token "/ssh:alice@myhost:")
            :not :to-equal (quite--connection-token "/ssh:bob@myhost:")))
  (it "distinguishes two ports on one host"
    (expect (quite--connection-token "/ssh:myhost#22:")
            :not :to-equal (quite--connection-token "/ssh:myhost#2222:")))
  (it "distinguishes two hops to one target"
    (expect (quite--connection-token "/ssh:bastionA|ssh:target:")
            :not :to-equal
            (quite--connection-token "/ssh:bastionB|ssh:target:")))
  (it "is stable for the same connection"
    (expect (quite--connection-token "/ssh:me@myhost#2222:")
            :to-equal (quite--connection-token "/ssh:me@myhost#2222:"))))

(describe "quite-remote-path"
  (it "prefixes a path with a connection"
    (expect (quite-remote-path "/ssh:myhost:" "/a/b")
            :to-equal "/ssh:myhost:/a/b"))
  (it "returns the bare path when the connection is nil"
    (expect (quite-remote-path nil "/a/b") :to-equal "/a/b")))

(describe "quite-remote--prefix"
  (it "builds a prefix from the default method"
    (expect (quite-remote--prefix "myhost") :to-equal "/ssh:myhost:"))
  (it "builds a prefix from a configured method"
    (let ((quite-remote-method "docker"))
      (expect (quite-remote--prefix "myhost") :to-equal "/docker:myhost:"))))

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
  (it "returns nil, meaning the local machine"
    ;; NOT `system-name'.  A host equal to `system-name' could not be told
    ;; apart from a deliberate /ssh:localhost: or /sudo: connection.
    (expect (quite-remote-localhost (current-buffer)) :to-be nil)))

(describe "quite-remote-connection-for-current-buffer"
  (it "returns the full prefix for a buffer visiting a remote file"
    (spy-on 'buffer-file-name :and-return-value "/ssh:me@remote#2222:/p/f.c")
    (expect (quite-remote-connection-for-current-buffer nil #'ignore)
            :to-equal "/ssh:me@remote#2222:"))
  (it "returns nil for a buffer visiting a local file"
    (spy-on 'buffer-file-name :and-return-value "/home/me/p/f.c")
    (expect (quite-remote-connection-for-current-buffer nil #'ignore) :to-be nil))
  (it "prompts when there is no file and PROMPT is non-nil"
    (spy-on 'buffer-file-name :and-return-value nil)
    (spy-on 'quite-remote--prompt-for-host :and-return-value "prompted")
    (expect (quite-remote-connection-for-current-buffer t #'ignore)
            :to-equal "/ssh:prompted:"))
  (it "calls DEFAULT-HOST-FUNC when there is no file and no prompt"
    (spy-on 'buffer-file-name :and-return-value nil)
    (expect (quite-remote-connection-for-current-buffer
             nil (lambda (_buf) "defaulted"))
            :to-equal "/ssh:defaulted:"))
  (it "treats a nil answer from DEFAULT-HOST-FUNC as local"
    ;; Guards the (and host ...) in the resolver.  Without it this built the
    ;; nonsense prefix "/ssh::".
    (spy-on 'buffer-file-name :and-return-value nil)
    (expect (quite-remote-connection-for-current-buffer nil #'ignore)
            :to-be nil)))

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
    (expect (quite-project-find-project "project" "/ssh:host:" '("/work")
                                        '("Makefile"))
            :to-equal "/work/project/"))
  (it "strips a buffer root carrying a user and a port"
    (spy-on 'quite-project--path-for-buffer
            :and-return-value "/ssh:me@host#2222:/work/project/")
    (expect (quite-project-find-project "project" "/ssh:me@host#2222:"
                                        '("/work") '("Makefile"))
            :to-equal "/work/project/"))
  (it "finds a project on a nil (local) connection with no prefix"
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'file-exists-p :and-call-fake
            (lambda (p) (equal p "/work/project/Makefile")))
    (expect (quite-project-find-project "project" nil '("/work") '("Makefile"))
            :to-equal "/work/project"))
  (it "prefixes the existence check with the connection"
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'file-exists-p :and-call-fake
            (lambda (p) (equal p "/ssh:remote:/work/project/Makefile")))
    (expect (quite-project-find-project "project" "/ssh:remote:" '("/work")
                                        '("Makefile"))
            :to-equal "/work/project"))
  (it "uses the connection verbatim, whatever its method, user or port"
    ;; The connection is copied from the buffer, so quite-remote-method has
    ;; no say here.  A user and a port must survive into the check.
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'file-exists-p :and-call-fake
            (lambda (p) (equal p "/sudo:me@remote#2222:/work/project/Makefile")))
    (let ((quite-remote-method "sshx"))
      (expect (quite-project-find-project "project" "/sudo:me@remote#2222:"
                                          '("/work") '("Makefile"))
              :to-equal "/work/project")))
  (it "prefixes the PROMPTED root with the connection as well"
    ;; The root-list branch and the prompt branch are separate code paths.
    ;; Dropping the connection in only the prompted one would otherwise leave
    ;; every other spec green.
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'quite-project--prompt-for-root :and-return-value "/elsewhere")
    (spy-on 'file-exists-p :and-call-fake
            (lambda (p) (equal p "/ssh:remote:/elsewhere/project/Makefile")))
    (expect (quite-project-find-project "project" "/ssh:remote:" '("/work")
                                        '("Makefile"))
            :to-equal "/elsewhere/project"))
  (it "errors when the project cannot be found"
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'file-exists-p :and-return-value nil)
    (spy-on 'quite-project--prompt-for-root :and-return-value "/nope")
    (expect (quite-project-find-project "project" nil '("/work") '("Makefile"))
            :to-throw))
  (it "names the prompted path in the error, not just the root list"
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'file-exists-p :and-return-value nil)
    (spy-on 'quite-project--prompt-for-root :and-return-value "/nope")
    (expect (condition-case err
                (quite-project-find-project "project" "/ssh:remote:" '("/work")
                                            '("Makefile"))
              (error (error-message-string err)))
            :to-match "/ssh:remote:/nope/project"))
  (it "names the local machine in the error, not an empty string"
    (spy-on 'quite-project--path-for-buffer :and-return-value nil)
    (spy-on 'file-exists-p :and-return-value nil)
    (spy-on 'quite-project--prompt-for-root :and-return-value "/nope")
    (expect (condition-case err
                (quite-project-find-project "project" nil '("/work")
                                            '("Makefile"))
              (error (error-message-string err)))
            :to-match "the local machine")))

(describe "quite--run-project-remote"
  (it "passes a nil connection, root, subdir, buffer and tag through to FUNC"
    (let (captured)
      (spy-on 'quite-remote-connection-for-current-buffer :and-return-value nil)
      (spy-on 'quite-project-find-project :and-return-value "/r/project")
      (spy-on 'quite-remote-path :and-return-value "/r/project")
      (spy-on 'quite-project-find-key-files-buffer :and-return-value :the-buffer)
      (spy-on 'quite--extract-subdir :and-return-value "sub")
      (quite--run-project-remote
       (lambda (c r s b tag) (setq captured (list c r s b tag)))
       '(:project-dir "project" :root-list ("/r") :key-files ("Makefile"))
       "TAG")
      (expect captured :to-equal '(nil "/r/project" "sub" :the-buffer "TAG"))))
  (it "passes the whole connection through, not a bare host"
    ;; The point of the connection model: a command function must be able to
    ;; tell a user, a port and a method apart.
    (let (captured)
      (spy-on 'quite-remote-connection-for-current-buffer
              :and-return-value "/sudo:me@box#2222:")
      (spy-on 'quite-project-find-project :and-return-value "/r/project")
      (spy-on 'quite-project-find-key-files-buffer :and-return-value :the-buffer)
      (spy-on 'quite--extract-subdir :and-return-value "sub")
      (quite--run-project-remote
       (lambda (c _r _s _b _tag) (setq captured c))
       '(:project-dir "project" :root-list ("/r") :key-files ("Makefile"))
       "TAG")
      (expect captured :to-equal "/sudo:me@box#2222:"))))

(describe "quite--run-in-buffer-context"
  (it "gives a REUSED buffer the freshly resolved default-directory"
    ;; Regression test.  The reuse branch used to run in whatever directory
    ;; the previous invocation left behind, so `compile' could run on an
    ;; earlier build's connection instead of the one just resolved.
    (let ((stale (generate-new-buffer " *quite-test-stale*"))
          (fresh (generate-new-buffer " *quite-test-fresh*"))
          seen)
      (unwind-protect
          (progn
            (with-current-buffer stale
              (setq default-directory "/old/dir/")
              (rename-buffer "*quite-ctx-test*"))
            (with-current-buffer fresh (setq default-directory "/new/dir/"))
            (quite--run-in-buffer-context
             (lambda () (setq seen default-directory) nil)
             fresh "*quite-ctx-test*")
            (expect seen :to-equal "/new/dir/"))
        (kill-buffer stale)
        (kill-buffer fresh)))))

;;; Build-command composition.

(describe "quite--make-build-command"
  (it "compiles \"git PROJECT COMMAND TAG\""
    (spy-on 'compile)
    (funcall (quite--make-build-command "build" "be") "h" "r" "s" "b" "all-release-local")
    (expect 'compile :to-have-been-called-with " git be build all-release-local "))
  (it "wraps the command in PREFIX and POSTFIX"
    (spy-on 'compile)
    (funcall (quite--make-build-command "build" "be" "PRE" "POST") "h" "r" "s" "b" "TAG")
    (expect 'compile :to-have-been-called-with "PRE git be build TAG POST")))

(describe "quite-build-command"
  (it "compiles a git-project command for the git-project architecture"
    (spy-on 'compile)
    (funcall (quite-build-command 'git-project
                                  '(:name "build" :command "build" :key "b")
                                  '(:git-name "be"))
             "h" "r" "s" "b" "all-release-local")
    (expect 'compile :to-have-been-called-with " git be build all-release-local "))
  (it "runs the :shell-command verbatim for the shell architecture"
    (spy-on 'compile)
    (funcall (quite-build-command 'shell
                                  '(:name "check" :command "check" :key "k"
                                          :shell-command "hatch run test")
                                  '(:name "widget"))
             "h" "r" "s" "b" "TAG")
    (expect 'compile :to-have-been-called-with "hatch run test"))
  (it "rejects a shell command with no :shell-command rather than running its name"
    (expect (quite-build-command 'shell '(:name "check" :command "check")
                                 '(:name "widget"))
            :to-throw 'error))
  (it "wraps a shell command in PREFIX and POSTFIX, omitting empty ones"
    (spy-on 'compile)
    (funcall (quite-build-command 'shell '(:command "check" :shell-command "make test")
                                  '(:command-prefix "PRE" :command-postfix "POST"))
             "h" "r" "s" "b" "TAG")
    (expect 'compile :to-have-been-called-with "PRE make test POST"))
  (it "ignores the build tag for the shell architecture"
    (spy-on 'compile)
    (let ((build-func (quite-build-command
                       'shell '(:command "check" :shell-command "./check.sh") nil)))
      (funcall build-func "h" "r" "s" "b" "one")
      (funcall build-func "h" "r" "s" "b" "two"))
    (expect 'compile :to-have-been-called-with "./check.sh")
    (expect (spy-calls-count 'compile) :to-equal 2))
  (it "names an unknown architecture instead of failing on dispatch"
    (expect (quite-build-command 'hatchling '(:command "build") '(:name "widget"))
            :to-throw 'error)))

(describe "quite--project-build-command"
  (it "defaults to the git-project architecture when none is given"
    (spy-on 'compile)
    (funcall (quite--project-build-command '(:command "build")
                                           '(:git-name "be" :target "all"))
             "h" "r" "s" "b" "TAG")
    (expect 'compile :to-have-been-called-with " git be build TAG "))
  (it "honors an explicit :build-architecture"
    (spy-on 'compile)
    (funcall (quite--project-build-command '(:command "check"
                                             :shell-command "make test")
                                           '(:build-architecture shell))
             "h" "r" "s" "b" "TAG")
    (expect 'compile :to-have-been-called-with "make test")))

(describe "quite--project-flavors dimension combinations"
  ;; A flavor name IS the build tag, so an empty component joined in is a
  ;; wrong target rather than a cosmetic flaw.  All four combinations of the
  ;; two optional dimensions have to name distinct, well-formed flavors.
  (it "joins both dimensions when both are present"
    (expect (quite--project-flavors "all" "local" '("release" "debug"))
            :to-equal '("all-release-local" "all-debug-local")))
  (it "omits the transform when it is unnamed, leaving no trailing hyphen"
    (expect (quite--project-flavors "all" "" '("release" "debug"))
            :to-equal '("all-release" "all-debug")))
  (it "keeps transforms distinct when there are no prefixes"
    (expect (append (quite--project-flavors "widget" "local" nil)
                    (quite--project-flavors "widget" "cluster" nil))
            :to-equal '("widget-local" "widget-cluster")))
  (it "names the flavor by the target alone when neither dimension is given"
    (expect (quite--project-flavors "widget" "" nil) :to-equal '("widget"))))

(describe "a shell project end to end"
  ;; The point of the architecture seam: a project built by its own tooling,
  ;; declaring neither :prefixes nor :transforms, is a first-class project.
  (let ((project '(:name "widget"
                   :build-architecture shell
                   :descriptor (:project-dir "widget" :root-list ("/w"))
                   :prefix-key "w"
                   :target "widget"
                   :commands ((:name "build" :command "build" :key "b"
                                     :shell-command "hatch build")
                              (:name "check" :command "check" :key "k"
                                     :shell-command "hatch run test")))))
    (it "produces one hydra head per command, described by the bare target"
      (let ((heads (quite-project-hydra-heads project)))
        (expect (length heads) :to-equal 2)
        (expect (mapcar #'car heads) :to-equal '("b" "k"))
        (expect (nth 2 (car heads)) :to-equal "widget")))
    (it "binds each command at its prefix key with no variant"
      (let ((quite-command-map (make-sparse-keymap)))
        (quite-bind-project-commands project)
        (expect (keymapp (lookup-key quite-command-map (kbd "w"))) :to-be-truthy)
        (expect (commandp (lookup-key quite-command-map (kbd "w b"))) :to-be-truthy)
        (expect (commandp (lookup-key quite-command-map (kbd "w k"))) :to-be-truthy)))
    (it "runs the command's :shell-command headlessly via quite-run"
      (spy-on 'compile)
      (let ((quite--projects nil))
        (quite-define-project project)
        (quite-run "widget" "check" "/w/widget"))
      (expect 'compile :to-have-been-called-with "hatch run test"))
    (it "runs build and check differently despite sharing one flavor"
      (spy-on 'compile)
      (let ((quite--projects nil))
        (quite-define-project project)
        (quite-run "widget" "build" "/w/widget")
        (quite-run "widget" "check" "/w/widget"))
      (expect (spy-calls-count 'compile) :to-equal 2)
      (expect (spy-calls-args-for 'compile 0) :to-equal '("hatch build"))
      (expect (spy-calls-args-for 'compile 1) :to-equal '("hatch run test")))))

(describe "quite--project-transforms"
  (it "returns the project's transforms when present"
    (let ((transforms '((:name "local" :func identity))))
      (expect (quite--project-transforms (list :transforms transforms))
              :to-equal transforms)))
  (it "defaults to a single identity transform when absent"
    (let ((transforms (quite--project-transforms '(:name "quite"))))
      (expect (length transforms) :to-equal 1)
      (expect (plist-get (car transforms) :name) :to-equal "")
      (expect (funcall (plist-get (car transforms) :func) "k") :to-equal "k"))))

(describe "quite--make-buffer-name"
  (it "names a local build with the short system name and no hash"
    (spy-on 'system-name :and-return-value "localbox.dom.com")
    (expect (funcall (quite--make-buffer-name "clang" "build")
                     nil "r" "sub" "buf" "TAG")
            :to-equal "*clang-build-sub-TAG-localbox*"))
  (it "names a remote build with the short host and a hash"
    (expect (funcall (quite--make-buffer-name "clang" "build")
                     "/ssh:host.dom.com:" "r" "sub" "buf" "TAG")
            :to-match "\\`\\*clang-build-sub-TAG-host-[0-9a-f]\\{12\\}\\*\\'"))
  (it "gives two connections to one host two different buffers"
    (let ((namer (quite--make-buffer-name "clang" "build")))
      (expect (funcall namer "/ssh:alice@h:" "r" "sub" "buf" "TAG")
              :not :to-equal
              (funcall namer "/ssh:bob@h:" "r" "sub" "buf" "TAG")))))

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
    (expect (quite--project-flavors "all" "local" '("release" "debug"))
            :to-equal '("all-release-local" "all-debug-local"))))

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
                :prefixes '("release" "debug")
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
      (spy-on 'quite-remote-connection-for-current-buffer :and-return-value nil)
      (spy-on 'quite-project-find-project :and-return-value "/r/P")
      (spy-on 'quite-project-find-key-files-buffer
              :and-return-value (get-buffer-create " *quite-test-buffer*"))
      (spy-on 'compile)
      (let ((current-prefix-arg nil))     ; no prefix -> first (release) flavor
        (call-interactively (lookup-key quite-command-map (kbd "pb"))))
      (expect 'compile :to-have-been-called-with " git be build all-release-local "))
    (it "one C-u selects the second (debug) flavor"
      (quite-bind-project-commands project)
      (spy-on 'quite-remote-connection-for-current-buffer :and-return-value nil)
      (spy-on 'quite-project-find-project :and-return-value "/r/P")
      (spy-on 'quite-project-find-key-files-buffer
              :and-return-value (get-buffer-create " *quite-test-buffer*"))
      (spy-on 'compile)
      (let ((current-prefix-arg '(4)))    ; C-u -> second (debug) flavor
        (call-interactively (lookup-key quite-command-map (kbd "pb"))))
      (expect 'compile :to-have-been-called-with " git be build all-debug-local "))))

(provide 'quite-tests)
;;; quite-tests.el ends here
