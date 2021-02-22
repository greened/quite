;;; quite-tests.el ---  QUIck Transparent Execution.  -*- lexical-binding: t; -*-

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

(require 'buttercup)
(require 'quite)

(describe "Quite tests"
  (describe "quite-remote--strip-host tests"
    (it "tests stripping a TRAMP remote prefix"
      (setq result (quite-remote--strip-host "/ssh:me@myhost:/path/to/test"))
      (expect result :to-be "/path/to/test"))

    (it "tests stripping a TRAMP remote prefix without a user name"
      (setq result (quite-remote--strip-host "/ssh:myhost:/path/to/test"))
      (expect result :to-be "/path/to/test"))

    (it "tests stripping a TRAMP remote prefix with a domain"
      (setq result (quite-remote--strip-host "/ssh:user@myhost.mine.com:/path/to/test"))
      (expect result :to-be "/path/to/test"))

    (it "tests stripping a TRAMP remote prefix with a domain but without a user name"
      (setq result (quite-remote--strip-host "/ssh:myhost.mine.com:/path/to/test"))
      (expect result :to-be "/path/to/test")))

  (describe "quite--prefix-arg-index tests"
    (it "tests a raw prefix argument"
      (expect (quite--prefix-arg-index '(0)) :to-equal 0)
      (expect (quite--prefix-arg-index '(4)) :to-equal 1)
      (expect (quite--prefix-arg-index '(16)) :to-equal 2)
      (expect (quite--prefix-arg-index '(64)) :to-equal 3))
    (it "tests a numeric prefix argument"
      (expect (quite--prefix-arg-index 0) :to-equal 0)
      (expect (quite--prefix-arg-index 1) :to-equal 1)
      (expect (quite--prefix-arg-index 2) :to-equal 2)
      (expect (quite--prefix-arg-index 4) :to-equal 4)
      (expect (quite--prefix-arg-index 16) :to-equal 16)
      (expect (quite--prefix-arg-index 64) :to-equal 64)))

  (describe "quite--extract-subdir tests"
    (it "tests extracting a subdir"
      (expect (quite--extract-subdir "/path/to/project/subdir" "project")
              :to-be "subdir"))

    (it "tests extracting a subdir with full project path"
      (expect (quite--extract-subdir "/path/to/project/subdir" "/path/to/project")
              :to-be "subdir"))

    (it "tests extracting an empty subdir"
      (expect (quite--extract-subdir "/path/to/project" "project")
              :to-be "")))

  (describe "quite--doit tests"
    :var (tester result)
    (before-each
      (fset 'tester (lambda (arg)
                      (setq result arg)))
      (spy-on 'tester))

    (it "tests calling a function with an argument"
      (quite--doit ("testval" 'tester))
      (expect 'tester :to-have-been-called-with "testval")
      (expect result :to-be "testval")))

  (describe "quite--dispatch tests"
    :var (func dispatch-table)
    (before-each
      (fset func (lambda (tag)
                   tag))
      (setq dispatch-table '((:function func :tag "tag1")
                             (:function func :tag "tag2")
                             (:function func :tag "tag3")
                             (:function func :tag "tag4")))
      (spy-on 'func))

    (it "tests dispatching to function 1"
      (apply #'quite--dispatch '(0) dispatch-table)
      (expect 'func :to-have-been-called-with "tag1")

      (apply #'quite--dispatch 0 dispatch-table)
      (expect 'func :to-have-been-called-with "tag1"))

    (it "tests dispatching to function 2"
      (apply #'quite--dispatch '(4) dispatch-table)
      (expect 'func :to-have-been-called-with "tag2")

      (apply #'quite--dispatch 1 dispatch-table)
      (expect 'func :to-have-been-called-with "tag2"))

    (it "tests dispatching to function 3"
      (apply #'quite--dispatch '(16) dispatch-table)
      (expect 'func :to-have-been-called-with "tag3")

      (apply #'quite--dispatch 2 dispatch-table)
      (expect 'func :to-have-been-called-with "tag3"))

    (it "tests dispatching to function 4"
      (apply #'quite--dispatch '(64) dispatch-table)
      (expect 'func :to-have-been-called-with "tag4")

      (apply #'quite--dispatch 3 dispatch-table)
      (expect 'func :to-have-been-called-with "tag4")))

  (describe "tests requiring a project descriptor"
    :var (descriptor)
    (before-each
      (setq descriptor
            '(:project-dir "project"
                           :root-list ("/path/to" "/home")
                           :key-files ("README.md"))))

    (describe "quite-project-parse-descriptor tests"
     (it "tests parsing a project descriptor"
       (let* ((parsed-descriptor (quite-project-parse-descriptor descriptor))
              (project-dir (nth 0 parsed-descriptor))
              (root-list (nth 1 parsed-descriptor))
              (key-files-list (nth 2 parsed-descriptor)))
         (expect project-dir :to-be "project")
         (expect root-list :to-be '("/path/to" "/home"))
         (expect project-dir :to-be '("README.md")))))

    (describe "quite--generate-invoker tests"
      (it "tests generating a function to invoke a passed function"
        (let* ((func (lambda (arg) arg))
               (generated (quite--generate-invoker (descriptor func))))
          (expect 'func :to-be '(lambda (tag)
                                  (quite--run-project-remote
                                   func
                                   descriptor
                                   tag))))))))
