;;; demo-common.el --- shared setup for quite demos -*- lexical-binding: t; -*-
;; Loads quite + hydra and defines ONE example project ("app") whose build
;; command is stubbed to echo the command line (no real build, no network), so
;; the demos are deterministic.  This file lives in docs/demo/, so quite.el is
;; two levels up; an elpaca build dir (for hydra) is added when present.
(defvar demo-dir (file-name-directory (or load-file-name buffer-file-name
                                          default-directory)))
(let ((builds (expand-file-name "elpaca/builds" user-emacs-directory)))
  (when (file-directory-p builds)
    (dolist (d (directory-files builds t "^[^.]"))
      (when (file-directory-p d) (push d load-path)))))
(setq inhibit-startup-screen t ring-bell-function 'ignore make-backup-files nil
      use-dialog-box nil auto-save-default nil compilation-scroll-output t
      compilation-mode-line-errors nil)
(require 'hydra)
;; Load quite from source (two levels up) so we get the checkout, not a stale
;; byte-compiled copy on the load-path.
(load (expand-file-name "../../quite.el" demo-dir) nil t)

;; Prettify flavor (tag) names in the matrix: strip the "app-"/"-std" scaffolding
;; so the grid reads "release" / "debug".
(setq quite-flavor-abbreviations '(("^app-" . "") ("-std$" . "")))

;; The example project.  The stub: `:command-prefix' makes the compile command
;; `echo "$ git app <cmd> <tag>"' -- so the *compilation* buffer shows exactly
;; the command quite assembled, run as ordinary `compile', with nothing real
;; executed.
(defvar demo-project
  (list :git-name "app" :name "app" :prefix-key "a" :target "app"
        :descriptor '(:project-dir "app" :root-list ("/tmp/quite-demo")
                      :key-files ("Makefile"))
        :commands '((:name "build" :command "build" :key "b")
                    (:name "check" :command "check" :key "c")
                    (:name "install" :command "install" :key "i"))
        :prefixes '("release" "debug")      ; the flavor axis (prefix-arg picks one)
        :transforms '((:name "std" :func identity))
        :command-prefix "echo '$'"          ; echo the command line, run nothing
        :command-postfix ""))

(quite-define-project demo-project)         ; registers it (keymap + quite--projects)
;;; demo-common.el ends here
