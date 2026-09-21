;;; demo-remote.el --- quite following the buffer to a remote host -*- lexical-binding: t; -*-
;; The headline claim, actually demonstrated: open a file on a REMOTE host over
;; TRAMP, press the same key you would press locally, and the build runs THERE.
;; Nothing about the keys or the project definition changes -- only the buffer.
;;
;; The remote is reached as `demo-host', an ssh alias, so this capture carries
;; no real machine name.  Point it at any host you can `ssh demo-host' to.
;; Example data only: the build command is stubbed to echo, so the remote runs
;; an `echo' and nothing else.
;;
;; The project must already exist ON THE REMOTE at /tmp/quite-demo/app, with a
;; Makefile and src/app.c.  Seeding it over TRAMP inside the recording would
;; just film connection latency; see docs/demo/README.md.
(load (expand-file-name "demo-common.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(global-set-key (kbd "C-c q") quite-command-map)

(defvar demo--remote "/ssh:demo-host:")
(defvar demo--file (concat demo--remote "/tmp/quite-demo/app/src/app.c"))

;; Keep the capture deterministic: no "are you sure you want to connect", no
;; version-control probing of a remote tree, no save prompts.
(setq vc-handled-backends nil
      tramp-verbose 0
      confirm-nonexistent-file-or-buffer nil)

(defun demo--show-compilation ()
  "Bring the most recent compilation buffer forward, full frame."
  (let ((buf (seq-find (lambda (b)
                         (with-current-buffer b (derived-mode-p 'compilation-mode)))
                       (buffer-list))))
    (when buf (switch-to-buffer buf) (delete-other-windows))))

(defun demo--play ()
  (message "opening a file on a REMOTE host over TRAMP …")
  (find-file demo--file)
  (delete-other-windows)
  (sit-for 1.0)
  (message "buffer: %s" (abbreviate-file-name (buffer-file-name)))
  (sit-for 4.0)

  (message "same project, same keys as local — now press  C-c q a b")
  (sit-for 3.0)
  (execute-kbd-macro (kbd "C-c q a b"))
  (sit-for 1.5)
  (demo--show-compilation)
  ;; default-directory is the remote root, so `compile' ran the command THERE.
  (message "built on %s — default-directory is remote, so compile ran there"
           (or (file-remote-p default-directory 'host) "local"))
  (sit-for 6.0)
  (sit-for 1.0)
  (kill-emacs 0))

(run-with-timer 0.5 nil #'demo--play)
;;; demo-remote.el ends here
