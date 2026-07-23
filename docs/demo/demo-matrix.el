;;; demo-matrix.el --- quite command × flavor matrix + a build -*- lexical-binding: t; -*-
;; Shows the grid quite composes for a project (commands × flavors, bound under a
;; prefix key / popped as a Hydra), then runs two of them -- ordinary `compile',
;; so a remote default-directory would build on the remote host.  Example data
;; only (the "app" project from demo-common.el; nothing real is executed).
(load (expand-file-name "demo-common.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun demo--matrix-buffer ()
  "Render the app project's command × flavor grid into a buffer."
  (let ((buf (get-buffer-create "*quite matrix*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "quite — project “app”: command × flavor matrix\n")
      (insert "bound under  C-c a … ,  and popped live as a Hydra\n\n")
      (insert (format "  %-7s %-10s %s\n" "key" "command" "flavors"))
      (insert "  ------- ---------- -----------------\n")
      (dolist (h (quite-project-hydra-heads demo-project))
        (insert (format "  C-c a %s %-10s %s\n"
                        (nth 0 h)
                        (car (last (split-string (nth 4 h)))) ; command name
                        (nth 2 h))))                            ; flavors
      (insert "\nthe prefix arg picks the flavor:")
      (insert "\n  C-c a b            → build release")
      (insert "\n  C-u C-c a b        → build debug\n")
      (goto-char (point-min)))
    buf))

(defun demo--play ()
  (switch-to-buffer (demo--matrix-buffer))
  (delete-other-windows)
  (message "quite: one prefix key → a grid of build variants (commands × flavors)")
  (sit-for 5.5)
  ;; run one — ordinary compile, in the project's (here local) default-directory
  (switch-to-buffer
   (quite-run "app" "build" "/tmp" "*app: build release*"))
  (delete-other-windows)
  (message "C-c a b → runs it via `compile' (a remote dir would build remotely)")
  (sit-for 4.0)
  ;; a different command from the same grid
  (switch-to-buffer
   (quite-run "app" "check" "/tmp" "*app: check release*"))
  (delete-other-windows)
  (message "C-c a c → the check command, same grid — buffer named per project·command")
  (sit-for 4.5)
  (sit-for 1.0)
  (kill-emacs 0))

(run-with-timer 0.5 nil #'demo--play)
;;; demo-matrix.el ends here
