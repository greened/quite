;;; demo-run.el --- quite-run, the headless entry -*- lexical-binding: t; -*-
;; The programmatic entry a tool (e.g. a build/test orchestrator) calls: run a
;; registered project's command with no keymap, Hydra, or file-visiting buffer.
;; Reuses the same compile command as the interactive matrix.  Example data only.
(load (expand-file-name "demo-common.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun demo--play ()
  (let ((buf (get-buffer-create "*scratch: driving quite*")))
    (with-current-buffer buf
      (emacs-lisp-mode)
      (erase-buffer)
      (insert ";; quite-run — the headless entry point.\n")
      (insert ";; No keymap, no Hydra, no open buffer: a tool just calls it,\n")
      (insert ";; reusing the SAME compile command as the interactive matrix.\n\n")
      (insert "(quite-run \"app\" \"build\" \"~/src/app\")"))
    (switch-to-buffer buf)
    (delete-other-windows)
    (message "quite-run PROJECT COMMAND [DIR] — how an orchestrator drives a build"))
  (sit-for 5.0)
  (switch-to-buffer
   (quite-run "app" "build" "/tmp" "*app: build (via quite-run)*"))
  (delete-other-windows)
  (message "→ ordinary `compile' in DIR; a remote DIR builds on the remote host")
  (sit-for 4.5)
  (sit-for 1.0)
  (kill-emacs 0))

(run-with-timer 0.5 nil #'demo--play)
;;; demo-run.el ends here
