;;; demo-matrix.el --- quite command × flavor matrix, driven by real keys -*- lexical-binding: t; -*-
;; Shows the grid quite composes for a project (commands × flavors), then runs
;; commands BY PRESSING THE KEYS -- `C-c q a b', then `C-u C-c q a b' -- so
;; prefix-argument flavor dispatch quite is built around is demonstrated rather
;; than described.  Example data only (the "app" project from demo-common.el,
;; whose build command is stubbed to echo, so nothing real is executed).
;;
;; An earlier version rendered a static table and then called `quite-run'
;; twice, while printing messages claiming the keys had been pressed.  The GIF
;; therefore asserted an interaction that never happened.  Drive the real
;; keymap here.  Never narrate a keystroke you did not send.
(load (expand-file-name "demo-common.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

;; Reach the project's commands the way a user would: a prefix key of your own
;; choosing bound to quite's command map.  Inside the map, each command sits at
;; the project's :prefix-key followed by the command's key -- so with
;; :prefix-key "a" and build key "b", the full sequence is C-c q a b.
(global-set-key (kbd "C-c q") quite-command-map)

;; The interactive path resolves the project root from the CURRENT BUFFER, so
;; the demo needs a real file inside the example project.  demo-common.el
;; points the descriptor at /tmp/quite-demo/app with a Makefile key file.
(defvar demo--root "/tmp/quite-demo/app")

(defun demo--seed-project ()
  "Create the example project tree and visit a source file inside it."
  (make-directory (expand-file-name "src" demo--root) t)
  (write-region "all:\n\t@true\n" nil (expand-file-name "Makefile" demo--root))
  (let ((f (expand-file-name "src/app.c" demo--root)))
    (write-region "int main(void) { return 0; }\n" nil f)
    (find-file f)))

(defun demo--matrix-buffer ()
  "Render the app project's command × flavor grid into a buffer."
  (let ((buf (get-buffer-create "*quite matrix*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "quite — project “app”: command × flavor matrix\n")
      (insert "bound under  C-c q a … ,  and popped live as a Hydra\n\n")
      (insert (format "  %-7s %-10s %s\n" "key" "command" "flavors"))
      (insert "  ------- ---------- -----------------\n")
      (dolist (h (quite-project-hydra-heads demo-project))
        (insert (format "  C-c q a %s %-8s %s\n"
                        (nth 0 h)
                        (car (last (split-string (nth 4 h)))) ; command name
                        (nth 2 h))))                            ; flavors
      (insert "\nthe prefix arg picks the flavor:")
      (insert "\n  C-c q a b          → build release")
      (insert "\n  C-u C-c q a b      → build debug\n")
      (goto-char (point-min)))
    buf))

(defun demo--show-compilation ()
  "Bring the most recent compilation buffer forward, full frame."
  (let ((buf (seq-find (lambda (b)
                         (with-current-buffer b (derived-mode-p 'compilation-mode)))
                       (buffer-list))))
    (when buf (switch-to-buffer buf) (delete-other-windows))))

(defun demo--play ()
  (demo--seed-project)
  (switch-to-buffer (demo--matrix-buffer))
  (delete-other-windows)
  (message "quite: one prefix key → a grid of build variants (commands × flavors)")
  (sit-for 5.5)

  ;; Back to the source file: the interactive path reads the current buffer.
  (find-file (expand-file-name "src/app.c" demo--root))
  (delete-other-windows)
  (message "from a file in the project — no prefix arg:  C-c q a b")
  (sit-for 2.5)
  (execute-kbd-macro (kbd "C-c q a b"))     ; the real binding, no prefix arg
  (sit-for 1.0)
  (demo--show-compilation)
  (message "C-c q a b → build, RELEASE flavor (command echoed, nothing run)")
  (sit-for 4.5)

  ;; Same key, one C-u: the next flavor in :prefixes.
  (find-file (expand-file-name "src/app.c" demo--root))
  (delete-other-windows)
  (message "same key, one C-u — the prefix arg selects the next flavor")
  (sit-for 2.5)
  (execute-kbd-macro (kbd "C-u C-c q a b")) ; same key, prefix arg = 1
  (sit-for 1.0)
  (demo--show-compilation)
  (message "C-u C-c q a b → build, DEBUG flavor — same keys, other flavor")
  (sit-for 5.0)
  (sit-for 1.0)
  (kill-emacs 0))

(run-with-timer 0.5 nil #'demo--play)
;;; demo-matrix.el ends here
