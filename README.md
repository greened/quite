# QUITE - QUIck Transparent Execution

QUITE is an Emacs package of utilities to create interactive commands to invoke
via keybindings.  QUITE includes utilities to invoke commands on remote buffers,
for exampile invoking `compile` on such a buffer, causing compilation to run on
the remote host.  Commands can be given a dedicated buffer with control over
naming.

## Getting Started

Using [straight.el](https://github.com/raxod502/straight.el "straight.el repo"):

    (use-package quite
      :straight (quite :type git :repo "https://github.com/greened/quite"))

## Configuration

### Create a project descriptor

    (setq clang-project-descriptor
		  `(:project-dir "llvm-project"
		    :root-list ("/path/to/project" "/another/path/to/project")
			:key-filed ("README.md")))

### Create a function to generate a buffer name


    (setq clang-build-buffer-template "*build-clang-%%h*")

    (defun clang-build-buffer-name (host)
           "Generate a buffer string for building clang on HOST."
           (quite-buffer-format
            clang-build-buffer-template
            (list `(?h ,(nth 0 (split-string host "\\."))))))

### Create some functions to invoke

    (setq clang-build-command-template
	      "ninja %s")

    (defun format-command (template &rest options)
           "Create a command from TEMPLATE substituting OPTIONS."
           (format template (mapconcat 'identity options " ")))

    (defun clang-build (host root buffer &rest options)
           "Invoke clang build on project at ROOT with OPTIONS in BUFFER."
            (quite-buffer-run-in-buffer-context
             (lambda ()
   	          (compile
	           (apply #'format-command clang-build-command-template options)))
             buffer
             (clang-build-buffer-name type flavor host)))

    (defun clang-check (host root buffer)
	       "Run clang and llvm tests."
		   (clang-build host root buffer "install" "check-clang" "check-llvm"))

    (defun clang-install (host root buffer)
	       "Do a clang install."
		   (clang-build host root buffer "install"))

## Authors

* **David Greene** - *Initial work and primary maintainer* - [GitHub](https://github.com/greened)

See also the list of [contributors](https://github.com/quite/contributors) who
participated in this project.

## License

This project is licensed under the [GNU General Public License version
3](https://www.gnu.org/licenses/gpl-3.0.en.html) - see the
[LICENSE.md](LICENSE.md) file for details
