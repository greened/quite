# QUITE - QUIck Transparent Execution

QUITE is an Emacs package of utilities to create interactive commands to invoke
via keybindings.  QUITE includes utilities to invoke commands on remote buffers,
for example invoking `compile` on such a buffer, causing compilation to run on
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
          :key-files (".arcconfig")))

### Create a function to generate a buffer name

    (setq clang-build-buffer-template "*build-clang-%s-%s-%s*")

    (defun clang-build-buffer-name (host subdir tag)
        "Generate a buffer string for building clang on HOST
        at project ROOT/SUBDIR in BUFFER with TAG."
        (format
         clang-build-buffer-template
         subdir
         tag
         (nth 0 (split-string host "\\."))))

### Create a function to invoke

    (defun clang-build (host root suubdir buffer tag)
        "Invoke clang build on project at HOST:ROOT/SUBDIR in BUFFER with TAG ."
        (quite-buffer-run-in-buffer-context
         (lambda (host root subdir buffer tag)
          (compile
           (format "ninja %s" tag)))
          buffer
          (clang-build-buffer-name type host subdir tag)))

### Setup a dispatch plist

    (setq dispatch-plist '(:function))
## Authors

* **David Greene** - *Initial work and primary maintainer* - [GitHub](https://github.com/greened)

See also the list of [contributors](https://github.com/quite/contributors) who
participated in this project.

## License

This project is licensed under the [GNU General Public License version
3](https://www.gnu.org/licenses/gpl-3.0.en.html) - see the
[LICENSE.md](LICENSE.md) file for details
