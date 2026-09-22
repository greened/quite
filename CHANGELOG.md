# Changelog

`quite` is an Emacs package that runs a project's build and development commands on the host where the edited file lives — local or remote over TRAMP — with prefix-argument dispatch selecting command flavors.

Reconstructed from git history. `v0.1.0` is the first tagged release.

## Unreleased

- Recorded `quite-remote.gif`. `v0.1.0` shipped `demo-remote.el` with no
  capture, so the feature the README leads with — the build follows the buffer
  to another host — was the one thing the GIFs never showed. It now shows a
  remote buffer, the same `C-c q a b`, and a compilation whose
  `default-directory` is the remote project root.

## v0.1.0 — 2026

The dormant package woke up and matured into a shippable tool. The build-composition machinery moved out of personal init and into the package proper (`quite-define-project`, built on independent binding and hydra-head builders), with pre-existing bugs and byte-compile warnings cleared. A headless surface was added so orchestrators like gaffer can drive builds without a keymap or a file-visiting buffer, and the project grew a real test suite, CI, docs, and demo GIFs.

- Fixed remote host stripping to handle hosts with `-`, `.` and `_`.
- Lifted the command × flavor matrix composition into the package; dropped the unused `:prefix` field; fixed `quite--buffer-format` and `quite-execute`.
- Corrected `quite--extract-subdir` to return the full subpath below the project dir, trailing-slash insensitive.
- Rewrote and expanded the buttercup suite to ~50 specs covering pure helpers, host/root resolution, command composition, and prefix-arg dispatch.
- Added a Makefile (test/compile/deps) and GitHub Actions CI running buttercup across Emacs 28.2, 29.4 and snapshot.
- Added the headless `quite-run` entry, a `quite--projects` registry, an optional flavor `TAG` argument, and repo-keyed `quite-register-repo`/`quite-run-repo` — quite's side of the gaffer build-backend contract.
- Fixed `quite-project-find-project` returning nil even on a successful root-list/remote hit.
- Wrote a usage README with a package comparison, an Architecture section with a Mermaid diagram, a developer CONTRIBUTING, and self-playing demo GIFs.
- Made the build architecture pluggable: a project's `:build-architecture` selects a method of the new generic `quite-build-command`, with `git-project` the default and a bundled `shell` architecture running a command's `:shell-command`. A project built by its own tooling is now an ordinary quite project rather than one quite must be kept away from.
- Made `:prefixes` and `:transforms` optional, and dropped absent components from flavor names, so a project with a single build flavor is named by `:target` alone.
- Put `quite-project-descriptors` in the `quite` customize group. It named `quite-project`, which no `defgroup` ever defined, so the option did not appear under the package.
- Replaced the hard-coded `/ssh:` TRAMP method with a `quite-remote-method` defcustom. Remote paths are now built in one place, `quite-remote--prefix`, and `quite-remote--strip-host` derives its regexp from the same variable, so it strips only a prefix quite would have written. A method needing more than a host name in its prefix is still unsupported.
- Made a TRAMP connection, not a host name, the thing quite carries internally. Quite used to read a host out of the buffer and rebuild a prefix from it, which cannot represent a user, a port or a hop: `/ssh:me@host:` silently became `/ssh:host:`, and a `#port` broke stripping outright, so the un-stripped root was prefixed a second time. Quite now copies the buffer's own prefix verbatim, spells local as `nil` rather than as a host matching `system-name`, strips with `file-local-name`, and passes the connection to command functions in place of the host. A compilation buffer's name now carries a hash of the whole connection, so two users, ports, methods or hops no longer share one buffer and one process; and a reused buffer is given the freshly resolved directory instead of the previous build's. `quite-remote-method` now applies only where quite has no name to copy. Ports work; hops are TRAMP's to declare, and the README says how.
- Fixed `quite-project-find-key-files-buffer`, which decided a key file existed by whether `find-file` returned a buffer. It always does — that is how you create a file — so the first key file always won, the rest were never tried, and a project whose real key file came second got a buffer visiting one that did not exist. The root carries the connection prefix, so that phantom was made on the remote host and making it opened a connection. It now defers to `quite-project--file-exists-p`, the sibling that already did this correctly, and uses `find-file-noselect` so a lookup stops rearranging windows.
- Bounded the project-root search at the project directory. `quite-project--path-for-buffer` matched `:project-dir` in the buffer's path and then let `locate-dominating-file` climb until it found a key file or hit the filesystem root, with nothing stopping it passing the project. A nested checkout, or any layout repeating the key file name higher up, therefore resolved to an ancestor — and quite built the wrong project silently, because that root looks valid to everything downstream. A key file above the project is now rejected, leaving the `:root-list` search to run instead. `:project-dir` is also matched literally now; unquoted, a `.` in it matched any character.
- Corrected the documentation against the connection model, and documented `:build-architecture` for users. Seven places still described a host where quite now carries a connection, including two architecture diagrams naming `remote-host`, a function that no longer exists. The README said nothing at all about `:build-architecture`, so a project built by `make` or `cask` could not discover that quite supports it — it is now shown with a `shell` example.
- Made the matrix demo press the keys it claims to press. It rendered a static table and called `quite-run`, while narrating `C-c a b` — so the GIF asserted an interaction that never happened, and that hid a real error: inside `quite-command-map` a command sits at the project's `:prefix-key` followed by the command's key, so the sequence is `C-c q a b`. It now drives the real keymap and shows the prefix argument selecting two different flavors. Added `demo-remote.el`, which demonstrates following the buffer to a remote host; its GIF is not recorded yet.
- Rewrote the `;;; Commentary:` header. It promised two functions and named one, broke off mid-sentence, and documented a `C-c C-q` binding the package never had, next to a `quite-dispatch` command it dropped in the 2020 reorganization. It now describes host and root resolution, project descriptors, flavor dispatch, `quite-define-project` and the headless `quite-run` entries. Refreshed the file header alongside it: copyright years, an `https` URL, and version 0.1.0 in place of the 0.0.1 the package long ago outgrew.

## 2021

Light housekeeping. Updated the README and version metadata, and switched to lexical binding, dropping `lexical-let`.

## 2020

The package took its real shape in December. Everything collapsed into a single `quite.el` in a major reorganization, gaining utility helpers to simplify use. Project discovery learned to check the current buffer's own directory for a key file before searching root lists, and the package acquired packaging basics: license, README, URL, Cask, and a first round of tests.

- Consolidated into one `quite.el`; removed empty/unused files, debug messages, and stale `require`s.
- Search the current buffer's directory first when resolving a project root.
- Added utility routines for stripping the host from a remote path and for invoking a function with descriptor parameters.
- Fixed the buffer command call to wrap the command function in a lambda for `quite--run-in-buffer-context`.
- Added license, README, copyright updates, a GitHub URL, a Cask file, and initial tests.

## 2019

Project inception. The initial commit landed a non-functional skeleton, and version 1 was set later that summer.

- Initial project commit (not yet functional).
- Set version to 1.
