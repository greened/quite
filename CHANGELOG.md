# Changelog

`quite` is an Emacs package that runs a project's build and development commands on the host where the edited file lives — local or remote over TRAMP — with prefix-argument dispatch selecting command flavors.

Reconstructed from git history; no release tags yet.

## 2026

The dormant package woke up and matured into a shippable tool. The build-composition machinery moved out of personal init and into the package proper (`quite-define-project`, built on independent binding and hydra-head builders), with pre-existing bugs and byte-compile warnings cleared. A headless surface was added so orchestrators like gaffer can drive builds without a keymap or a file-visiting buffer, and the project grew a real test suite, CI, docs, and demo GIFs.

- Fixed remote host stripping to handle hosts with `-`, `.` and `_`.
- Lifted the command × flavor matrix composition into the package; dropped the unused `:prefix` field; fixed `quite--buffer-format` and `quite-execute`.
- Corrected `quite--extract-subdir` to return the full subpath below the project dir, trailing-slash insensitive.
- Rewrote and expanded the buttercup suite to ~50 specs covering pure helpers, host/root resolution, command composition, and prefix-arg dispatch.
- Added a Makefile (test/compile/deps) and GitHub Actions CI running buttercup across Emacs 28.2, 29.4 and snapshot.
- Added the headless `quite-run` entry, a `quite--projects` registry, an optional flavor `TAG` argument, and repo-keyed `quite-register-repo`/`quite-run-repo` — quite's side of the gaffer build-backend contract.
- Fixed `quite-project-find-project` returning nil even on a successful root-list/remote hit.
- Wrote a usage README with a package comparison, an Architecture section with a Mermaid diagram, a developer CONTRIBUTING, and self-playing demo GIFs.

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
