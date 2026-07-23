# Contributing to quite

quite is one Emacs-Lisp file (`quite.el`). This is the developer's map: the
architecture, the two entry surfaces, the project data model, the public API, the
extension points, and the important internals.

## Layout

- `quite.el` — the whole package (one file, `;;;`-sectioned).
- `tests/quite-tests.el` — buttercup specs (pure; no network/repo).
- `README.md` — user-facing overview + comparison.

## Architecture

quite has a simple job: **organize a matrix of build commands and point `compile`
at the right host and root.** Remoteness is free — `compile` with a remote
`default-directory` runs on the remote host via TRAMP; quite just resolves *which*
host/root and *which* command. A generic **caller** (a keybinding, a Hydra, or an
orchestrator) drives quite; the **build host** is local or any TRAMP remote.

```mermaid
flowchart TB
  subgraph quite
    direction TB
    DEF["quite-define-project<br/>commands × transforms × prefixes"]
    REG[("quite--projects<br/>registry")]
    MAP["quite-command-map + Hydra heads"]
    RUN["quite-run · headless"]
    CTX["host/root from buffer<br/>remote-host · find-project"]
    CMD["quite--make-build-command → compile"]
    DEF --> REG
    DEF --> MAP
    REG --> RUN
    MAP --> CTX --> CMD
    RUN --> CMD
  end
  CALLER["caller"]
  HOST[("build host<br/>local / TRAMP")]
  CALLER -. "keys / Hydra" .-> MAP
  CALLER -. "quite-run" .-> RUN
  CMD == "compile in default-directory" ==> HOST
  classDef ext fill:#eee,stroke:#888,stroke-dasharray:5 3;
  class CALLER,HOST ext;
```

## Two entry surfaces

1. **The per-project matrix** — `quite-define-project` takes a project plist and
   composes *commands × transforms × prefixes* into (a) bindings in
   `quite-command-map` (via `quite-bind-project-commands`) and (b) Hydra heads
   (via `quite-project-hydra-heads`). Interactive: the raw prefix argument selects
   a *flavor* (`quite--prefix-arg-index` / `quite--dispatch`), and the command
   runs in a named compilation buffer whose host/root come from the **current
   buffer** (`quite-remote-host-for-current-buffer`, `quite-project-find-project`).
2. **`quite-run` (headless)** — `quite-run NAME COMMAND &optional DIR BUFFER-NAME`
   looks the project up in `quite--projects`, builds the command with
   `quite--make-build-command`, and runs it via `compile` in `DIR` (a remote DIR
   builds remotely) — no keymap, Hydra, or file-visiting buffer required. This is
   the integration entry a tool (e.g. a PR-work orchestrator) calls; it reuses the
   same build command as the matrix, so headless and interactive builds match.

Both bottom out in `quite--make-build-command`, which returns a
`(HOST ROOT SUBDIR BUFFER TAG)` function that runs
`"PREFIX git GIT-NAME COMMAND TAG POSTFIX"` via `compile`. Execution is ordinary
`compile`; quite only assembles the command line and sets the directory.

## Data model

- **Project plist** (argument to `quite-define-project`, also stored in
  `quite--projects` keyed by `:name`):
  - `:git-name` — the git-project name in the compile command.
  - `:name` — project name (buffer names, Hydra columns, `quite-run` key).
  - `:descriptor` — a `quite-project-descriptors` plist (below).
  - `:prefix-key` — key prefix (after `C-c`) for the bindings.
  - `:target` — target string used in flavor (tag) names.
  - `:commands` — list of `(:name :command :key)` plists.
  - `:prefixes` — list of prefix-name strings; **list order = the C-u index**
    (position 0 = no prefix, 1 = one `C-u`, …).
  - `:transforms` — list of `(:name :func)` plists; `:func` maps a command key to
    its variant (e.g. `identity`, `upcase`).
  - `:command-prefix` / `:command-postfix` — optional shell text around the
    compile command (e.g. activating a venv).
- **Descriptor plist** (`quite-project-descriptors`): `:project-dir`,
  `:root-list` (candidate roots to search on the host), `:key-files` (files that
  identify the root).
- **`quite--projects`** — alist `NAME → project-plist`, populated by
  `quite-define-project`; the lookup table behind `quite-run`.
- **`quite-descriptors`** — a separate, general dispatch list for `quite-execute`:
  plists of `(:function :tag)` chosen by prefix argument (independent of the
  per-project matrix).

## Public API

- **Define / run:** `quite-define-project` (usual overlay entry point),
  `quite-run` (headless), `quite-execute` (prefix-dispatch over
  `quite-descriptors`), `quite-bind-project-commands`, `quite-project-hydra-heads`.
- **Host/root:** `quite-remote-host-for-current-buffer`,
  `quite-remote-create-remote-path`, `quite-remote-localhost`,
  `quite-project-find-project`.
- **Dispatch:** `quite-generate-dispatcher`, `quite-generate-buffer-dispatcher`.
- **Config (defcustom):** `quite-descriptors`, `quite-project-descriptors`,
  `quite-flavor-abbreviations` (regexp→replacement, shortens Hydra head labels).

## Important internals

- `quite--make-build-command` — the command builder (used by both surfaces).
- `quite--dispatch` / `quite--prefix-arg-index` — prefix-argument → flavor index
  (nil/0 → #1, 4 → #2, 16 → #3, …).
- `quite--run-in-buffer-context` / `quite--generate-buffer-action` /
  `quite--make-buffer-name` — run a command in a (created or reused) named
  compilation buffer.
- `quite-project-find-project` — resolves the root from the current buffer, or by
  searching `:root-list` on the (possibly remote) host for `:project-dir` +
  `:key-files`; returns the prefix-less root. (Note: its search branch must
  *return* the found root — a missing else once made it return nil on a hit; keep
  the "return found-root on success" arm and its spec.)

## Local setup

Load quite from a checkout — elpaca `:try-local`, `package-vc`, `straight`, or a
plain `load-path` + `require`. Needs Emacs ≥ 28 and [`hydra`]; the tests need
[`buttercup`].

## Running the checks

There's no `check.sh` yet; the equivalent invocation (byte-compile with warnings
as errors, then buttercup) is:

```sh
ADD='(dolist (d (directory-files "~/.emacs.d/elpaca/builds" t "^[^.]"))
       (when (file-directory-p d) (add-to-list (quote load-path) d)))'
emacs -batch -Q --eval "$ADD" --eval '(setq byte-compile-error-on-warn t)' \
  -L . -f batch-byte-compile quite.el && rm -f *.elc
emacs -batch -Q --eval "$ADD" -L . -L tests -l buttercup -f buttercup-run-discover
```

Green means **no byte-compile warnings and every spec passes.**

## Conventions

- Docstrings wrap at 80 columns.
- Specs exercise the pure layer — host/root resolution mocks `file-exists-p` /
  `system-name`; dispatch and command composition are tested without a real
  `compile`.
- Add a spec with any behavior change.
- quite is a **generic** build organizer — it names no consumer. New integration
  points (like `quite-run`) are plain functions/registries any caller can use.

[`hydra`]: https://github.com/abo-abo/hydra
[`buttercup`]: https://github.com/jorgenschaefer/emacs-buttercup
