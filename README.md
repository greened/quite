# quite — QUIck Transparent Execution

`quite` runs a project's build and development commands **on the host where the
file you are editing actually lives** — your local machine when the current
buffer visits a local file, or a remote host (over TRAMP) when it visits a
remote file — without you having to think about which. It works out the host
and the project root from the current buffer, then runs the command there in a
dedicated, predictably named compilation buffer.

Its distinctive feature is **prefix-argument dispatch to command _flavors_**: a
single key runs, say, `build`, and the prefix argument selects *which* flavor of
that build to run — no prefix for one flavor, `C-u` for the next, `C-u C-u` for
the one after, and so on. `quite-define-project` composes a whole matrix of
*commands × transforms × flavors* into a keymap and a
[Hydra](https://github.com/abo-abo/hydra).

> Status: small, single-author package; the public surface is stable but the
> configuration is deliberately explicit (you describe your projects and command
> vocabulary yourself). See **Caveats**.

## What it does

- **The host follows the buffer.** `quite` inspects the current buffer with
  `file-remote-p`/`buffer-file-name`. A remote (`/ssh:host:…`) buffer runs the
  command on `host`; a local buffer runs it on the local machine. You use the
  same key either way.
- **Project discovery.** A *project descriptor* is a plist of `:project-dir`,
  `:root-list` and `:key-files`. `quite` either infers the project root from the
  buffer's own path, or searches the `:root-list` directories **on the resolved
  host** for `:project-dir` containing one of the `:key-files`.
- **Flavor dispatch.** Each command is bound once; the raw prefix argument
  indexes into an ordered list of flavors (e.g. `release` then `debug`), and the
  chosen flavor's tag is passed through to the shell command.
- **Composition.** `quite-define-project` turns a compact spec into (a) bindings
  in `quite-command-map` and (b) Hydra heads, so a project's whole build matrix
  is a few keystrokes away.

## Example

```elisp
(require 'quite)

;; Reach quite's commands under a prefix of your choosing.
(global-set-key (kbd "C-c q") quite-command-map)

(quite-define-project
 (list :git-name    "be"                       ; git-project sub-command name
       :name        "llvm"                      ; used in buffer names / hydra
       :descriptor  '(:project-dir "llvm-project"
                      :root-list ("~/ws")
                      :key-files ("Makefile"))
       :prefix-key  "r"                          ; keys live under C-c q r ...
       :target      "llvm-project"               ; used in flavor (tag) names
       :commands    '((:name "configure" :command "configure" :key "f")
                      (:name "build"     :command "build"     :key "b"))
       :prefixes    '("devrel" "devdbg")         ; list ORDER = the C-u index
       :transforms  (list (list :name "local"   :func #'identity)
                          (list :name "cluster" :func #'upcase))))
```

With the above, `C-c q r b` builds `llvm-project`'s `devrel` flavor locally;
`C-u C-c q r b` builds the `devdbg` flavor; `C-c q r B` (upcased key) builds the
`cluster` variant; and `C-c q r h` pops the project's build Hydra. If the buffer
you invoke from is remote, every one of those runs on the remote host instead —
same keys.

## How it works (the moving parts)

| Function | Role |
|---|---|
| `quite-remote-host-for-current-buffer` | resolves local vs. remote host from the buffer |
| `quite-project-find-project` | finds the project root on that host (buffer-relative or by searching `:root-list`) |
| `quite--dispatch` / `quite--prefix-arg-index` | maps the raw prefix argument to a flavor index |
| `quite-generate-buffer-dispatcher` | builds the interactive command that runs a flavor in a named buffer |
| `quite-define-project` | composes commands × transforms × flavors into `quite-command-map` + Hydra heads |

Execution itself is ordinary `compile`, so remoteness is carried by
`default-directory`/TRAMP — `quite`'s job is to *point it at the right host and
root* and to organize the command matrix.

## How it compares

Short version: the **remote-execution** part of `quite` is not, by itself,
unique — any package that runs `compile` with a remote `default-directory` (which
is most of them) already runs on the remote host, because that is a TRAMP
feature. What `quite` adds on top is (1) resolving the host **and** the project
root *from the current buffer*, including searching a list of candidate roots on
that host, and (2) the **prefix-argument flavor matrix** with keymap/Hydra
composition. If you want a build/test command bound per project, the mainstream
packages do that better; if you want one key to reach a *grid* of build variants
that transparently follows you between local and remote trees, that is `quite`'s
niche.

### vs. `project.el` (built-in)

`project.el` is Emacs's built-in project framework. `project-compile` and
`project-shell-command` run in the project root, and because they inherit
`default-directory`, they already run on the remote host for a remote project.
But `project.el` offers no notion of build *flavors*, no prefix-dispatched
command grid, and its project detection is VC/marker-based rather than
"search these roots on this host for this directory." It is general project
management; command execution is intentionally minimal.

### vs. Projectile

Projectile is the heavyweight general project manager (navigation, search,
replace, and much more). Its `projectile-compile-project` /
`projectile-test-project` / `projectile-run-project` remember a *single*
configurable command per project (with history) and are TRAMP-aware. That covers
"run my build" well, but it is one command per action, not an indexed matrix of
flavors, and Projectile brings a large surface area you may not want if commands
are all you need.

### vs. projection

[`projection`](https://github.com/mohkale/projection) is the closest in spirit:
a `project.el` extension that generates *project-type-aware* commands with almost
exactly `quite`'s vocabulary (configure / build / test / run / package /
install), supports multiple command options per type, and is remote-aware. It is
better maintained and more automatic (it *detects* the toolchain), but it is
driven by project *type* rather than by an explicit descriptor + prefix-flavor
matrix, and it has no direct equivalent of "one key, prefix-selected flavor."

### vs. `compile` / `recompile`

These are the primitive `quite` builds on. `compile` runs a shell command in
`default-directory` (remote if the buffer is remote); `recompile` repeats it.
No project resolution, no flavors, no composition — you supply the full command
each time.

### Adjacent, different niches

- **prodigy.el** — manages long-running *services/daemons* (start/stop/restart),
  not one-shot build commands.
- **emacs-taskrunner / helm-make / makefile-executor** — *discover* tasks from
  Makefiles, npm, etc. and run them; no host-from-buffer model and no flavor
  matrix.

### Feature matrix

| Capability | quite | project.el | Projectile | projection | compile |
|---|:--:|:--:|:--:|:--:|:--:|
| Runs on remote host (TRAMP) | ✅ | ✅ | ✅ | ✅ | ✅ |
| Host **inferred from the current buffer** | ✅ | ➖¹ | ➖¹ | ➖¹ | ➖¹ |
| Searches a **list of candidate roots** on the host | ✅ | ❌ | ❌ | ❌ | ❌ |
| **Prefix-arg flavor matrix** (one key → variants) | ✅ | ❌ | ❌ | ❌ | ❌ |
| Command **keymap + Hydra** composition | ✅ | ❌ | ➖² | ➖² | ❌ |
| Configure/build/test/install command vocabulary | ✅ | ❌ | ➖³ | ✅ | ❌ |
| General project mgmt (nav, search, VC) | ❌ | ✅ | ✅ | ➖⁴ | ❌ |
| Built-in / actively maintained by a team | ❌ | ✅ | ✅ | ➖ | ✅ |
| Zero-config auto-detection of toolchain | ❌ | ➖ | ✅ | ✅ | ❌ |

¹ Runs remotely because `default-directory` is remote, but the host is the
project's, not resolved per-buffer with candidate-root search.
² Achievable with user glue, not built in. ³ Single configurable command per
action. ⁴ Defers to `project.el` for management.

### quite: pros and cons

| Pros | Cons |
|---|---|
| Same keys run locally or remotely — execution follows the buffer's host | Niche; overlaps with better-maintained general packages |
| Prefix-argument **flavor matrix**: one key, many build variants | Idiosyncratic flavor model with a learning curve |
| Explicit **multi-root search** on the resolved host | You must hand-write descriptors + the command/flavor matrix (no auto-detection) |
| Composes cleanly into a keymap + Hydra | Small, single-author project |
| Lightweight and focused; builds on plain `compile` | Its remote transparency largely *is* TRAMP + `default-directory`, not unique |

**Use `quite`** when you routinely build the *same* trees across local and
remote hosts and want one key to reach a grid of build flavors. **Prefer
`projection` or Projectile** when you want automatic, toolchain-aware commands
with little configuration, or a full project-management suite.

## Installation

`quite` is not on MELPA. With `use-package` + a fetcher (e.g. elpaca/straight):

```elisp
(use-package quite
  :ensure (:fetcher github :repo "greened/quite")
  :config
  (global-set-key (kbd "C-c q") quite-command-map))
```

## Testing

Tests use [buttercup](https://github.com/jorgenschaefer/emacs-buttercup) and
[Cask](https://github.com/cask/cask):

```sh
cask install     # once, to fetch dev dependencies
make test        # cask exec buttercup -L . tests
```

CI runs the suite across several Emacs versions on every push (see
`.github/workflows/test.yml`).

## License

GPL-3.0-or-later. See `LICENSE.md`.
