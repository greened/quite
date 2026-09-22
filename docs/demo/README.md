# Demo scenarios

Self-playing scripts that produce the GIFs in the top-level README. Each runs a
scripted quite session in a headless `emacs -nw`, captured with
[asciinema](https://asciinema.org) and rendered to a GIF with
[agg](https://github.com/asciinema/agg). They use **example data only** — the
`app` project defined in `demo-common.el`, whose build command is stubbed to echo
the command line, so nothing real is executed and there's no network.

## Files

| Script | GIF | Shows |
|---|---|---|
| `demo-matrix.el` | `quite.gif` | the command × flavor grid, then **real keypresses** — `C-c q a b` and `C-u C-c q a b` — showing the prefix argument select two flavors |
| `demo-run.el` | `quite-run.gif` | `quite-run` — the headless entry a tool/orchestrator calls |
| `demo-remote.el` | `quite-remote.gif` | quite following the buffer to a REMOTE host over TRAMP — the headline feature. Needs an ssh alias `demo-host` and the project present on the remote at `/tmp/quite-demo/app` |
| `demo-common.el` | — | shared setup: load quite + hydra, define the stubbed `app` project |

## Prerequisites

- `quite` and its dependency [`hydra`](https://github.com/abo-abo/hydra) on
  `load-path`. `demo-common.el` loads quite from the checkout (two levels up) and
  adds your elpaca build dir (for hydra) when present — adjust for your package
  manager.
- The [`agg`](https://github.com/asciinema/agg) and `asciinema` binaries.

## Recording one

Run from this directory so the scripts self-locate the repo:

```sh
cd docs/demo
export TERM=xterm-256color   # emacs -nw refuses TERM=dumb
asciinema rec --window-size 100x30 --overwrite \
  -c "emacs -nw -Q -l demo-matrix.el" quite.cast
agg --theme monokai quite.cast ../media/quite.gif
```

Each script drives itself and exits (`kill-emacs`) when done.

**Record from a shell that has job control.** `asciinema` puts the terminal
into raw mode. Started from a shell without job control — `sh -c`, an editor's
`start-process`, or a `tmux new-session` whose command *is* the recorder — it
sits in a background process group, takes `SIGTTOU` and stops. The symptom is
silence: no cast file, no error, and `ps` reporting state `T`. Type the command
in a terminal, or send it to an interactive `tmux` pane with `send-keys`.

`demo-matrix.el` drives the **real keymap** with `execute-kbd-macro`, so it
needs a project on disk for the interactive path to resolve: it creates
`/tmp/quite-demo/app` with a `Makefile` and a source file, and visits that file.
Remove `/tmp/quite-demo` between recordings for a clean run.

**Never narrate a keystroke the script does not send.** An earlier version of
`demo-matrix.el` printed "C-c a b ..." while actually calling `quite-run`, so
the GIF asserted an interaction that never happened — and that hid a real bug:
the key sequence in the message was wrong, because inside `quite-command-map`
a command sits at the project's `:prefix-key` followed by the command's key
(`C-c q` + `a` + `b`), not at the command key alone.
