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
| `demo-matrix.el` | `quite.gif` | the command × flavor grid quite composes, then running `build` / `check` via `compile` |
| `demo-run.el` | `quite-run.gif` | `quite-run` — the headless entry a tool/orchestrator calls |
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
