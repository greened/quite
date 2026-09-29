#!/bin/sh
# check.sh -- byte-compile (warnings fatal) + buttercup specs for quite.
#
# Env:
#   EMACS          Emacs binary (default: emacs)
#   ELPACA_BUILDS  dir whose subdirs hold the deps (default: ~/.emacs.d/elpaca/builds)
set -eu
cd "$(dirname "$0")"
EMACS="${EMACS:-emacs}"
ELPACA_BUILDS="${ELPACA_BUILDS:-$HOME/.emacs.d/elpaca/builds}"

ADD="(dolist (d (directory-files \"$ELPACA_BUILDS\" t \"^[^.]\"))
       (when (file-directory-p d) (add-to-list 'load-path d)))"

rm -f ./*.elc tests/*.elc

echo "== byte-compile (warnings are errors) =="
"$EMACS" -batch -Q --eval "$ADD" --eval "(setq byte-compile-error-on-warn t)" \
  -L . -f batch-byte-compile quite.el

rm -f ./*.elc tests/*.elc

echo "== buttercup =="
"$EMACS" -batch -Q --eval "$ADD" -L . -L tests \
  -l buttercup -f buttercup-run-discover
