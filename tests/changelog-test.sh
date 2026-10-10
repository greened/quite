#!/bin/sh
# changelog-test.sh: run changelog.sh on fixture repos and check what it
# writes and what it refuses.
#
# Hermetic: git reads no global or system config and no repository named by
# the caller's environment, so neither a user hook nor the repo running this
# test reaches the fixtures.
set -eu
src=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

unset GIT_DIR GIT_WORK_TREE GIT_INDEX_FILE GIT_OBJECT_DIRECTORY GIT_COMMON_DIR
unset GIT_CONFIG_PARAMETERS GIT_CONFIG_COUNT GIT_TEMPLATE_DIR
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
export GIT_AUTHOR_NAME=Fixture GIT_AUTHOR_EMAIL=fixture@example.invalid
export GIT_COMMITTER_NAME=Fixture GIT_COMMITTER_EMAIL=fixture@example.invalid

fail() { echo "changelog-test: $*" >&2; exit 1; }

# commit DATE FILE MESSAGE: change FILE and commit it on DATE.
commit() {
  echo "$2 $1" >> "$2"
  git add "$2"
  GIT_AUTHOR_DATE="$1T12:00:00Z" GIT_COMMITTER_DATE="$1T12:00:00Z" \
    git commit -q -m "$3"
}

# refuses WHY ARGS: changelog.sh must fail with WHY and leave the file alone.
refuses() {
  why=$1; shift
  cat CHANGELOG.md > "$work/before" 2>/dev/null || : > "$work/before"
  if ./changelog.sh "$@" 2> "$work/err"; then fail "did not refuse: $why"; fi
  grep -q "$why" "$work/err" || fail "refused for another reason: $(cat "$work/err")"
  after=$(cat CHANGELOG.md 2>/dev/null || :)
  [ "$after" = "$(cat "$work/before")" ] || fail "a refusal changed the file: $why"
}

no_temp_left() {
  ! ls -a | grep -q '^\.CHANGELOG\.md\.' || fail "a temp file was left behind"
}

d=$(printf '\342\200\224')
today=$(date +%Y-%m-%d)

git init -q "$work/repo"
cp "$src/changelog.sh" "$work/repo/"
cd "$work/repo"

printf '# Changelog\n\nIntro.\n\n## v0.1.0 %s 2026\n\n- Old text.\n' "$d" \
  > CHANGELOG.md
git add CHANGELOG.md
GIT_COMMITTER_DATE=2026-01-01T12:00:00Z git commit -q -m 'first'
git tag -a -m 'quite 0.1.0' v0.1.0

commit 2026-01-02 a.txt 'quite: a plain change

The lead paragraph,
  wrapped onto two lines.

A second paragraph.

Co-Authored-By: Fixture'
commit 2026-01-03 a.txt 'other: a foreign prefix stays'
commit 2026-01-04 a.txt 'A body of trailers only!

Co-Authored-By: Fixture'
commit 2026-01-05 a.txt 'Fix -100% of a\n path, caf'"$(printf '\303\251')"'

Text with 50% and a back\slash.'
commit 2026-01-06 a.txt 'A commit to leave out

It has text.

Changelog: skip'

refuses 'is required'
refuses 'not a version' --version 1.2
refuses 'not a version' --version 1..2
refuses 'already exists' --version 0.1.0

./changelog.sh --version 0.2.0 --stdout > "$work/out"
git diff --quiet -- CHANGELOG.md || fail "--stdout changed the file"
./changelog.sh --version 0.2.0 > /dev/null
no_temp_left

printf '%s\n' '# Changelog' '' 'Intro.' '' \
  "## v0.2.0 $d $today" '' \
  "- Fix -100% of a\\n path, caf$(printf '\303\251'). Text with 50% and a back\\slash." \
  '- A body of trailers only!' \
  '- other: a foreign prefix stays.' \
  '- a plain change. The lead paragraph, wrapped onto two lines.' '' \
  "## v0.1.0 $d 2026" '' '- Old text.' > "$work/want"
cmp -s CHANGELOG.md "$work/want" ||
  fail "unexpected CHANGELOG.md:$(diff "$work/want" CHANGELOG.md)"
sed -n '5,10p' "$work/want" | cmp -s - "$work/out" ||
  fail "--stdout printed another section:$(cat "$work/out")"

refuses 'uncommitted changes' --version 0.3.0
cp CHANGELOG.md "$work/saved" && rm CHANGELOG.md
refuses 'uncommitted changes' --version 0.3.0
cp "$work/saved" CHANGELOG.md
git commit -q -am 'CHANGELOG for 0.2.0'
git tag -a -m 'quite 0.2.0' v0.2.0
refuses 'nothing landed' --version 0.3.0

# A branch that wrote its own entry under "## Unreleased".
printf '%s\n' '# Changelog' '' 'Intro.' '' '## Unreleased' '' \
  '- A hand entry,' '  wrapped.' '' "## v0.2.0 $d $today" '' '- Kept.' \
  > CHANGELOG.md
GIT_AUTHOR_DATE=2026-01-07T12:00:00Z GIT_COMMITTER_DATE=2026-01-07T12:00:00Z \
  git commit -q -am 'A branch that wrote its own entry'
hand=$(git rev-parse HEAD)
commit 2026-01-08 a.txt 'A later change'
refuses 'edited CHANGELOG.md' --version 0.3.0
refuses 'not a commit' --version 0.3.0 --from no-such-rev
./changelog.sh --version 0.3.0 --from "$hand" > /dev/null
printf '%s\n' '# Changelog' '' 'Intro.' '' "## v0.3.0 $d $today" '' \
  '- A later change.' '- A hand entry,' '  wrapped.' '' \
  "## v0.2.0 $d $today" '' '- Kept.' > "$work/want"
cmp -s CHANGELOG.md "$work/want" ||
  fail "Unreleased was not folded:$(diff "$work/want" CHANGELOG.md)"

# An empty "## Unreleased" right above a release still leaves a blank line.
git commit -q -am 'CHANGELOG for 0.3.0'
git tag -a -m 'quite 0.3.0' v0.3.0
printf '%s\n' '# Changelog' '' '## Unreleased' '' "## v0.3.0 $d $today" \
  > CHANGELOG.md
git commit -q -am 'An empty Unreleased'
empty=$(git rev-parse HEAD)
commit 2026-01-09 a.txt 'Another change'
./changelog.sh --version 0.4.0 --from "$empty" > /dev/null
printf '%s\n' '# Changelog' '' "## v0.4.0 $d $today" '' '- Another change.' '' \
  "## v0.3.0 $d $today" > "$work/want"
cmp -s CHANGELOG.md "$work/want" ||
  fail "an empty Unreleased was mishandled:$(diff "$work/want" CHANGELOG.md)"

git commit -q -am 'CHANGELOG for 0.4.0'
git tag -a -m 'quite 0.4.0' v0.4.0
commit 2026-01-10 a.txt 'Only a skipped commit

Changelog: skip'
refuses 'carries Changelog: skip' --version 0.5.0
no_temp_left

# A repo with no release tag and no CHANGELOG.md.
git init -q "$work/bare"
cp "$src/changelog.sh" "$work/bare/"
cd "$work/bare"
commit 2026-02-01 a.txt 'The root'
root=$(git rev-parse HEAD)
git tag v2-rc
git tag pre-scrub-backup
commit 2026-02-02 a.txt 'A first change'
refuses 'no tag matches' --version 0.1.0
./changelog.sh --version 0.1.0 --from "$root" > /dev/null
printf '%s\n' '# Changelog' '' "## v0.1.0 $d $today" '' \
  '- A first change.' > "$work/want"
cmp -s CHANGELOG.md "$work/want" ||
  fail "unexpected new CHANGELOG.md:$(diff "$work/want" CHANGELOG.md)"
[ "$(stat -c %a CHANGELOG.md 2>/dev/null || stat -f %Lp CHANGELOG.md)" = 644 ] ||
  fail "the new CHANGELOG.md is not mode 644"
refuses 'untracked' --version 0.1.0 --from "$root"
no_temp_left

echo "changelog-test: ok"
