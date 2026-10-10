#!/bin/sh
# changelog.sh: write the CHANGELOG.md section for a release from its commits.
#
# The section goes above the existing entries and starts with a "## vX.Y.Z
# <em dash> DATE" heading, DATE being today. Each commit landed since the base
# gives one "- SUBJECT. LEAD" bullet below it, newest first, with any "quite: "
# prefix cut and the body's lead paragraph joined onto the same line. Git's
# own trailer parsing decides what the trailers are, so a body made only of
# trailers gives a bullet with the subject alone. A commit with a
# "Changelog: skip" trailer gives no entry. Existing entries are not touched,
# except that a "## Unreleased" heading at the top is replaced by the new
# heading, so its hand-written bullets follow the generated ones in the
# release's section. Review and edit the result, then commit it and tag that
# commit vX.Y.Z, so the next release starts from it.
#
# Usage: ./changelog.sh --version X.Y.Z [--from REV] [--stdout]
#   --version X.Y.Z  the version being released, whose tag must not exist yet
#   --from REV       start after REV, not after the newest release tag. Every
#                    commit up to and including REV is skipped, so their
#                    entries must already be in the file.
#   --stdout         print the generated section and leave CHANGELOG.md alone
#
# It refuses, and writes nothing, when the range holds no entry, when
# CHANGELOG.md is untracked, deleted or has uncommitted changes (a second run
# would add every entry again) or when a commit in the range already edited
# CHANGELOG.md (its entry would be written twice). That refusal lists those
# commits newest first. Pass --from the first one listed to start after it
# instead.
set -eu
cd "$(dirname "$0")"
PKG=quite
FILE=CHANGELOG.md
TAGS='v[0-9]*.[0-9]*.[0-9]*'

die() { echo "changelog.sh: $*" >&2; exit 1; }

from= stdout= version=
while [ $# -gt 0 ]; do
  case $1 in
    --from) [ $# -ge 2 ] || die "--from needs a revision"; from=$2; shift 2 ;;
    --version) [ $# -ge 2 ] || die "--version needs X.Y.Z"; version=$2; shift 2 ;;
    --stdout) stdout=1; shift ;;
    *) die "unknown argument: $1" ;;
  esac
done

case $version in
  '') die "--version X.Y.Z is required" ;;
  *[!0-9.]* | .* | *. | *..* | *.*.*.* ) die "not a version X.Y.Z: $version" ;;
  *.*.*) ;;
  *) die "not a version X.Y.Z: $version" ;;
esac
git rev-parse --verify --quiet "refs/tags/v$version" > /dev/null &&
  die "tag v$version already exists"

if [ -z "$from" ]; then
  from=$(git describe --tags --abbrev=0 --match "$TAGS" HEAD 2>/dev/null) ||
    die "no tag matches $TAGS, so pass --from REV"
fi
base=$(git rev-parse --verify --quiet "$from^{commit}") ||
  die "not a commit: $from"

commits=$(git rev-list --topo-order --no-merges "$base..HEAD")
[ -n "$commits" ] || die "nothing landed since $from"

# A deleted CHANGELOG.md counts as an uncommitted change, or the write
# below would start a fresh file and drop every older entry.
if [ -z "$stdout" ]; then
  if git cat-file -e "HEAD:$FILE" 2> /dev/null; then
    git diff --quiet HEAD -- "$FILE" ||
      die "$FILE has uncommitted changes, so commit or drop them first"
  elif [ -e "$FILE" ]; then
    die "$FILE is untracked, so commit or remove it first"
  fi
fi
edited=$(git rev-list --topo-order --no-merges "$base..HEAD" -- "$FILE")
if [ -n "$edited" ]; then
  git log --topo-order --no-walk --format='  %h %s' $edited >&2
  die "the commits above edited $FILE, so pass --from the first one listed"
fi

# Built from octal escapes so this file stays ASCII.
emdash=$(printf '\342\200\224')
entries=$(mktemp)
trap 'rm -f "$entries" ${new:+"$new"}' EXIT

printf '## v%s %s %s\n\n' "$version" "$emdash" "$(date +%Y-%m-%d)" > "$entries"
n=0
for c in $commits; do
  if git log -1 --format='%(trailers:key=Changelog,valueonly,unfold)' "$c" |
       grep -qix '[[:space:]]*skip[[:space:]]*'; then
    continue
  fi
  n=$((n + 1))
  subject=$(git log -1 --format=%s "$c")
  subject=${subject#"$PKG: "}
  case $subject in *[.!?]) ;; *) subject=$subject. ;; esac
  # When git finds a trailer block, it is the body's last paragraph.
  trailers=$(git log -1 --format='%(trailers:only,unfold)' "$c")
  lead=$(git log -1 --format=%b "$c" | awk -v trailers="${trailers:+1}" '
    /^[ \t]*$/ { if (open) { p++; open = 0 } next }
    { sub(/^[ \t]+/, ""); sub(/[ \t]+$/, "")
      if (!open) { open = 1; para[p + 1] = $0 }
      else para[p + 1] = para[p + 1] " " $0 }
    END { if (open) p++
          if (trailers) p--
          if (p >= 1) print para[1] }')
  printf -- '- %s%s\n' "$subject" "${lead:+ $lead}" >> "$entries"
done
[ "$n" -gt 0 ] || die "every commit since $from carries Changelog: skip"

if [ -n "$stdout" ]; then
  cat "$entries"
  exit 0
fi

# Next to the file, so the rename is atomic on its file system.
new=$(mktemp "./.$FILE.XXXXXX")
# mktemp makes the file private, and CHANGELOG.md is not.
chmod 644 "$new"
# A top "## Unreleased" heading gives way to the new one, and the blank lines
# after it go too, so its bullets continue the generated list.
if [ -e "$FILE" ]; then cat "$FILE"; else printf '# Changelog\n\n'; fi |
  awk -v entries="$entries" '
    function put() { while ((getline l < entries) > 0) print l; done = 1 }
    !done && /^## Unreleased[ \t]*$/ { put(); fold = 1; next }
    !done && /^## / { put(); print "" }
    fold && /^[ \t]*$/ { next }
    fold { fold = 0; if (/^## /) print "" }
    { print }
    END { if (!done) put() }' > "$new"
mv "$new" "$FILE"
new=
echo "changelog.sh: wrote v$version with $n entries to $FILE"
