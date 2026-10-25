#!/bin/sh
# What rests on an assumption? Usage: scripts/assumption-impact.sh A-007
#
# Lists the commits whose messages name the assumption (in an Assumes: trailer), the
# files they touched, and then the later commits that touched any of those files,
# which may have built on them without naming it.
set -e
id="$1"
[ -n "$id" ] || { echo "usage: $0 A-nnn" >&2; exit 1; }

direct=$(git log --format=%H --grep="^Assumes:.*$id\\b" phase-2)
if [ -z "$direct" ]; then
  echo "No commit names $id."
  exit 0
fi

echo "== Commits that name $id =="
for c in $direct; do
  git log -1 --format='%h %ad %s' --date=short "$c"
done

echo
echo "== Files they touched =="
files=$(for c in $direct; do git show --name-only --format= "$c"; done | sort -u)
echo "$files"

first=$(echo "$direct" | tail -n 1)
echo
echo "== Later commits that touched those files (they may build on them) =="
# shellcheck disable=SC2086
git log --format='%h %ad %s' --date=short "$first"..phase-2 -- $files | grep -v "$(git log -1 --format=%h "$first")" || true
