#!/bin/sh
# Commit what is staged, with the trailers the autonomous run uses.
#
#   ASSUMES="A-003 A-007" TESTS="make test passes" \
#     scripts/auto-commit.sh "Subject line" <<'MSG'
#   Body...
#   MSG
#
# ASSUMES may be empty. TESTS defaults to "make test passes". There is no Decision trailer:
# since 2026-10-30 a decision is written in the devlog and in the commit's body.
set -e
subject="$1"
[ -n "$subject" ] || { echo "usage: $0 subject < body" >&2; exit 1; }
body=$(cat)
message=$(printf '%s\n\n%s\n' "$subject" "$body")
trailers=$(printf 'Tests: %s' "${TESTS:-make test passes}")
[ -n "$ASSUMES" ] && trailers=$(printf 'Assumes: %s\n%s' "$ASSUMES" "$trailers")
message=$(printf '%s\n\n%s' "$message" "$trailers")
message=$(printf '%s\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01DJDtn3s1tWS6zt9uS8qRHE' "$message")
git commit -q -m "$message"
git log -1 --format='%h %s'
