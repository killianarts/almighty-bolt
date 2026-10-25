#!/bin/sh
# Commit what is staged, with the trailers the autonomous run uses.
#
#   ASSUMES="A-003 A-007" DECISION="0012" TESTS="make test passes" \
#     scripts/auto-commit.sh "Subject line" <<'MSG'
#   Body...
#   MSG
#
# ASSUMES and DECISION may be empty. TESTS defaults to "make test passes".
set -e
subject="$1"
[ -n "$subject" ] || { echo "usage: $0 subject < body" >&2; exit 1; }
body=$(cat)
message=$(printf '%s\n\n%s\n' "$subject" "$body")
[ -n "$DECISION" ] && message=$(printf '%s\n\nDecision: %s' "$message" "$DECISION")
[ -n "$ASSUMES" ] && message=$(printf '%s\nAssumes: %s' "$message" "$ASSUMES")
message=$(printf '%s\nTests: %s' "$message" "${TESTS:-make test passes}")
message=$(printf '%s\n\nCo-Authored-By: Claude Sonnet 5.5 <noreply@anthropic.com>\nClaude-Session: https://claude.ai/code/session_01DJDtn3s1tWS6zt9uS8qRHE' "$message")
git commit -q -m "$message"
git log -1 --format='%h %s'
