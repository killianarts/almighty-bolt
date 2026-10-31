#!/bin/sh
# Commit what is staged, with a Tests: trailer and the co-author's trailers.
#
#   MODEL="Claude Opus 5.5" SESSION="https://claude.ai/code/session_..." \
#   TESTS="make test passes" scripts/auto-commit.sh "Subject line" <<'MSG'
#   Body...
#   MSG
#
# MODEL is required: the model that wrote the commit, for its Co-Authored-By line.
# SESSION is the session's link, for a Claude-Session line; left out when unset.
# TESTS defaults to "make test passes". A decision, and the evidence for it, goes in the body.
set -e
subject="$1"
[ -n "$subject" ] || { echo "usage: MODEL=... $0 subject < body" >&2; exit 1; }
[ -n "$MODEL" ] || { echo "$0: set MODEL to the model's name, e.g. MODEL=\"Claude Opus 5.5\"" >&2; exit 1; }
body=$(cat)
message=$(printf '%s\n\n%s\n' "$subject" "$body")
trailers=$(printf 'Tests: %s' "${TESTS:-make test passes}")
message=$(printf '%s\n\n%s' "$message" "$trailers")
message=$(printf '%s\n\nCo-Authored-By: %s <noreply@anthropic.com>' "$message" "$MODEL")
[ -z "$SESSION" ] || message=$(printf '%s\nClaude-Session: %s' "$message" "$SESSION")
git commit -q -m "$message"
git log -1 --format='%h %s'
