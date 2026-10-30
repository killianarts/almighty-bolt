#!/bin/sh
# Create or update the labels used by .github/release.yml and the pull request
# template. Safe to re-run. Needs the GitHub CLI, authenticated (gh auth login).
set -eu

repo="${1:-killianarts/almighty-bolt}"

label() {
  gh label create "$1" --repo "$repo" --color "$2" --description "$3" --force
}

label "area:platform"          "1d76db" "Windowing, event loop, clipboard, timers"
label "area:rendering"         "5319e7" "GPU renderer and display list"
label "area:text"              "0e8a16" "Text shaping, measurement, IME"
label "area:layout"            "fbca04" "Layout"
label "area:input"             "d93f0b" "Input, focus, hit testing, replay"
label "area:variants"          "006b75" "The immediate variant (the ui)"
label "area:machine-interface" "c2e0c6" "Accessibility tree and agent protocol"
label "breaking"               "b60205" "Backwards incompatible change"
label "performance"            "ff9f1c" "Speed or memory improvement"
label "docs"                   "0075ca" "Documentation"
label "internal"               "ededed" "Not user-facing; left out of release notes"
label "skip-changelog"         "ededed" "Left out of generated release notes"
