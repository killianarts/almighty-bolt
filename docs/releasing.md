# Releasing

1. Check that every user-facing change since the last tag has an entry in
   `docs/releases/dev.md` and one in `CHANGELOG.md`. Compare against merged
   pull requests without the `internal` label.
2. `git mv docs/releases/dev.md docs/releases/X.Y.md`. Fill in the date and
   the Highlights (three to six lines, grouped by area; they become the top of
   the GitHub release). Check every reference link.
3. Copy `docs/releases/_template.md` to a new `docs/releases/dev.md`.
4. In `CHANGELOG.md`, rename `[Unreleased]` to `[X.Y.Z] - YYYY-MM-DD`, add a
   fresh `[Unreleased]` section above it and update the compare links.
5. Commit, then tag and push: `git tag vX.Y.Z && git push origin vX.Y.Z`.
6. The Release workflow publishes the GitHub release: Highlights, a link to
   the full notes and the change log, then GitHub's generated list of pull
   requests grouped by `area:*` label (`.github/release.yml`). Tags with a
   hyphen (`v0.2.0-rc1`) are marked as pre-releases.
7. Patch releases (`X.Y.Z`) reuse `X.Y.md`: add a "Fixes in X.Y.Z" section
   with the date and keep the Highlights to the fixes.
