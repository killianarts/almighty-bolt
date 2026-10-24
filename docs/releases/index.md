# Release notes

Each release has a page here, `X.Y.md`, written for people who *use* the
library. Patch releases (`X.Y.Z`) get a short page of their own listing fixes.
The page for the release in progress is [`dev.md`](dev.md).

## What belongs in release notes

Write about what a user can see, call or measure.

- **In:** new and changed public API, new behavior, fixes users could hit,
  supported platform changes, deprecations and removals.
- **In, with details:** an internal change that users will notice. Say what
  improved and by how much ("idle wakeups dropped from 60/s to 0",
  "retained trees use 40% less memory on the million-row list"), cite the
  benchmark result in `results/`, and leave out how it was done.
- **Out:** refactors, renamed internals, test and CI changes, dependency
  bumps with no user effect. These go in the [change log](../../CHANGELOG.md)
  only if at all, and in the commit history.

## Discussing API changes

Any added or changed public API gets its own entry that:

1. says what problem it solves, in a sentence or two;
2. shows a short, working code example (Common Lisp, and Coalton where the
   Coalton library has the feature);
3. links to the reference documentation for each symbol;
4. for a change, shows the old and new code side by side and says what to do.

Symbols are written in code formatting and linked, for example
[`make-window`](../reference/window.md#make-window). Reference pages do not
exist yet; write the link anyway so the notes do not need a second pass, and
check them when the documentation lands.

## Page structure

Follows [Django's release notes](https://docs.djangoproject.com/en/6.1/releases/6.1/).
Copy [`_template.md`](_template.md). In order:

1. Title, release date (or "Under development"), a short welcome paragraph
   and links to each major section.
2. **Highlights**: a few lines, used as the body of the GitHub release.
3. **Platform and compatibility**: supported OSes, GPUs, SBCL and Coalton
   versions.
4. **What's new**: one `###` section per major feature with prose, example
   and links, then **Minor features** with one `####` per area (for example
   Windowing, Rendering, Text, Layout, Input, the machine interface).
5. **Backwards incompatible changes**, by area.
6. **Features deprecated**: the item, its replacement, the release that
   removes it.
7. **Features removed**: items whose deprecation ended; point to the earlier
   notes for instructions.

## Writing process

- Whoever changes user-facing behavior adds the entry to `dev.md` in the
  same pull request. Reviewers check for it (see the pull request template).
- The [change log](../../CHANGELOG.md) gets a one-line entry for every
  user-facing change, linking to its section in the release notes.
- At release time, `dev.md` is renamed to `X.Y.md`, the date and highlights
  are filled in, a fresh `dev.md` is copied from the template, and the tag is
  pushed. See [`../releasing.md`](../releasing.md).
