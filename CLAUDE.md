# almighty-bolt

A GUI library for Common Lisp, on macOS first (AppKit and Metal, called straight from SBCL with CFFI). It is immediate mode: the interface is a function that is called again every frame. [`README.md`](README.md) says what it is and how to use it, and [`docs/getting-started.md`](docs/getting-started.md) takes a new user through it.

## How the project is run
- **Decisions go in commit messages.** What was decided and why, with the evidence (a measurement, a failing case), is the body of the commit that makes the change. There is no devlog, no decision record and no assumptions register. The early experiment (three designs compared: the charter, the devlog, 31 decision records and the assumptions register) is on the tag `archive/experiment-notes`, and the designs that were not chosen are on `archive/three-variants` and `archive/mcclim`.
- **Tests are integration and end-to-end only, for now.** No new unit tests. A property that matters is checked from outside (over a replay script, in a window). Mutation-check every new or changed test with a deliberate defect. `make test` is the headless suite (real OS, text engine and GPU, no window); `make smoke` opens windows and needs a logged-in Mac.
- **Measurements** are written by the `make bench-*` targets to `results/` (see `results/README.org`). A record is never edited, only superseded.
- **Commits.** Use `scripts/auto-commit.sh`, and only when `make test` passes; its `Tests:` trailer says what was run. Do not push without Micah's say-so, and ask again for each new push.
- **User-facing changes** go in `CHANGELOG.md` and `docs/releases/dev.md` (the notes of the release in progress, written for people who use the library) in the same commit. v0.1 is out (`docs/releases/0.1.md`), so a change adds its entry to `dev.md` and to the `[Unreleased]` section of the change log. `docs/releasing.md` says how a release is made.

## Code
- One package per file, named after the project and the file (tests under `t/`). `.asd` files and `vendored/` are the exceptions.
- Classic ASDF: `defsystem` with `:serial t` and an explicit `:components` list in dependency order. Never `package-inferred-system`.
- Write `loop` clause words as keywords (`:for`, `:in`, `:do`).
- Each file imports exactly what it uses (`:import-from` lists) and exports its own API.

## Working here
- ASDF looks only in this tree (see the `REGISTRY` in the `Makefile`), never in `~/common-lisp/`.
- `make deps` fetches `vendored/` with `vend`. Never delete through a symlink into `vendored/`.
- Run one SBCL job at a time. macOS tools are BSD (`sed -i ''`).
- ASDF takes a compiled file for current unless the source is newer by a whole second, so a source restored from a copy can be loaded as its own mutant. Delete the compiled files (through `asdf:output-files`) before and after a mutation run.
- Hot reload and the REPL entry: `open-app` and `state-let` are described in the README. SBCL calls a hook whenever a function is defined (`src/redefinition.lisp`); the host then makes the app again on the main thread (`reload-host` in `src/platform/macos/host.lisp`) and the state kept by `state-let` (`src/kept.lisp`) is handed to the new one. Under Sly the main thread is waiting on stdin, which `open-app` relies on, so the REPL has to be SBCL (`C-u M-x sly` asks which Lisp; the `.asd` stops in any other).
