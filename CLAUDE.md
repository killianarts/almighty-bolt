# almighty-bolt

A GUI library for Common Lisp, on macOS first (AppKit and Metal, called straight from SBCL with CFFI). It is immediate mode: the interface is a function that is called again every frame. The charter, `gui-framework-charter.org`, is the long document; `notes/devlog.org` is the lab notebook.

## How the project is run
- **No decision records.** A decision goes in the devlog entry of the day, with its evidence, and in the commit message. `notes/decisions/` (0001 to 0031) is frozen history: do not edit or add.
- **Devlog.** One dated entry per working session in `notes/devlog.org`: what was tried, measured, failed and learned.
- **Tests are integration and end-to-end only, for now.** No new unit tests. A property that matters is checked from outside (over a replay script, in a window). Mutation-check every new or changed test with a deliberate defect. `make test` is the headless suite (real OS, text engine and GPU, no window); `make smoke` opens windows and needs a logged-in Mac.
- **Commits.** Use `scripts/auto-commit.sh`, and only when `make test` passes. Trailers: `Assumes:` (ids from `notes/assumptions.org`) and `Tests:`. Do not push without Micah's say-so, and ask again for each new push.
- **User-facing changes** go in `CHANGELOG.md` and `docs/releases/dev.md` (the notes of the release in progress, written for people who use the library) in the same commit. Until v0.1 ships, `dev.md` describes the library as a whole, and is kept true; after it, a change adds its entry.

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
- Hot reload and the REPL entry (`open-app`, `kept-let`) are described in the devlog, 2026-10-28 and 2026-10-29. Under Sly the main thread is waiting on stdin, which `open-app` relies on.
