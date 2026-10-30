# Change log

All notable user-facing changes. Format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and versions follow
[Semantic Versioning](https://semver.org/) once 1.0 is released. Before 1.0,
minor releases may break the API; every break is listed under "Changed" and
explained in the release notes.

Each entry is one line and links to its section in the
[release notes](docs/releases/index.md) when there is one. Internal changes
are not listed.

## [Unreleased]

### Added

- Windowing: `open-app`, `run-app` and `close-app` open a window from the REPL or as a program ([0.1](docs/releases/dev.md#opening-a-window-from-the-repl)).
- Reload: compiling or saving changes the open window, with `kept-let` state and the widgets' state kept ([0.1](docs/releases/dev.md#changing-an-app-while-it-is-open)).
- Widgets: eleven widgets and six containers, each widget with a required `:id` ([0.1](docs/releases/dev.md#widgets-and-ids)).
- Canvas: an area to draw in and handle input for ([0.1](docs/releases/dev.md#a-canvas-for-drawing)).
- `app-value` reads a kept variable of the open app from the REPL.
- Agents and tests: a semantic tree of the screen, a machine interface to work an app by name (`almighty-bolt/machine`), and an opt-in MCP server for the open app (`almighty-bolt/mcp`) ([0.1](docs/releases/dev.md#the-screen-as-a-tree-and-a-machine-interface)).

### Changed

### Deprecated

### Removed

### Fixed

- Text: the letters of a line stand on one baseline. Some had been set up to a pixel higher or lower than their neighbours.

### Security
