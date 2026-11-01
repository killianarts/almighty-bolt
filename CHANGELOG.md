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

- VoiceOver reads every widget, can press buttons, check boxes, options and rows, move a slider and give a field the keyboard, and is told when they change ([0.2](docs/releases/dev.md#voiceover-reads-the-screen)).
- `:live` on `label` and `paragraph`: a screen reader reads the text out whenever it changes, wherever its cursor is ([0.2](docs/releases/dev.md#voiceover-reads-the-screen)).
- Copy, cut and paste in a text field with Command-C, Command-X and Command-V, through the system's clipboard ([0.2](docs/releases/dev.md#text-fields)).
- `scroll`, a container that shows what is inside it in a window moved by the wheel ([0.2](docs/releases/dev.md#a-scroll-container)).
- `lazy-list`, a scrolling list that describes only the rows that show, so a million rows cost what the visible ones do ([0.2](docs/releases/dev.md#a-list-that-builds-only-the-rows-that-show)).
- `text-area`, a text field of several lines whose long lines go on in the next ([0.2](docs/releases/dev.md#a-text-area)).
- A gallery example with every widget and container on one page ([0.2](docs/releases/dev.md#examples)).
- `switch`, `spinner`, and `:direction :vertical` on `choice` for radio buttons ([0.2](docs/releases/dev.md#a-switch-radio-buttons-and-a-spinner)).
- `dropdown`, whose menu floats above the screen ([0.2](docs/releases/dev.md#a-dropdown)).
- `menu-button`, and `context-menu`, a menu on a right-click or Control-click anywhere in an area ([0.2](docs/releases/dev.md#a-menu-button-and-context-menus)).
- A press of the primary button with Control down is a secondary press, as on a Mac ([0.2](docs/releases/dev.md#a-menu-button-and-context-menus)).
- `secondary-click` in the machine interface and `secondary_click` in the MCP server ([0.2](docs/releases/dev.md#a-menu-button-and-context-menus)).
- `:tooltip` on every widget ([0.2](docs/releases/dev.md#tooltips)).
- `popover` and `dialog`, a modal one that keeps the pointer and the keyboard ([0.2](docs/releases/dev.md#popovers-and-dialogs)).
- `toast`, words at the bottom of the window for a while, read out by VoiceOver as they come ([0.2](docs/releases/dev.md#toasts)).
- `combo-box`, a text field with a menu of suggestions ([0.2](docs/releases/dev.md#a-combo-box)).
- `open-file-dialog` and `save-file-dialog`, the system's Open and Save panels ([0.2](docs/releases/dev.md#the-systems-file-panels)).
- Themes: a light theme beside the dark one, the window following the system's setting; `define-theme`, `make-theme` and `with-theme`, for a window or a part of one ([0.2](docs/releases/dev.md#themes)).

### Changed

- The words of a disabled button are dimmed ([0.2](docs/releases/dev.md#themes)).
- `label` and `paragraph` with no `:size` are the theme's font size (15 in the themes that come with the library) ([0.2](docs/releases/dev.md#themes)).
- The machine interface's `type-text` and `set-value` type a line feed as Return (in a text field, that submits it); `click`, `double-click` and `set-value` refuse a thing scrolled out of sight ([0.2](docs/releases/dev.md#machine-interface)).

### Deprecated

### Removed

### Fixed

### Security

## [0.1.0] - 2026-10-31

### Added

- Windowing: `open-app`, `run-app` and `close-app` open a window from the REPL or as a program ([0.1](docs/releases/0.1.md#opening-a-window-from-the-repl)).
- Reload: compiling or saving changes the open window, with `state-let` state and the widgets' state kept ([0.1](docs/releases/0.1.md#changing-an-app-while-it-is-open)).
- Widgets: eleven widgets and six containers, each widget with a required `:id` ([0.1](docs/releases/0.1.md#widgets-and-ids)).
- Canvas: an area to draw in and handle input for ([0.1](docs/releases/0.1.md#a-canvas-for-drawing)).
- `state` reads a kept variable of the open app from the REPL.
- Agents and tests: a semantic tree of the screen, a machine interface to work an app by name (`almighty-bolt/machine`), and an opt-in MCP server for the open app (`almighty-bolt/mcp`) ([0.1](docs/releases/0.1.md#the-screen-as-a-tree-and-a-machine-interface)).

[Unreleased]: https://github.com/killianarts/almighty-bolt/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/killianarts/almighty-bolt/releases/tag/v0.1.0
