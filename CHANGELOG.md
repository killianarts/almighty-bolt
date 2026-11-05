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

- `:turn :up` and `:turn :down` on `label`: text turned a quarter, reading upwards or downwards, as sharp as upright; `context-push-text` takes the turn too ([0.3](docs/releases/dev.md#a-font-for-every-element)).
- `load-font-file`: the fonts in a file, asked for by name while the app runs ([0.3](docs/releases/dev.md#a-font-for-every-element)).
- `:font` on every widget that draws text, and `typeface`: a family or a role of the theme, with any weight, a size, a tracking and a line height; a theme's `:body-font`, `:label-font`, `:heading-font`, `:code-font` and `:button-font` ([0.3](docs/releases/dev.md#a-font-for-every-element)).
- The inspector: while an app is developed, Command-Option-I opens a window beside it that says what a picked widget is and changes its value and its theme ([0.3](docs/releases/dev.md#the-inspector)).
- `open-window` and `close-window`: more than one window, each with an app of its own, from a button or the REPL ([0.3](docs/releases/dev.md#more-than-one-window)).
- Where a window opens: `:top`, `:left`, `:bottom` and `:right` from the screen's edges, or `:beside` another window, on `open-app`, `open-window` and `run-app`; with neither, windows cascade ([0.3](docs/releases/dev.md#more-than-one-window)).
- `design-surface`: a container whose contents are drawn but do not work, and a press on which returns the id of what it was on, for a designer ([0.3](docs/releases/dev.md#the-gui-designer-begun)).
- The start of a GUI designer, in `apps/gui-designer`: a view drawn with the real widgets and listed as an outline, to which widgets are added, whose ids and options are changed, and which is saved as a `define-view` file and opened again ([0.3](docs/releases/dev.md#the-gui-designer-begun)).
- `define-view` and `view`: a view made in the GUI designer, drawn by an app with its own code wired to it by the designer's ids, checked when the code is compiled ([0.3](docs/releases/dev.md#views-a-design-wired-to-code)).
- `drag-source`, `drop-target` and `dragging`: dragging within an app; widgets dragged into the GUI designer's view and within it ([0.3](docs/releases/dev.md#dragging)).
- `emacs/almighty-bolt.el` and the system `almighty-bolt/emacs`: the studio and an Emacs buffer in step through the image, over Sly: Edit Source and Edit Wiring in the inspector, point on a view's clause selecting its widget, and a view edited on either side shown on the other ([0.3](docs/releases/dev.md#the-studio-and-emacs)).
- The running app edited in the studio: a widget picked in it opens the view that drew it in the designer, live, and an edit there changes the running app as it is made ([0.3](docs/releases/dev.md#an-app-in-a-pane)).
- Open View in the GUI designer: a view the running Lisp has, opened by name and edited live, and saved to the file it came from ([0.3](docs/releases/dev.md#the-gui-designer-begun)).
- `show-pane`, `place-pane` and `pane-group`: an app moves a dock's panes ([0.3](docs/releases/dev.md#docked-panes)).
- `app-pane`: another app, working, in an area of this one, at a zoom, made again when its code is compiled, which VoiceOver and the machine interface see into; and `inspector-pane`, the inspector of it in the same window ([0.3](docs/releases/dev.md#an-app-in-a-pane)).
- `stage`: what is in it laid out at a size of its own, whatever the room, and cut off where the room ends; the GUI designer's canvas and the studio's App pane are on stages, sized by the person ([0.3](docs/releases/dev.md#an-app-in-a-pane)).
- `split` takes `:keep :second`, and a dock's layout keeps a split's second group with `:second`, for a sidebar on the right; `dock` takes `:min` ([0.3](docs/releases/dev.md#two-panes-and-a-divider)).
- `dock`: panes in groups of tabs, moved by dragging their tabs, by a tab's menu or with Control-Shift-Right and -Left, each keeping what it holds ([0.3](docs/releases/dev.md#docked-panes)).
- A `shortcut` in a dock's pane is heard only in the active pane ([0.3](docs/releases/dev.md#docked-panes)).
- A `split` given a new `:size` takes it up ([0.3](docs/releases/dev.md#docked-panes)).
- The studio, `apps/studio`: the parts of the GUI designer and of the theme designer as panes of one window ([0.3](docs/releases/dev.md#the-studio-begun)).
- `tree`: a tree of items that open and close; the GUI designer's outline is one ([0.3](docs/releases/dev.md#a-tree-view)).
- VoiceOver opens a `context-menu` with Show Menu (VO-Shift-M), and the semantic tree marks what has one `:menu t` ([0.3](docs/releases/dev.md#input)).
- `tabs`, and `choice` with `:style :tabs`: a tab bar ([0.3](docs/releases/dev.md#input)).
- `split`: two panes with a divider that is dragged, and in a view ([0.3](docs/releases/dev.md#two-panes-and-a-divider)).
- `app-menu`: an app's own menus in the menu bar, with shortcuts ([0.3](docs/releases/dev.md#the-apps-own-menus)).
- Every widget and container in the catalogue of views and of the GUI designer, and `define-widget-entry` for an app's own ([0.3](docs/releases/dev.md#every-widget-in-the-catalogue-and-yours)).
- A view inside another, `(:view id (:name other))`, and in the GUI designer the views defined, to put in the one designed ([0.3](docs/releases/dev.md#every-widget-in-the-catalogue-and-yours)).
- Undo and Redo in the theme designer ([0.3](docs/releases/dev.md#examples)).
- Undo and Redo in the GUI designer ([0.3](docs/releases/dev.md#the-gui-designer-begun)).
- `this-window`: in an app's function, the window it is in, to close it or open another beside it ([0.3](docs/releases/dev.md#more-than-one-window)).
- `:cursor` on `canvas`: the shape the pointer has over it ([0.3](docs/releases/dev.md#input)).
- `shortcut`, true in the frame a keyboard shortcut is pressed ([0.3](docs/releases/dev.md#input)).
- `:width`, `:height`, `:min-width`, `:min-height`, `:max-width` and `:max-height` on `column`, `row`, `stack`, `padding`, `panel` and `center` ([0.3](docs/releases/dev.md#layout)).
- `:border-color` and `:border-width` on `panel` and `pressable`, and `push-rounded-stroke` for a canvas ([0.3](docs/releases/dev.md#rendering)).
- `:weight :bold` and `:font` on `label` and `paragraph` ([0.3](docs/releases/dev.md#text)).
- `:style :segmented` on `choice`: joined segments, for switching between views ([0.3](docs/releases/dev.md#input)).
- A `:color` can be a colour of the theme: `:accent`, `(:accent :alpha 0.2)`, `(:mix :accent :background 0.8)`; and `theme-color` ([0.3](docs/releases/dev.md#rendering)).
- `pressable`, a container that is pressed as a button is, with the look of a button or a plain one, and a toggle's selected state ([0.3](docs/releases/dev.md#layout)).
- `grid`, children in columns and rows, with equal or shared column widths and rows of their own height, all the tallest's, or a given one ([0.3](docs/releases/dev.md#layout)).
- `spacer`, empty room that takes the free space of its row or column ([0.3](docs/releases/dev.md#layout)).
- `:within` on the machine interface's calls and the MCP server's tools: the container a thing is in, for an id used in more than one container ([0.3](docs/releases/dev.md#machine-interface)).
- The gallery shows how each widget and container is called, with its options and their defaults, and has a `grid`, a `pressable`, a `spacer` and a segmented `choice` ([0.3](docs/releases/dev.md#examples)).
- Spacing in a theme: `:spacing`, a scale that the paddings, gaps and heights of the controls are worked out from, and each of those sizes on its own; and a Spacing slider in the theme designer ([0.3](docs/releases/dev.md#spacing-in-a-theme)).

### Changed

- `context-push-text` takes an optional turn, so a method of it for a context of your own must take it too ([0.3](docs/releases/dev.md#context-push-text-takes-a-turn)).
- A window whose frame fails shows the last frame that did not, with the error over it, and goes on, instead of failing again on every event ([0.3](docs/releases/dev.md#windowing)).
- `make-session` takes `:app` where it took `:variant`, and `session-variant` is `session-app` ([0.3](docs/releases/dev.md#machine-interface)).
- The examples' make targets take `TASK=` alone, not `VARIANT=immediate` (`make 7guis TASK=counter`, `make image` writes `build/apps`), each task's app is in `app.lisp`, and their systems are `almighty-bolt-7guis/apps` and `almighty-bolt-stress/apps` ([0.3](docs/releases/dev.md#examples)).
- The canvas examples (the cells, the circles and the stress apps) use only the public `almighty-bolt` package, not `almighty-bolt/internals` ([0.3](docs/releases/dev.md#examples)).
- Quit (Command-Q) in a Lisp you work in closes every window, not only the one in front ([0.3](docs/releases/dev.md#more-than-one-window)).
- `context-font` takes an optional weight, any from 1 to 1000 or by name, and an optional tracking, so a method of it for a context of your own needs `&optional weight tracking` ([0.3](docs/releases/dev.md#backwards-incompatible-changes-in-xy)).
- The pointer highlights a row of a menu only when it moves onto it, so a pointer at rest no longer takes the highlight from the arrow keys ([0.3](docs/releases/dev.md#input)).
- A menu longer than the window shows the rows that fit and scrolls; `:max-rows` on `dropdown` and `menu-button`; letters typed in an open menu go to the row they begin ([0.3](docs/releases/dev.md#input)).
- A `list-box` scrolls to show the selected row on its first frame and when the app selects another ([0.3](docs/releases/dev.md#input)).

### Deprecated

### Removed

### Fixed

- A float whose anchor is another box, and nothing else in the frame changed, is laid out again: the designer's outline follows the selection ([0.3](docs/releases/dev.md#bug-fixes)).
- Text fields, combo boxes and text areas take what is typed through the system's input method: Japanese, Chinese and Korean, accents made with dead keys, and the Character Viewer ([0.3](docs/releases/dev.md#bug-fixes)).
- Two things at the top of an app, with ids of their own, are an error instead of the last alone being shown ([0.3](docs/releases/dev.md#bug-fixes)).
- Command-Z and Command-Shift-Z undo and redo in text fields, combo boxes and text areas ([0.3](docs/releases/dev.md#bug-fixes)).
- An app that is only a dialog, with nothing under it, shows the dialog instead of an empty window ([0.3](docs/releases/dev.md#bug-fixes)).
- The pointer is a text cursor over text that can be edited or selected ([0.3](docs/releases/dev.md#bug-fixes)).
- The menu bar has the Edit and Window menus and the application menu's Hide, Hide Others and Show All ([0.3](docs/releases/dev.md#bug-fixes)).
- A keyboard shortcut with Command and Option (`(shortcut #\i :command :option)`) is the key pressed, not what Option makes of it ([0.3](docs/releases/dev.md#bug-fixes)).
- `open-session` returns once the window has made its first frame, so a screenshot taken at once is of the window and not one pixel ([0.3](docs/releases/dev.md#bug-fixes)).
- A field's `:labelled-by` names the label nearest it, so two containers can each have a label and a field with the same ids ([0.3](docs/releases/dev.md#bug-fixes)).
- A panel whose `:color` is transparent draws nothing behind what is in it, instead of the theme's panel colour ([0.3](docs/releases/dev.md#bug-fixes)).
- A slider whose value the app rounds no longer warns that a frame did not settle while it is dragged ([0.3](docs/releases/dev.md#bug-fixes)).
- The wheel over a list, a scroll container or a text area that cannot move any further that way scrolls the container it is in ([0.3](docs/releases/dev.md#bug-fixes)).

### Security

## [0.2.0] - 2026-11-01

### Added

- VoiceOver reads every widget, can press buttons, check boxes, options and rows, move a slider and give a field the keyboard, and is told when they change ([0.2](docs/releases/0.2.md#voiceover-reads-the-screen)).
- `:live` on `label` and `paragraph`: a screen reader reads the text out whenever it changes, wherever its cursor is ([0.2](docs/releases/0.2.md#voiceover-reads-the-screen)).
- `scroll`, a container that shows what is inside it in a window moved by the wheel ([0.2](docs/releases/0.2.md#a-scroll-container)).
- `lazy-list`, a scrolling list that describes only the rows that show, so a million rows cost what the visible ones do ([0.2](docs/releases/0.2.md#a-list-that-builds-only-the-rows-that-show)).
- `text-area`, a text field of several lines whose long lines go on in the next ([0.2](docs/releases/0.2.md#a-text-area)).
- A gallery example with every widget and container on one page ([0.2](docs/releases/0.2.md#examples)).
- `switch`, `spinner`, and `:direction :vertical` on `choice` for radio buttons ([0.2](docs/releases/0.2.md#a-switch-radio-buttons-and-a-spinner)).
- `dropdown`, whose menu floats above the screen ([0.2](docs/releases/0.2.md#a-dropdown)).
- `menu-button`, and `context-menu`, a menu on a right-click or Control-click anywhere in an area ([0.2](docs/releases/0.2.md#a-menu-button-and-context-menus)).
- A press of the primary button with Control down is a secondary press, as on a Mac ([0.2](docs/releases/0.2.md#a-menu-button-and-context-menus)).
- `secondary-click` in the machine interface and `secondary_click` in the MCP server ([0.2](docs/releases/0.2.md#a-menu-button-and-context-menus)).
- `:tooltip` on every widget ([0.2](docs/releases/0.2.md#tooltips)).
- `popover` and `dialog`, a modal one that keeps the pointer and the keyboard ([0.2](docs/releases/0.2.md#popovers-and-dialogs)).
- `toast`, words at the bottom of the window for a while, read out by VoiceOver as they come ([0.2](docs/releases/0.2.md#toasts)).
- `combo-box`, a text field with a menu of suggestions ([0.2](docs/releases/0.2.md#a-combo-box)).
- `open-file-dialog` and `save-file-dialog`, the system's Open and Save panels ([0.2](docs/releases/0.2.md#the-systems-file-panels)).
- Themes: a light theme beside the dark one, the window following the system's setting; `define-theme`, `make-theme` and `with-theme`, for a window or a part of one ([0.2](docs/releases/0.2.md#themes)).
- A theme designer, `apps/theme-designer` ([0.2](docs/releases/0.2.md#themes)).

### Changed

- The words of a disabled button are dimmed ([0.2](docs/releases/0.2.md#themes)).
- `label` and `paragraph` with no `:size` are the theme's font size (15 in the themes that come with the library) ([0.2](docs/releases/0.2.md#themes)).
- The machine interface's `type-text` and `set-value` type a line feed as Return (in a text field, that submits it); `click`, `double-click` and `set-value` refuse a thing scrolled out of sight ([0.2](docs/releases/0.2.md#machine-interface)).

### Deprecated

### Removed

### Fixed

- Copy, cut and paste in a text field with Command-C, Command-X and Command-V, through the system's clipboard ([0.2](docs/releases/0.2.md#bug-fixes)).
- Text that is not edited can be selected with the pointer and copied, with `:selectable` on `label` and `paragraph` ([0.2](docs/releases/0.2.md#bug-fixes)).
- A double-click in a text field, a text area or a combo box selects a word, and a triple-click the field or the paragraph; a drag after them goes on by whole words or lines ([0.2](docs/releases/0.2.md#bug-fixes)).
- Text in many fonts or sizes no longer stops the window with "The glyph atlas is full": the atlas starts again, and the frame is drawn again ([0.2](docs/releases/0.2.md#bug-fixes)).

### Security

## [0.1.0] - 2026-10-31

### Added

- Windowing: `open-app`, `run-app` and `close-app` open a window from the REPL or as a program ([0.1](docs/releases/0.1.md#opening-a-window-from-the-repl)).
- Reload: compiling or saving changes the open window, with `state-let` state and the widgets' state kept ([0.1](docs/releases/0.1.md#changing-an-app-while-it-is-open)).
- Widgets: eleven widgets and six containers, each widget with a required `:id` ([0.1](docs/releases/0.1.md#widgets-and-ids)).
- Canvas: an area to draw in and handle input for ([0.1](docs/releases/0.1.md#a-canvas-for-drawing)).
- `state` reads a kept variable of the open app from the REPL.
- Agents and tests: a semantic tree of the screen, a machine interface to work an app by name (`almighty-bolt/machine`), and an opt-in MCP server for the open app (`almighty-bolt/mcp`) ([0.1](docs/releases/0.1.md#the-screen-as-a-tree-and-a-machine-interface)).

[Unreleased]: https://github.com/killianarts/almighty-bolt/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/killianarts/almighty-bolt/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/killianarts/almighty-bolt/releases/tag/v0.1.0
