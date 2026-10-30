# almighty-bolt 0.1 release notes

*Under development*

Welcome to almighty-bolt 0.1, the first release: a GUI library for Common Lisp that you work on from the REPL. You open a window, edit the code, compile it, and the open window changes with its state where it was. See [what's new](#whats-new).

<!-- highlights:start -->
## Highlights

- Windowing: `open-app` opens a window from the REPL and gives the REPL back; no server is started.
- Reload: compiling a definition (or saving a file) changes the open window in about half a second, and `kept-let` state, the caret, the scroll and the keyboard stay where they were.
- Widgets: label, paragraph, button, text field, slider, choice, check box, list box, progress, canvas, scrim; row, column, stack, padding, panel, center.
- Efficiency: a still window draws nothing and wakes nothing (0 frames and 0 wakeups a second at rest); in the shipped replay scripts a frame allocates nothing after the first (counter, cells, dashboard) or between about 0.8 and 3 KB (timer, editor, rows). The numbers are in `results/`.
- Text: shaped and rendered by Core Text, with input methods (Japanese input is used in the editor stress app).
<!-- highlights:end -->

## Platform and compatibility

- macOS on Apple silicon (the only platform tested), with Metal.
- SBCL 2.6 (developed on 2.6.3). Other Lisps are not supported.
- One window. One fixed dark theme.
- Not in this release: the accessibility tree and the machine interface (planned for 0.1 before it ships), VoiceOver, Windows and Linux, a Coalton library.

## What's new

### Opening a window from the REPL

`open-app` opens a window and returns at once, so the REPL stays yours. It is for an editor that starts Lisp itself with a pipe for its input (Sly, SLIME); the window's event loop then runs on the main thread, which is waiting for the editor.

```lisp
(almighty-bolt:open-app 'make-counter :title "Counter" :width 360 :height 160)
```

Closing the window, Command-W, Command-Q or `(almighty-bolt:close-app)` leave Lisp running, and `open-app` can be called again. `run-app` is for a program that is only a window: it runs on the main thread and closing the window ends the process. See [`open-app`, `run-app` and `close-app`](../../README.md#starting-an-app) and the [getting started guide](../getting-started.md).

### Changing an app while it is open

When code is compiled into the image, from the editor or by loading a file, the window calls the app's constructor again and swaps in what it makes. A new app that fails in its first frame is taken out and the old one put back, with the error printed. `run-app` takes `:watch-systems` to also load systems again when their files are saved.

State is declared with `kept-let`, which is `let` over cells that the window keeps by the name of the variable:

```lisp
(defun make-counter ()
  (kept-let ((count 0))
    (lambda () ...)))
```

What the person did to the widgets is kept too (the caret and selection of text fields, the scroll of lists, the keyboard focus). Edit a variable's initial form and it starts over. See [State](../../README.md#state).

### Widgets and ids

Eleven widgets and six containers. Every widget takes a required `:id`, a name that stays with it from frame to frame; leaving it out is a `missing-id` error that names the widget. See [Widgets](../../README.md#widgets).

### A canvas for drawing

`canvas` is an area that an app draws in and handles the pointer and keyboard for; the 7GUIs Circles and Cells, a dashboard of animated tiles, a list of a million rows and a text editor with input-method support are all written with it. See [Canvas](../../README.md#canvas).

### Minor features

#### Windowing

- `app-value` reads a kept variable of the open app from the REPL.
- A window that is still draws nothing: zero frames and zero wakeups a second at rest, measured with `make bench-app`.

#### Developing

- The library compiles without a warning, and `make test` fails if it stops doing so.
- `make test` runs the integration and end-to-end tests; `make smoke` runs the checks that open windows.

## Backwards incompatible changes

None: this is the first release.

## Features deprecated

None.

## Features removed

None.
