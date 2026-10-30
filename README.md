# almighty-bolt

A GUI library for Common Lisp. You describe the whole interface in a function that is called again for every frame; each widget says in its return value what the person did to it; the window is drawn on the GPU. It is built for working the way Lisp is worked in: you open a window from the REPL, edit the code, compile it, and the open window changes, with its state where it was.

**Status:** pre-release, working towards v0.1. macOS only (Apple silicon is what it is tested on), SBCL only, one window, one fixed dark theme. Not yet there: the accessibility tree and the machine interface (planned for v0.1), VoiceOver, Windows and Linux.

```lisp
(defpackage #:counter
  (:use #:cl #:almighty-bolt))
(in-package #:counter)

(defun make-counter ()
  (kept-let ((count 0))                  ; state that survives a reload
    (lambda ()                           ; called every frame: describes the whole screen
      (padding (:all 24)
        (row (:gap 16 :cross-align :center)
          (label (princ-to-string count) :id :count :size 32)
          (when (button "Count" :id :count-button)   ; true on the frame it is clicked
            (incf count)))))))

(open-app 'make-counter :title "Counter" :width 360 :height 160)
```

`open-app` opens the window and gives you the REPL back. Change a label, press C-c C-c on `make-counter`, and the window changes in about half a second with the count where it was.

## Getting it

You need a Mac with a GPU that runs Metal, SBCL (2.6 is what this is developed on) and `vend`, the tool that fetches the Lisp dependencies into `vendored/`.

```sh
git clone git@github.com:killianarts/almighty-bolt.git
cd almighty-bolt
make deps       # vend get
make test       # compile without warnings, then the integration and end-to-end tests
make smoke      # opens windows: the REPL check, the hot reload check, run-app
```

To use it from a Lisp, point ASDF at the repository (so that it finds `vendored/` too) and load it:

```lisp
(require :asdf)
(asdf:initialize-source-registry
 '(:source-registry (:tree "/path/to/almighty-bolt/") :ignore-inherited-configuration))
(asdf:load-system "almighty-bolt-examples")   ; or "almighty-bolt" alone
```

[docs/getting-started.md](docs/getting-started.md) takes you from here to a to-do list, with Emacs and Sly.

## Starting an app

An app is a function of no arguments, its **constructor**, that makes the function that describes the screen. Give it by name, so that compiling it again is what the window runs.

| | |
|---|---|
| `(open-app 'make-x :title "X" :width 360 :height 160)` | Opens a window and returns at once, with the REPL free. Closing the window (its button, Command-W, Command-Q) or `(close-app)` leaves Lisp running, and `open-app` can be called again. It needs the main thread to be waiting for something, as it is when an editor starts Lisp with a pipe for its input (Sly, SLIME). |
| `(run-app 'make-x ...)` | For a program that is only a window (`sbcl --load app.lisp`): it runs on the main thread and closing the window ends the process. `:hot t` remakes the app when code is compiled, `:watch-systems '("my-system")` also reloads when a source file is saved. |
| `(close-app)` | Closes the window that `open-app` opened. |

## State

`kept-let` is `let` for the state of an app. The window keeps its cells by the name of the variable, and when the app is made again (because code was compiled) a name that is in the store gets the cell it had, with its value. Only state goes in it: what is worked out from the state is made again from the new code. Edit the initial form of a variable and it starts over; to start the whole app over, close the window and open it again. The caret and selection of text fields, the scroll of lists and which widget has the keyboard are kept too, by widget id.

`(app-value 'name)` reads a kept variable of the open app from the REPL: `(app-value 'counter::count)`.

## Widgets

Every widget takes a required `:id` keyword: a name that stays with the widget from frame to frame. It is what keeps a field's caret across frames and across a reload, and what tests and replay scripts find a widget by. Leaving it out is an error on the first frame, `missing-id`, which names the widget. Two siblings must not share an id. Containers keep an optional `:id`. In a loop over data the id is the item's own (`:id (task-id task)`), so that state follows the item when the list is re-sorted. Every widget also takes `:flex`, its share of the free space of its container.

| Call | Returns |
|---|---|
| `(label text &key id size color flex)` | |
| `(paragraph text &key id size color flex)` | text broken into lines to the width it is given |
| `(progress fraction &key id width flex)` | |
| `(button text &key id enabled flex)` | true when clicked (the pointer, or Space or Return with the keyboard on it) |
| `(text-field string &key id width invalid enabled flex)` | the text now, whether it was changed this frame, whether Return was pressed |
| `(slider value minimum maximum &key id width enabled flex)` | the value now, whether it was changed |
| `(choice options selected &key id enabled flex)` | the index now, whether it was changed |
| `(checkbox label checked &key id enabled flex)` | the state now, whether it was changed |
| `(list-box items selected &key id rows width enabled flex)` | the index now, whether it was changed |
| `(canvas width height painter &key id focusable autofocus flex)` | the events for the canvas, oldest first (below) |
| `(scrim &key id)` | true when pressed: a translucent layer over what is before it in a `stack` |

A widget that shows a value is given the value the app holds, and returns the new one; if the app passes something else, that is what is shown:

```lisp
(multiple-value-bind (text changed) (text-field name :id :name :width 160)
  (when changed (setf name text)))
```

Containers take their options in a list and their children after it: `(column (:gap 10 :cross-align :center) ...)`. They are `column`, `row` (`:gap :main-align :cross-align :fill :flex`), `stack` (children on top of each other), `padding` (`:all :left :top :right :bottom`), `panel` (padding with a background: `:color :radius`) and `center`.

`(now)` is the time of the frame in nanoseconds, and `(ask-for-frame)` asks for another one: a still app draws nothing and costs no CPU, and an animation asks for the next frame itself.

### Canvas

A `canvas` is an area that you draw in and handle input for. `painter` is called with the display list, the context, and the width and height, and draws from the top left; what it draws is drawn once if it is the same as last frame. Draw with `push-rect`, `push-rounded-rect`, `push-clip` and `push-unclip`, colours made by `pack-color`, and text with `context-push-text` after `context-font` and `context-measure`. The events the canvas returns are `(:press x y button click-count)`, `(:move x y)`, `(:drag x y)`, `(:release x y inside-p)`, `(:key event)` (see `key-event-logical-key` and the `+key-...+` and `+modifier-...+` constants), `(:text string)`, `(:composition kind text selection-start selection-length)`, `(:wheel dx dy)` and `(:exit)`, with points in the canvas. `make-text-state` and `edit-text` are the text editing that the text field uses. The 7GUIs Cells and Circles and the stress apps' editor are canvases.

## Examples, tests, history

- [`examples/`](examples): a counter and a to-do list, written with the public package only.
- [`apps/7guis/`](apps/7guis) and [`apps/stress/`](apps/stress): the seven 7GUIs tasks, and a dashboard, a million-row list and a text editor with input-method support. They are the acceptance suite as well as examples.
- `make test` compiles the project into a directory that is thrown away and fails on any warning, then runs the integration and end-to-end tests (events and replay scripts in, display lists and frames out; the real operating system, text engine and GPU; no window). `make smoke` opens windows. Unit tests are not written for now; see [`CLAUDE.md`](CLAUDE.md).
- [`notes/devlog.org`](notes/devlog.org) is the lab notebook, and [`gui-framework-charter.org`](gui-framework-charter.org) the plan. The library began as an experiment comparing retained, reactive and immediate-mode approaches; the other approaches are archived on the tag `archive/three-variants`, and the immediate one was chosen for now.
- [`docs/releases/dev.md`](docs/releases/dev.md) has the release notes of the release in progress.

The systems are declared MIT in their `.asd` files.
