# almighty-bolt

An Immediate Mode GUI library for Common Lisp. The library aims to make building
cross-platform GUIs as simple as possible without sacrificing performance or
memory efficiency. Building and understanding interfaces is far easier than in
other frameworks like McClim, Flutter, GPUI, React, etc.

**Status:** 0.1, the first release. macOS only (Apple silicon is what
it is tested on), SBCL only, one window, one fixed dark theme. Has an MCP
interface. Windows and Linux support are planned.

```lisp
(defpackage #:counter
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)))
(in-package #:counter)

(defun make-counter ()
  (bolt:state-let ((count 0))            ; state that survives a reload
    (lambda ()                           ; called every frame: describes the whole screen
      (bolt:padding (:all 24)
        (bolt:row (:gap 16 :cross-align :center)
          (bolt:label (princ-to-string count) :id :count :size 32)
          (when (bolt:button "Count" :id :count-button)   ; true on the frame it is clicked
            (incf count)))))))

(bolt:open-app 'make-counter :title "Counter" :width 360 :height 160)
```

`open-app` opens the window and gives you the REPL back. Change a label, press
C-c C-c on `make-counter`, and the window changes in about half a second with
the count where it was.

`bolt` is a local nickname, so none of the library's names are imported into
your package. In code they take the prefix (`bolt:row`); the table of widgets
below lists them bare.

## Features

- **Immediate mode.** Unlike retained mode (McClim, many non-Lisp UI frameworks)
  or reactive-hybrid mode (Flutter, GPUI, React) UIs, interfaces are simple
  function calls that rerender the UI wholesale. No complex state/signal
  management necessary.

  Unlike a normal immediate mode UI, layout and drawing are skipped when nothing
  affects sizes. Nothing happening in the window? No drawing. We love your CPU
  and respect your battery.
  
- **Hot-reloading.** Just because UI elements aren't individual objects with their
  own state doesn't mean you lose Lisp's typical hot-reloading. Compiling your
  app's UI function updates live, open apps.

- **Eleven widgets and six containers.** Label, paragraph, button, text field,
  slider, choice, check box, list box, progress bar, canvas and scrim; row,
  column, stack, padding, panel and center. More coming soon.

- **Canvas.** For drawing non-widget elements.

- **MCP Server.** For our LLM friends.
  
## Getting it

You need a Mac with a GPU that runs Metal, SBCL (2.6 is what this is developed
on) and `vend`, the tool that fetches the Lisp dependencies into `vendored/`.

```sh
git clone git@github.com:killianarts/almighty-bolt.git
cd almighty-bolt
make deps       # vend get
make test       # compile without warnings, then the integration and end-to-end tests
make smoke      # opens windows: the REPL check, the hot reload check, run-app
```

To use it from a Lisp, point ASDF at the repository (so that it finds
`vendored/` too) and load it:

```lisp
(require :asdf)
(asdf:initialize-source-registry
 '(:source-registry (:tree "/path/to/almighty-bolt/") :ignore-inherited-configuration))
(asdf:load-system "almighty-bolt-examples")   ; or "almighty-bolt" alone
```

[docs/getting-started.md](docs/getting-started.md) takes you from here to a
to-do list, with Emacs and Sly.

## Starting an app

An app is a function of no arguments, its **constructor**, that makes the
function that describes the screen. Give it by name, so that compiling it again
is what the window runs.

|                                                             |                                                                                                                                                                                                                                                      |
|-------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `(bolt:open-app 'make-x :title "X" :width 360 :height 160)` | Opens a window and gives the REPL back. Closing the window leaves Lisp running, and `open-app` can be called again.                                                                                                                                  |
| `(bolt:run-app 'make-x ...)`                                | For a program that is only a window (`sbcl --load app.lisp`): it runs on the main thread, and closing the window ends the process. `:hot t` remakes the app when code is compiled, `:watch-systems '("my-system")` also when a source file is saved. |
| `(bolt:close-app)`                                          | Closes the window that `open-app` opened.                                                                                                                                                                                                            |

## State

In almighty-bolt, widgets don't hold state--apps do. `bolt:state-let` is `let`
for the state of an app. Application state bound with `bolt:state-let` will
remain between frames. The caret and selection of text fields, the scroll of
lists and which widget has the keyboard are kept too, by widget id. 
Edit a variable's initial form and it starts over; close the window and open it
again to start the whole app over.

`(bolt:state 'name)` reads a kept variable of the open app from the REPL:
`(bolt:state 'counter::count)`.

## Widgets

Widgets are simple functions that can be composed into a GUI. Each takes a
required `:id`, a name that stays with the widget from frame to frame: it keeps
a text field's caret across frames and across a hot-reload/function compile, and
it is what tests and agents find the widget by. Leave it out, or use one twice
in a container, and the error names the widget. Pass a widget the value your app
holds, and it gives you the new one:

```lisp
(multiple-value-bind (text changed) (bolt:text-field name :id :name :width 160)
  (when changed (setf name text)))
```

| Call                                                                         | Returns                                                                     |
|------------------------------------------------------------------------------|-----------------------------------------------------------------------------|
| `(label text &key id size color flex)`                                       |                                                                             |
| `(paragraph text &key id size color flex)`                                   | text broken into lines to the width it is given                             |
| `(progress fraction &key id labelled-by width flex)`                         |                                                                             |
| `(button text &key id enabled flex)`                                         | true when clicked (the pointer, or Space or Return with the keyboard on it) |
| `(text-field string &key id labelled-by width invalid enabled flex)`         | the text now, whether it was changed this frame, whether Return was pressed |
| `(slider value minimum maximum &key id labelled-by width enabled flex)`      | the value now, whether it was changed                                       |
| `(choice options selected &key id labelled-by enabled flex)`                 | the index now, whether it was changed                                       |
| `(checkbox label checked &key id enabled flex)`                              | the state now, whether it was changed                                       |
| `(list-box items selected &key id labelled-by rows width enabled flex)`      | the index now, whether it was changed                                       |
| `(canvas width height painter &key id labelled-by focusable autofocus flex)` | the events for the canvas, oldest first                                     |
| `(scrim &key id)`                                                            | true when pressed: a translucent layer over what is before it in a `stack`  |

Containers take their options in a list and their children after it:
`(bolt:column (:gap 10 :cross-align :center) ...)`. They are `column`, `row`,
`stack`, `padding`, `panel` and `center`. A still app draws nothing and costs no
CPU; an animation asks for its next frame with `(bolt:ask-for-frame)`. The
options of each, `:flex` and `:labelled-by` are in
[docs/reference.md](docs/reference.md#widgets).

## Canvas

`bolt:canvas` is an area that you draw in and handle the pointer and keyboard
for. Give it a painter, which is called with the display list, the context and
the size, and it returns the events for the area; what the painter draws is
drawn once if it is the same as last frame. The 7GUIs Cells and Circles and the
editor in [`bench/stress/`](bench/stress) are canvases.
[The reference](docs/reference.md#canvas) has the drawing calls and the events.

## Working an app from a program or an agent

The screen is also a **semantic tree**: every widget says what it is, what it is
called, what it holds, its state and where it is. Two systems use the semantic tree.

**`almighty-bolt/machine`** is the Lisp side: a session on the open window that
does what a person's hands do, and returns when the app has answered. There are
no sleeps.

```lisp
(let ((session (almighty-bolt/machine/host-session:open-session)))   ; the window that open-app opened
  ;; m is a nickname for almighty-bolt/machine
  (m:see session)                                                    ; the tree
  (m:set-value session "Ada" :role :text-field :name "Name:")
  (m:click session :role :button :name "Create")
  (m:screenshot session))                                            ; a PNG, in a vector of bytes
```

**`almighty-bolt/mcp`** is an [MCP](https://modelcontextprotocol.io) server for
the open app, so that an agent can see it and use it. It does nothing until you
start it, and it listens on `127.0.0.1` only.

```lisp
(asdf:load-system "almighty-bolt/mcp")
(almighty-bolt/mcp:start-mcp-server :port 7071)
```

Anything on your machine that can connect to the port can work the app, so leave
it off in an app whose buttons you would not let any program press. The
[reference](docs/reference.md#the-machine-interface) has the calls, and the nine
tools and how to point an MCP client at the server
([MCP server](docs/reference.md#the-mcp-server)).

## Examples

- [`examples/`](examples): a counter and a to-do list, written with the public
  package only.
- [`examples/7guis/`](examples/7guis): the seven 7GUIs tasks.
  [`bench/stress/`](bench/stress) has a dashboard, a million-row list and a text
  editor with input-method support.

`make test` compiles the project and runs the integration and end-to-end tests,
with no window; `make smoke` opens windows. [`CLAUDE.md`](CLAUDE.md) says how
the project is worked on, and [`docs/releases/`](docs/releases) has the release
notes.

almighty-bolt is released under the [MIT license](LICENSE).
