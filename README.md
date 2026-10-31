# almighty-bolt

An Immediate Mode GUI library for Common Lisp. The library aims to make building
cross-platform GUIs as simple as possible without sacrificing performance or
memory efficiency. almighty-bolt caches the UI and only rerenders it when there
is some event on an element in the UI, such as a mouse hover, keyboard caret
ticking animation, etc. That means that almighty-bolt also doesn't sap your
CPU/battery dry.

It supports hot-reloading via function compilation and also has a "watch mode" so
you can update GUIs by just saving the file you're working on.

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

- **Cached Immediate mode.** Unlike retained mode (McClim, many non-Lisp UI frameworks)
  or reactive-hybrid mode (Flutter, GPUI, React) UIs, interfaces are simply
  rerendered wholesale. You don't need complex mechanisms for managing state and
  complex UI changes--you just rerender on some event.
  
  Unlike a normal immediate mode UI, the element tree is cached and morphed,
  meaning the changes that are rendered to the screen are only done on some
  event and only on elements effected by the event--saving CPU and battery power.

- **Hot-reloading.** Just because UI elements are individual objects with their
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

|                                                             |                                                                                                                                                                                                                                                             |
|-------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `(bolt:open-app 'make-x :title "X" :width 360 :height 160)` | Opens a window and returns at once, with the REPL free. Closing the window (its button, Command-W, Command-Q) or `(close-app)` leaves Lisp running, and `open-app` can be called again.                                                                     |
| `(bolt:run-app 'make-x ...)`                                | For a program that is only a window (`sbcl --load app.lisp`): it runs on the main thread and closing the window ends the process. `:hot t` remakes the app when code is compiled, `:watch-systems '("my-system")` also reloads when a source file is saved. |
| `(bolt:close-app)`                                          | Closes the window that `open-app` opened.                                                                                                                                                                                                                   |

## State

`state-let` is `let` for the state of an app. The window keeps its cells by the
name of the variable, and when the app is made again (because code was compiled)
a name that is in the store gets the cell it had, with its value. Only state
goes in it: what is worked out from the state is made again from the new code.
Edit the initial form of a variable and it starts over; to start the whole app
over, close the window and open it again. The caret and selection of text
fields, the scroll of lists and which widget has the keyboard are kept too, by
widget id.

`(bolt:state 'name)` reads a kept variable of the open app from the REPL:
`(bolt:state 'counter::count)`.

## Working an app from a program or an agent

The screen is also a **semantic tree**: every widget says what it is (a
`button`, a `text-field`, a `list`, a `checkbox`...), what it is called, what it
holds, its state (focused, disabled, the caret, the selected item) and where it
is, and the containers hold what is in them. It is made when it is asked for,
from the frame that was last drawn, so a frame costs nothing for it; a screen
reader will read the same tree (the VoiceOver bridge is the first thing after
0.1). A node is a property list: `(:id :name :role :text-field :name "Name:"
:value "Ada" :state (:focused t :hovered t :caret 3) :bounds (310.0 60.0 160.0
26.0))`.

Two things use it, in different systems because they are for different people.

**`almighty-bolt/machine`** is the Lisp side, in the `almighty-bolt` system: a
session on the open window that does what a person's hands do. Each action goes
in as the events a person would make (a click is a move, a press and a release
at the middle of the thing), and returns when the app has answered, which the
window says itself when it has nothing more to draw; there are no sleeps.

```lisp
(let ((session (almighty-bolt/machine/host-session:open-session)))   ; the window that open-app opened
  ;; m is a nickname for almighty-bolt/machine
  (m:see session)                                                    ; the tree
  (m:set-value session "Ada" :role :text-field :name "Name:")
  (m:click session :role :button :name "Create")
  (m:screenshot session))                                            ; a PNG, in a vector of bytes
```

Things are named by `:id`, or by `:role` and `:name`, and a name that fits two
things is an error that says so (`ambiguous-node`). The calls are `see`,
`find-node`, `click`, `double-click`, `type-text`, `press-key`, `set-value`,
`select-item`, `scroll`, `wait` and `screenshot`. The tests of the 7GUIs drive
their apps through it.

**`almighty-bolt/mcp`** is an [MCP](https://modelcontextprotocol.io) server for
the open app, so that an agent can see it and use it. It is a separate system
(it needs `com.inuoe.jzon`, which `make deps` fetches), it does nothing until
you call it, and it listens on `127.0.0.1` only.

```lisp
(asdf:load-system "almighty-bolt/mcp")
(almighty-bolt/mcp:start-mcp-server :port 7071)
;; MCP server on 127.0.0.1:7071. For an MCP client that wants a pipe: command nc, arguments 127.0.0.1 7071
```

The server speaks MCP's newline-delimited JSON-RPC on that socket. A client that
starts its server as a program over a pipe, as most do, can use `nc` as the
pipe:

```json
{"mcpServers": {"my-app": {"command": "nc", "args": ["127.0.0.1", "7071"]}}}
```

The agent gets nine tools: `snapshot` (the screen as text, one line for each
thing with its role, name, id, value, state and place), `click`, `double_click`,
`type_text`, `press_key`, `set_value`, `scroll`, `wait_idle` and `screenshot`
(an image). Every action answers with the screen as it is afterwards, so an
agent needs one call to act and see. What cannot be done (a thing that is not
there, a disabled button, two things of the same name) comes back as an error
result that says why, and the agent goes on. This is what the agent reads of the
counter:

```
window @0,0,222,80
  static-text "34" id=count @24,24,36,32
  button "Count" id=count-button @130,26,68,29
```

`(almighty-bolt/mcp:stop-mcp-server)` closes it and every connection. Anything
on this machine that can connect to the port can work the app, which is why it
is local and has to be started by a call: leave it off in an app whose buttons
you would not let any program on your machine press.

## Widgets

Every widget takes a required `:id` keyword: a name that stays with the widget
from frame to frame. It is what keeps a field's caret across frames and across a
reload, and what tests and replay scripts find a widget by. Leaving it out is an
error on the first frame, `missing-id`, which names the widget. Two things
inside one container with the same id are an error as well, `duplicate-id`,
which names both and the container: the ui could not tell them apart (they would
share the caret, the keyboard and what was drawn). Only widgets are required to
have an id; containers keep an optional `:id`. In a loop over data the id is the
item's own (`:id (task-id task)`), so that state follows the item when the list
is re-sorted. Every widget also takes `:flex`, its share of the free space of
its container. A widget that holds something but has no words of its own (a text
field, a slider, a choice, a list, a progress bar, a canvas) takes
`:labelled-by`, the id of the label that names it, which is what a screen reader
or an agent calls it by: `(bolt:text-field name :id :name :labelled-by :name-label)`.

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
| `(canvas width height painter &key id labelled-by focusable autofocus flex)` | the events for the canvas, oldest first (below)                             |
| `(scrim &key id)`                                                            | true when pressed: a translucent layer over what is before it in a `stack`  |

A widget that shows a value is given the value the app holds, and returns the
new one; if the app passes something else, that is what is shown:

```lisp
(multiple-value-bind (text changed) (bolt:text-field name :id :name :width 160)
  (when changed (setf name text)))
```

Containers take their options in a list and their children after it: `(column
(:gap 10 :cross-align :center) ...)`. They are `column`, `row` (`:gap
:main-align :cross-align :fill :flex`), `stack` (children on top of each other),
`padding` (`:all :left :top :right :bottom`), `panel` (padding with a
background: `:color :radius`) and `center`.

`(bolt:now)` is the time of the frame in nanoseconds, and `(bolt:ask-for-frame)` asks for
another one: a still app draws nothing and costs no CPU, and an animation asks
for the next frame itself.

### Canvas

A `canvas` is an area that you draw in and handle input for. `painter` is called
with the display list, the context, and the width and height, and draws from the
top left; what it draws is drawn once if it is the same as last frame. Draw with
`push-rect`, `push-rounded-rect`, `push-clip` and `push-unclip`, colours made by
`pack-color`, and text with `context-push-text` after `context-font` and
`context-measure`. The events the canvas returns are `(:press x y button
click-count)`, `(:move x y)`, `(:drag x y)`, `(:release x y inside-p)`, `(:key
event)` (see `key-event-logical-key` and the `+key-...+` and `+modifier-...+`
constants), `(:text string)`, `(:composition kind text selection-start
selection-length)`, `(:wheel dx dy)` and `(:exit)`, with points in the canvas.
`make-text-state` and `edit-text` are the text editing that the text field uses.
The 7GUIs Cells and Circles and the stress apps' editor are canvases.

## Examples, tests, history

- [`examples/`](examples): a counter and a to-do list, written with the public
  package only.
- [`examples/7guis/`](examples/7guis): the seven 7GUIs tasks. They are the
  acceptance suite as well as examples. [`bench/stress/`](bench/stress) has a
  dashboard, a million-row list and a text editor with input-method support, for
  the benchmarks and the tests.
- `make test` compiles the project into a directory that is thrown away and
  fails on any warning, then runs the integration and end-to-end tests (events
  and replay scripts in, display lists and frames out; the real operating
  system, text engine and GPU; no window). `make smoke` opens windows, and in
  one of them a client of the MCP server presses the counter's button, takes a
  screenshot that macOS then decodes, and does the same through `nc`. Unit tests
  are not written for now; see [`CLAUDE.md`](CLAUDE.md).
- The library began as an experiment comparing retained, reactive and
  immediate-mode approaches. The immediate one was chosen for now; the others are
  archived on the tag `archive/three-variants`.
- [`docs/releases/`](docs/releases) has the release notes; [0.1](docs/releases/0.1.md)
  is the first.

almighty-bolt is released under the [MIT license](LICENSE).
