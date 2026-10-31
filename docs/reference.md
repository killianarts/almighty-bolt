# Reference

What the [README](../README.md) leaves out: the options of widgets and containers, the canvas, and the machine interface and MCP server in full. Calls that you would type are written with the `bolt:` nickname (see the README); names in running text are bare.

## Widgets

Every widget takes a required `:id` keyword: a name that stays with the widget from frame to frame. It is what keeps a field's caret across frames and across a reload, and what tests and replay scripts find a widget by. Leaving it out is an error on the first frame, `missing-id`, which names the widget. Two things inside one container with the same id are an error as well, `duplicate-id`, which names both and the container: the ui could not tell them apart (they would share the caret, the keyboard and what was drawn). Only widgets are required to have an id; containers keep an optional `:id`. In a loop over data the id is the item's own (`:id (task-id task)`), so that state follows the item when the list is re-sorted.

Every widget also takes `:flex`, its share of the free space of its container. A widget that holds something but has no words of its own (a text field, a slider, a choice, a list, a progress bar, a canvas) takes `:labelled-by`, the id of the label that names it, which is what an agent calls it by: `(bolt:text-field name :id :name :labelled-by :name-label)`.

## Containers

Containers take their options in a list and their children after it: `(bolt:column (:gap 10 :cross-align :center) ...)`. They are `column` and `row` (`:gap :main-align :cross-align :fill :flex`), `stack` (children on top of each other), `padding` (`:all :left :top :right :bottom`), `panel` (padding with a background: `:color :radius`) and `center`.

## Time

`(bolt:now)` is the time of the frame in nanoseconds, and `(bolt:ask-for-frame)` asks for another one: a still app draws nothing and costs no CPU, and an animation asks for the next frame itself.

## Canvas

A `canvas` is an area that you draw in and handle input for. `painter` is called with the display list, the context, and the width and height, and draws from the top left; what it draws is drawn once if it is the same as last frame. Draw with `push-rect`, `push-rounded-rect`, `push-clip` and `push-unclip`, colours made by `pack-color`, and text with `context-push-text` after `context-font` and `context-measure`. The events the canvas returns are `(:press x y button click-count)`, `(:move x y)`, `(:drag x y)`, `(:release x y inside-p)`, `(:key event)` (see `key-event-logical-key` and the `+key-...+` and `+modifier-...+` constants), `(:text string)`, `(:composition kind text selection-start selection-length)`, `(:wheel dx dy)` and `(:exit)`, with points in the canvas. `make-text-state` and `edit-text` are the text editing that the text field uses. The 7GUIs Cells and Circles and the stress apps' editor are canvases.

## The screen as a tree

Every widget says what it is (a `button`, a `text-field`, a `list`, a `checkbox`...), what it is called, what it holds, its state (focused, disabled, the caret, the selected item) and where it is, and the containers hold what is in them. The tree is made when it is asked for, from the frame that was last drawn, so a frame costs nothing for it. A node is a property list: `(:id :name :role :text-field :name "Name:" :value "Ada" :state (:focused t :hovered t :caret 3) :bounds (310.0 60.0 160.0 26.0))`.

Two things use it, in different systems because they are for different people.

## The machine interface

**`almighty-bolt/machine`** is the Lisp side, in the `almighty-bolt` system: a session on the open window that does what a person's hands do. Each action goes in as the events a person would make (a click is a move, a press and a release at the middle of the thing), and returns when the app has answered, which the window says itself when it has nothing more to draw; there are no sleeps.

```lisp
(let ((session (almighty-bolt/machine/host-session:open-session)))   ; the window that open-app opened
  ;; m is a nickname for almighty-bolt/machine
  (m:see session)                                                    ; the tree
  (m:set-value session "Ada" :role :text-field :name "Name:")
  (m:click session :role :button :name "Create")
  (m:screenshot session))                                            ; a PNG, in a vector of bytes
```

Things are named by `:id`, or by `:role` and `:name`, and a name that fits two things is an error that says so (`ambiguous-node`). The calls are `see`, `find-node`, `click`, `double-click`, `type-text`, `press-key`, `set-value`, `select-item`, `scroll`, `wait` and `screenshot`. The tests of the 7GUIs drive their apps through it.

## The MCP server

**`almighty-bolt/mcp`** is an [MCP](https://modelcontextprotocol.io) server for the open app, so that an agent can see it and use it. It is a separate system (it needs `com.inuoe.jzon`, which `make deps` fetches), it does nothing until you call it, and it listens on `127.0.0.1` only.

```lisp
(asdf:load-system "almighty-bolt/mcp")
(almighty-bolt/mcp:start-mcp-server :port 7071)
;; MCP server on 127.0.0.1:7071. For an MCP client that wants a pipe: command nc, arguments 127.0.0.1 7071
```

The server speaks MCP's newline-delimited JSON-RPC on that socket. A client that starts its server as a program over a pipe, as most do, can use `nc` as the pipe:

```json
{"mcpServers": {"my-app": {"command": "nc", "args": ["127.0.0.1", "7071"]}}}
```

The agent gets nine tools: `snapshot` (the screen as text, one line for each thing with its role, name, id, value, state and place), `click`, `double_click`, `type_text`, `press_key`, `set_value`, `scroll`, `wait_idle` and `screenshot` (an image). Every action answers with the screen as it is afterwards, so an agent needs one call to act and see. What cannot be done (a thing that is not there, a disabled button, two things of the same name) comes back as an error result that says why, and the agent goes on. This is what the agent reads of the counter:

```
window @0,0,222,80
  static-text "34" id=count @24,24,36,32
  button "Count" id=count-button @130,26,68,29
```

`(almighty-bolt/mcp:stop-mcp-server)` closes it and every connection. Anything on this machine that can connect to the port can work the app, which is why it is local and has to be started by a call: leave it off in an app whose buttons you would not let any program press.
