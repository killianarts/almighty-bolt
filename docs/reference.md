# Reference

What the [README](../README.md) leaves out: the options of widgets and containers, the canvas, and the machine interface and MCP server in full. Calls that you would type are written with the `bolt:` nickname (see the README); names in running text are bare.

## Widgets

Every widget takes a required `:id` keyword: a name that stays with the widget from frame to frame. It is what keeps a field's caret across frames and across a reload, and what tests and replay scripts find a widget by. Leaving it out is an error on the first frame, `missing-id`, which names the widget. Two things inside one container with the same id are an error as well, `duplicate-id`, which names both and the container: the ui could not tell them apart (they would share the caret, the keyboard and what was drawn). Only widgets are required to have an id; containers keep an optional `:id`. In a loop over data the id is the item's own (`:id (task-id task)`), so that state follows the item when the list is re-sorted.

A text field takes the keys a Mac text field does: the arrows (with Shift to select and Option to move by a word), Home, End, Backspace, forward delete, Command-A to select everything, and Command-C, Command-X and Command-V to copy, cut and paste through the system's clipboard. A line break that is pasted into it becomes a space. A host made without a window (in a test or a replay) keeps what is copied to itself, and never touches the clipboard.

Every widget also takes `:flex`, its share of the free space of its container. A widget that holds something but has no words of its own (a text field, a slider, a choice, a list, a progress bar, a canvas) takes `:labelled-by`, the id of the label that names it, which is what an agent calls it by: `(bolt:text-field name :id :name :labelled-by :name-label)`.

## Containers

Containers take their options in a list and their children after it: `(bolt:column (:gap 10 :cross-align :center) ...)`. They are `column` and `row` (`:gap :main-align :cross-align :fill :flex`), `stack` (children on top of each other), `padding` (`:all :left :top :right :bottom`), `panel` (padding with a background: `:color :radius`), `center`, `scroll` and `lazy-list`.

`scroll` (`:id :height :flex`) shows what is inside it, one under another, in a window that the wheel or a trackpad moves up and down, with a bar at the right edge when not all of it shows. Its `:id` is required, since the place the window is at is kept by it. The window is `:height` points tall where nothing gives it a height (the window is as tall as what is inside if there is no `:height`), and `:flex` gives it a share of a column's free space instead. The wheel goes first to what is inside it (a `list-box` scrolls its own rows), and what is left moves the window while the pointer is over the container or anything in it. When Tab brings the keyboard to something out of the window, the window moves as little as shows all of it. What is out of the window cannot be pressed, and is `:hidden` in the semantic tree.

```lisp
(bolt:scroll (:id :log :height 300)
  (dolist (line lines)
    (bolt:label (line-text line) :id (line-id line))))
```

Moving the window lays the screen out again, so a wheel tick costs what a change of layout does: about 0.06 ms with 50 rows of a label and a button, and 2.8 ms with 2000 rows.

`lazy-list` (`:id :key :row-height :height :width :flex`) is a `scroll` for a long list, in which only the rows that show are described. It is written like `dolist`, and its body is one row, `:row-height` points tall (24 if not given):

```lisp
(bolt:lazy-list (task tasks :id :tasks :key #'task-id :row-height 28 :height 300)
  (bolt:row (:gap 8)
    (bolt:checkbox (task-text task) (task-done task) :id :done)))
```

The items are a vector (a list works, but it is walked from the start to the first row that shows). `:key` gives an item's id, which is the id of its row: what the ui keeps for a row (a caret, the keyboard) stays with its item when the items are reordered or one is put in above it. Without `:key` a row is known by its index. The window is `:height` points tall where nothing gives it a height (400 if there is no `:height`), and `:width` wide, or as wide as it is allowed to be. A wheel tick costs the same with a million rows as with a hundred, about 0.04 ms with rows of a label and a button. Tab reaches only the rows that are described. In the semantic tree it is a `:scroll-area` whose state also has `:row-count` and `:first-row`, and whose children are the rows that were described.

## Time

`(bolt:now)` is the time of the frame in nanoseconds, and `(bolt:ask-for-frame)` asks for another one: a still app draws nothing and costs no CPU, and an animation asks for the next frame itself.

## Canvas

A `canvas` is an area that you draw in and handle input for. `painter` is called with the display list, the context, and the width and height, and draws from the top left; what it draws is drawn once if it is the same as last frame. Draw with `push-rect`, `push-rounded-rect`, `push-clip` and `push-unclip`, colours made by `pack-color`, and text with `context-push-text` after `context-font` and `context-measure`. The events the canvas returns are `(:press x y button click-count)`, `(:move x y)`, `(:drag x y)`, `(:release x y inside-p)`, `(:key event)` (see `key-event-logical-key` and the `+key-...+` and `+modifier-...+` constants), `(:text string)`, `(:composition kind text selection-start selection-length)`, `(:wheel dx dy)` and `(:exit)`, with points in the canvas. `make-text-state` and `edit-text` are the text editing that the text field uses. The 7GUIs Cells and Circles and the stress apps' editor are canvases.

## The screen as a tree

Every widget says what it is (a `button`, a `text-field`, a `list`, a `checkbox`...), what it is called, what it holds, its state (focused, disabled, the caret, the selected item) and where it is, and the containers hold what is in them. The tree is made when it is asked for, from the frame that was last drawn, so a frame costs nothing for it. A node is a property list: `(:id :name :role :text-field :name "Name:" :value "Ada" :state (:focused t :hovered t :caret 3) :bounds (310.0 60.0 160.0 26.0))`.

### VoiceOver

VoiceOver reads the tree through the system's accessibility API. Each widget is an element of the window's view, with its name as the label (a static text says its words as its value), its value, whether it is enabled, and its place on the screen, which is worked out when it is asked for so that a window that was moved is where it says. The widget's `:id` is its accessibility identifier. The roles are:

| Widget | Role | Inside it |
|---|---|---|
| `label`, `paragraph` | `AXStaticText` | |
| `button` | `AXButton` | |
| `text-field` | `AXTextField` | |
| `checkbox` | `AXCheckBox` | |
| `slider` | `AXSlider` | its value, and its least and greatest |
| `progress` | `AXProgressIndicator` | its value, from 0 to 1 |
| `choice` | `AXRadioGroup` | an `AXRadioButton` for each option, valued 1 if it is the chosen one, with the identifier of the choice and the option's number after a dot |
| `list-box` | `AXList` | an `AXRow` for each row that is showing, the picked one selected, with the identifier of the list and the row's number after a dot |
| `canvas`, `scrim` | `AXGroup` | |

Containers that only lay things out (and `scroll`) are not elements, and what is in them is taken up to their parent. An element stays the same object while its widget is on the screen (it is found by the ids from the top, and by position for a container with no id; a row by its number in the list, so that it is the same when the list scrolls), so a screen reader's cursor stays on it.

A button, a check box, an option or a row can be pressed from VoiceOver; the press is a move, a press and a release in the middle of the thing, delivered to the window as a hand's would be, and a disabled one is not pressed. A slider can be moved up or down by a twentieth of its range, by pressing where the value then is. A text field can be given the keyboard, which a press on it does. The view says which element has the keyboard and which is under a point. Once something has asked what the window shows (VoiceOver, the Accessibility Inspector), or while the system says that VoiceOver or Switch Control is running, the system is told after each frame when a value changed, when elements came or went, when one moved or changed size (so that the cursor of a screen reader follows it), when the picked row of a list changed, and when the keyboard moved; before that nothing is worked out until the system asks, and a window at rest does no work. (The system's own flag for VoiceOver was found to say no inside an app while VoiceOver was running, so it is not what this goes by alone.) A list is read as the rows that are showing.

VoiceOver says a changed value only for the thing its cursor is on. A `label` or `paragraph` given `:live t` is read out, by announcement, whenever its text changes, wherever the cursor is: `(bolt:label (princ-to-string count) :id :count :live t)`. One that has just appeared is not read out, only one that changes; use it for a result, a count or a status, not for text that changes every frame.

Two other things use the tree, in different systems because they are for different people.

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

Things are named by `:id`, or by `:role` and `:name`, and a name that fits two things is an error that says so (`ambiguous-node`). The calls are `see`, `find-node`, `click`, `double-click`, `type-text`, `press-key`, `set-value`, `select-item`, `scroll`, `wait` and `screenshot`. The tests of the 7GUIs drive their apps through it. A thing scrolled out of the window of a `scroll` is `:hidden` in its state, with bounds of no size where it went out, and `click` and `double-click` refuse it with a message that says to scroll first; `scroll` on the container (its role is `:scroll-area`, and its state has `:offset` and `:content-height`) brings it into sight.

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
