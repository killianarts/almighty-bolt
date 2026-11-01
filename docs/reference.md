# Reference

What the [README](../README.md) leaves out: the options of widgets and containers, the canvas, and the machine interface and MCP server in full. Calls that you would type are written with the `bolt:` nickname (see the README); names in running text are bare.

## Widgets

Every widget takes a required `:id` keyword: a name that stays with the widget from frame to frame. It is what keeps a field's caret across frames and across a reload, and what tests and replay scripts find a widget by. Leaving it out is an error on the first frame, `missing-id`, which names the widget. Two things inside one container with the same id are an error as well, `duplicate-id`, which names both and the container: the ui could not tell them apart (they would share the caret, the keyboard and what was drawn). Only widgets are required to have an id; containers keep an optional `:id`. In a loop over data the id is the item's own (`:id (task-id task)`), so that state follows the item when the list is re-sorted.

A text field takes the keys a Mac text field does: the arrows (with Shift to select and Option to move by a word), Home, End, Backspace, forward delete, Command-A to select everything, and Command-C, Command-X and Command-V to copy, cut and paste through the system's clipboard. A line break that is pasted into it becomes a space. The pointer selects as in a Mac text field: a press puts the caret, a drag selects, a double-click selects a word and a triple-click all of the field, and a drag after a double- or triple-click goes on by whole words or lines. A host made without a window (in a test or a replay) keeps what is copied to itself, and never touches the clipboard.

A `choice` puts its options side by side; with `:direction :vertical` it puts them one under another, as radio buttons are on a Mac. A `switch` is a check box that looks like a light switch, for a setting that takes effect at once. A `spinner` (`:size`, 18 points) is a ring of dots that goes round, for work whose end is not known; it asks for a frame about ten times a second while it is shown, so show it only while the work goes on.

A `dropdown` (a pop-up button) shows the option that is chosen; a press, or the space bar, Return or an arrow with the keyboard on it, opens a menu of all of them, with the chosen one marked. The menu floats above the screen, under the dropdown (or over it, if there is no room under it). A row of it chooses; Up and Down move the highlight and Return chooses it; Escape, or a press anywhere outside the menu, closes it, and that press goes nowhere else. While the menu is open the wheel scrolls nothing under it. `:width` makes the dropdown as wide as that; by default it is as wide as what it shows.

A `menu-button` is a button that opens a menu of commands, of which none is marked; it returns the one chosen, on the frame it is chosen. Its menu works as a dropdown's does.

A `context-menu` (`:id :items`) is a container: what is in it is laid out as a column, and a press of the secondary button anywhere on it (or of the primary button with Control down, as a Mac takes it) opens a menu of `:items` where the pointer is. It returns the item chosen, on the frame it is chosen. A context menu inside another's area takes the press first.

```lisp
(let ((command (bolt:context-menu (:id :row-menu :items '("Rename" "Delete"))
                 (bolt:label (task-text task) :id :text))))
  (when (equal command "Delete") (remove-task task)))
```

A `popover` (`:id :open :anchor :placement`) floats, while the app says it is `:open`, below (or, with `:placement :right`, beside) the widget whose id is `:anchor`, which must be described before it in the same container. What its body describes is inside it, one under another, and works as anywhere else. A press outside it, or Escape, dismisses it: it returns true on that frame, and the press goes nowhere else. Several can be open at once.

A `dialog` (`:id :open :title`) is modal: while `:open`, it is in the middle of the window over a scrim, with `:title` over what its body describes. The pointer reaches nothing under it, and the keyboard stays in it: it goes to the first thing in it that can have it, and Tab goes round inside it. Escape dismisses it (it returns true on that frame); its buttons say what they do as buttons do.

```lisp
(when (bolt:dialog (:id :confirm :open asking :title "Delete the file?")
        (bolt:row (:gap 8)
          (when (bolt:button "Cancel" :id :cancel) (setf asking nil))
          (when (bolt:button "Delete" :id :delete) (delete-file path) (setf asking nil))))
  (setf asking nil))
```

`toast` is a function, not a widget: `(bolt:toast "Saved" :seconds 3)`, called anywhere in the app's function (where a button says it was pressed), shows the words at the bottom of the window, in the middle, for `:seconds` (3), the newer under the older; a press on one puts it away. While a toast shows, the frames ask for the next. In the semantic tree it is a `:toast`, and VoiceOver reads it out as it comes.

A `combo-box` is a text field with a menu of suggestions under it: `(bolt:combo-box text suggestions :id :city)`, where SUGGESTIONS are the strings the app finds for the text, each frame. Typing opens them; Down and Up move into them, Return or a press takes one (its text becomes the field's), and Escape closes them; the space bar types. It returns the text, whether it changed, and whether it changed because a suggestion was taken. It is a `:combo-box` in the tree and to VoiceOver (`AXComboBox`), and `set-value` types into it as into a field.

`open-file-dialog` (`:id :show :types`) and `save-file-dialog` (`:id :show :types :name`) ask the person for a file with the system's own Open and Save panels. They are called every frame, as widgets are; on a frame `:show` is true (pass what a button returned), the panel is shown, after the frame, and the call returns the path chosen on the frame it comes back, once, and otherwise NIL (as after Cancel). `:types` are the extensions allowed, without the dot; `:name` is the name a Save panel offers. While a panel is open the window still does what the REPL and an agent give it.

```lisp
(let ((path (bolt:open-file-dialog :id :open :types '("txt" "md")
                                   :show (bolt:button "Open..." :id :open-button))))
  (when path (setf document (load-document path))))
```

A `text-area` is a text field of several lines: `:rows` lines tall (6 if not given) and `:width` wide (320), with the lines that are too long going on in the next. It returns the text and whether it changed; its lines end in a line feed. Return puts one in. Up and Down move a line, keeping to where across the first of them started; Command-Up and Command-Down go to the start and the end of the text, and Command-Left and Command-Right to the start and the end of the line. The other keys, the pointer, and copy, cut and paste, are a text field's (a triple-click selects the paragraph, up to and with its line feed); a pasted line break stays one (a return, or a return and a line feed, becomes a line feed). It scrolls itself to keep the caret in sight, and the wheel scrolls it. In the semantic tree it is a `:text-area` with the text as its value and `:caret`, `:selection` and `:offset` in its state; the machine interface's `type-text` and `set-value` type a line feed as Return.

Every widget also takes `:tooltip`, words that float under it when the pointer has rested on it for half a second, until the pointer leaves or something is pressed or typed. A widget with a tooltip takes the pointer even if it takes no presses (a label, a disabled button). In the semantic tree the words are the node's `:help`, and the shown tooltip is a `:tooltip` node; VoiceOver reads them as the widget's help.

Every widget also takes `:flex`, its share of the free space of its container. A widget that holds something but has no words of its own (a text field, a text area, a slider, a choice, a list, a progress bar, a canvas) takes `:labelled-by`, the id of the label that names it, which is what an agent calls it by: `(bolt:text-field name :id :name :labelled-by :name-label)`.

## Containers

Containers take their options in a list and their children after it: `(bolt:column (:gap 10 :cross-align :center) ...)`. They are `column` and `row` (`:gap :main-align :cross-align :fill :flex`), `stack` (children on top of each other), `padding` (`:all :left :top :right :bottom`), `panel` (padding with a background: `:color :radius`), `center`, `scroll`, `lazy-list`, `context-menu`, `popover` and `dialog`.

`scroll` (`:id :height :flex`) shows what is inside it, one under another, in a window that the wheel or a trackpad moves up and down, with a bar at the right edge when not all of it shows. Its `:id` is required, since the place the window is at is kept by it. The window is `:height` points tall where nothing gives it a height (the window is as tall as what is inside if there is no `:height`), and `:flex` gives it a share of a column's free space instead. It is as wide as it is allowed to be (as wide as what is inside where nothing limits it), so its bar is beside what is inside. The wheel goes first to what is inside it (a `list-box` scrolls its own rows), and what is left moves the window while the pointer is over the container or anything in it. When Tab brings the keyboard to something out of the window, the window moves as little as shows all of it and its ring. The ring round what has the keyboard is drawn past the container's edges, where what is inside is cut off. What is out of the window cannot be pressed, and is `:hidden` in the semantic tree.

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

## Themes

A theme is the colours of the widgets, their font and its size, and how round the corners of a control are. The window is drawn in `:dark` or `:light`, whichever the system shows (it changes when the system's setting does), unless the app draws in a theme of its own.

`(define-theme name (&key (base :dark)) &rest settings)` defines the theme called `name`, a keyword, like `base` with `settings`. Defining it again changes it, and an open window shows the change at once. `(make-theme &key name base &rest settings)` makes one without naming it.

`(with-theme (theme) body...)` draws what `body` describes in `theme` (a theme or its name), and a menu, popover or dialog that `body` opens. With it round the whole app, the window and its background are in that theme. A tooltip and a toast are in the window's theme.

The settings (`+theme-settings+`):

- The seeds, `:background`, `:text` and `:accent` (the colour of a button, a chosen row and the selection). A theme that gives a seed works out the colours that come from it again, and keeps its base's others; a colour given as well as its seed is what was given.
- The colours worked out from them: `:panel`, `:dim-text`, `:button-text` (the words on a button, and on the chosen row of a list or a menu), `:ring` (round what has the keyboard), `:field-border`, `:field-fill`, `:field-invalid`, `:field-disabled`, `:selection`, `:caret`, `:track`, `:thumb`, `:row-selected`, `:row-hover`, `:scrim`, `:tooltip`, `:tooltip-text`, `:button`, `:button-hover`, `:button-pressed` and `:button-disabled`.
- `:font-name`, `:font-size` (what a `label` or `paragraph` is with no `:size`) and `:radius`.

A colour is `"#rrggbb"`, `"#rrggbbaa"`, or a packed colour (`pack-color`). `(find-theme name)` is the theme, `(theme-names)` the names there are, `(theme-setting theme setting)` one of its values, and `(theme-form theme)` the `define-theme` form that defines it again. The theme designer (`apps/theme-designer`, `(almighty-bolt-theme-designer:open-designer)`) makes a theme by eye and writes its `define-theme` form. `(contrast-ratio a b)` is the WCAG contrast of two colours (text is easy to read at 4.5 or more), `(mix-colors a b fraction)` a colour between two, `(parse-color designator)` a packed colour, and `(color-hex color)` its `"#rrggbb"`.

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
| `text-area` | `AXTextArea` | |
| `combo-box` | `AXComboBox` | while its suggestions show, an `AXMenuItem` for each |
| `checkbox`, `switch` | `AXCheckBox` | |
| `spinner` | `AXBusyIndicator` | |
| `menu-button` | `AXMenuButton` | while its menu is open, an `AXMenuItem` for each row |
| `dropdown` | `AXPopUpButton` | its value is the option it shows; while its menu is open, an `AXMenuItem` for each row, which can be pressed |
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

Things are named by `:id`, or by `:role` and `:name`, and a name that fits two things is an error that says so (`ambiguous-node`). The calls are `see`, `find-node`, `click`, `double-click`, `secondary-click`, `type-text`, `press-key`, `set-value`, `select-item`, `scroll`, `wait` and `screenshot`. The tests of the 7GUIs drive their apps through it. A thing scrolled out of the window of a `scroll` is `:hidden` in its state, with bounds of no size where it went out, and `click`, `double-click` and `set-value` refuse it with a message that says to scroll first; `scroll` on the container (its role is `:scroll-area`, and its state has `:offset` and `:content-height`) brings it into sight.

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

The agent gets ten tools: `snapshot` (the screen as text, one line for each thing with its role, name, id, value, state and place), `click`, `double_click`, `secondary_click` (which opens a context menu), `type_text`, `press_key`, `set_value`, `scroll`, `wait_idle` and `screenshot` (an image). Every action answers with the screen as it is afterwards, so an agent needs one call to act and see. What cannot be done (a thing that is not there, a disabled button, two things of the same name) comes back as an error result that says why, and the agent goes on. This is what the agent reads of the counter:

```
window @0,0,222,80
  static-text "34" id=count @24,24,36,32
  button "Count" id=count-button @130,26,68,29
```

`(almighty-bolt/mcp:stop-mcp-server)` closes it and every connection. Anything on this machine that can connect to the port can work the app, which is why it is local and has to be started by a call: leave it off in an app whose buttons you would not let any program press.
