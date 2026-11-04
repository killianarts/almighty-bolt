# Reference

What the [README](../README.md) leaves out: the options of widgets and
containers, the canvas, and the machine interface and MCP server in full. Calls
that you would type are written with the `bolt:` nickname (see the README);
names in running text are bare.

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
is re-sorted.

A text field takes the keys a Mac text field does: the arrows (with Shift to
select and Option to move by a word), Home, End, Backspace, forward delete,
Command-A to select everything, Command-Z and Command-Shift-Z to undo and redo
(a run of typing, or of deleting, is one edit), and Command-C, Command-X and
Command-V to copy, cut and paste through the system's clipboard. A line break
that is pasted into it becomes a space. The pointer selects as in a Mac text
field: a press puts the caret, a drag selects, a double-click selects a word and
a triple-click all of the field, and a drag after a double- or triple-click goes
on by whole words or lines. A host made without a window (in a test or a replay)
keeps what is copied to itself, and never touches the clipboard.

A `label` or `paragraph` takes `:weight`, `:regular` (the default) or `:bold`,
and `:font`, the name of a font family (the theme's if not given), such as
`"Menlo"`: `(bolt:label "16:22" :id :candles :size 34 :weight :bold)`. A family
without a bold face is drawn regular.

A `label` or `paragraph` given `:selectable t` can be selected with the pointer,
as a text field can (a drag, a double-click for a word, a triple-click for all
of a label or a paragraph's line), and copied with Command-C; Command-A selects
all of it. A press gives it the keyboard, which it needs for those keys, but Tab
goes past it, and its selection shows only while it has the keyboard. In a
dialog it keeps the keyboard it was given. Text is not selectable unless it is
asked to be: a label in a button or a row is part of what is pressed.

A `choice` puts its options side by side; with `:direction :vertical` it puts
them one under another, as radio buttons are on a Mac. With `:style :segmented`
it is one control of joined segments, the chosen one filled, as a Mac segmented
control is: for switching between views, `(bolt:choice '("Readings" "Calendar")
view :id :view :style :segmented)`. It works as any choice does, by the pointer,
the arrows and `set-value`, and is a `:radio-group` in the semantic tree (with
`:style :segmented` in its state), as AppKit's segmented control is to
VoiceOver. A `switch` is a check box that looks like a light switch, for a
setting that takes effect at once. A `spinner` (`:size`, 18 points) is a ring of
dots that goes round, for work whose end is not known; it asks for a frame about
ten times a second while it is shown, so show it only while the work goes on.

A `dropdown` (a pop-up button) shows the option that is chosen; a press, or the
space bar, Return or an arrow with the keyboard on it, opens a menu of all of
them, with the chosen one marked. The menu floats above the screen, under the
dropdown (or over it, if there is no room under it). A row of it chooses; Up and
Down move the highlight and Return chooses it; Escape, or a press anywhere
outside the menu, closes it, and that press goes nowhere else. While the menu is
open the wheel scrolls nothing under it. `:width` makes the dropdown as wide as
that; by default it is as wide as what it shows.

A menu with more rows than fit in the window shows as many as fit, with a bar at
its right edge that says where they are among them all; a `dropdown` or
`menu-button` given `:max-rows` shows no more than that. The wheel over the menu
moves its rows, the arrow keys keep the highlighted row in view, and letters
typed with the menu open move the highlight to the first row that begins with
them (what is typed within a second is one word). A row the pointer rests on is
highlighted only when the pointer moves onto it, so the keys and the wheel are
not undone by a pointer that is still. The machine interface's `set-value` on a
dropdown reaches any of its options, typing the start of it and going down or up
to it as a person would.

A `menu-button` is a button that opens a menu of commands, of which none is
marked; it returns the one chosen, on the frame it is chosen. Its menu works as
a dropdown's does.

A `context-menu` (`:id :items`) is a container: what is in it is laid out as a
column, and a press of the secondary button anywhere on it (or of the primary
button with Control down, as a Mac takes it) opens a menu of `:items` where the
pointer is. It returns the item chosen, on the frame it is chosen. A context
menu inside another's area takes the press first.

```lisp
(let ((command (bolt:context-menu (:id :row-menu :items '("Rename" "Delete"))
                 (bolt:label (task-text task) :id :text))))
  (when (equal command "Delete") (remove-task task)))
```

A `popover` (`:id :open :anchor :placement`) floats, while the app says it is
`:open`, below (or, with `:placement :right`, beside) the widget whose id is
`:anchor`, which must be described before it in the same container. What its
body describes is inside it, one under another, and works as anywhere else. A
press outside it, or Escape, dismisses it: it returns true on that frame, and
the press goes nowhere else. Several can be open at once.

A `dialog` (`:id :open :title`) is modal: while `:open`, it is in the middle of
the window over a scrim, with `:title` over what its body describes. The pointer
reaches nothing under it, and the keyboard stays in it: it goes to the first
thing in it that can have it, and Tab goes round inside it. Escape dismisses it
(it returns true on that frame); its buttons say what they do as buttons do.

```lisp
(when (bolt:dialog (:id :confirm :open asking :title "Delete the file?")
        (bolt:row (:gap 8)
          (when (bolt:button "Cancel" :id :cancel) (setf asking nil))
          (when (bolt:button "Delete" :id :delete) (delete-file path) (setf asking nil))))
  (setf asking nil))
```

`shortcut` is a function too: `(bolt:shortcut #\s :command)` is true in the
frame that Command-S is pressed, and takes the key, so nothing described after
it sees it. The modifiers, `:command`, `:shift`, `:option` and `:control`, must
be the ones held, no more; the key is a character or a `+key-...+` code. A
widget described before it that has the keyboard takes the keys it uses first (a
text field, Command-A, -C, -V, -X and -Z), and the menu bar's (Command-H, -M, -Q
and -W) never reach the app.

The window has the menu bar of a Mac app: the application's menu (Hide, Hide
Others, Show All, Quit), Edit (Undo, Redo, Cut, Copy, Paste, Select All) and
Window (Minimize, Zoom, Close). A command of the Edit menu, chosen with the
pointer or by its shortcut, reaches the app as its shortcut's key, so it does
what that key does in whatever has the keyboard. In a Lisp you work in
(`open-app`), Quit closes every window and leaves the Lisp running.

`toast` is a function, not a widget: `(bolt:toast "Saved" :seconds 3)`, called
anywhere in the app's function (where a button says it was pressed), shows the
words at the bottom of the window, in the middle, for `:seconds` (3), the newer
under the older; a press on one puts it away. While a toast shows, the frames
ask for the next. In the semantic tree it is a `:toast`, and VoiceOver reads it
out as it comes.

A `combo-box` is a text field with a menu of suggestions under it:
`(bolt:combo-box text suggestions :id :city)`, where SUGGESTIONS are the strings
the app finds for the text, each frame. Typing opens them; Down and Up move into
them, Return or a press takes one (its text becomes the field's), and Escape
closes them; the space bar types. It returns the text, whether it changed, and
whether it changed because a suggestion was taken. It is a `:combo-box` in the
tree and to VoiceOver (`AXComboBox`), and `set-value` types into it as into a
field.

`open-file-dialog` (`:id :show :types`) and `save-file-dialog` (`:id :show
:types :name`) ask the person for a file with the system's own Open and Save
panels. They are called every frame, as widgets are; on a frame `:show` is true
(pass what a button returned), the panel is shown, after the frame, and the call
returns the path chosen on the frame it comes back, once, and otherwise NIL (as
after Cancel). `:types` are the extensions allowed, without the dot; `:name` is
the name a Save panel offers. While a panel is open the window still does what
the REPL and an agent give it.

```lisp
(let ((path (bolt:open-file-dialog :id :open :types '("txt" "md")
                                   :show (bolt:button "Open..." :id :open-button))))
  (when path (setf document (load-document path))))
```

A `text-area` is a text field of several lines: `:rows` lines tall (6 if not
given) and `:width` wide (320), with the lines that are too long going on in the
next. It returns the text and whether it changed; its lines end in a line feed.
Return puts one in. Up and Down move a line, keeping to where across the first
of them started; Command-Up and Command-Down go to the start and the end of the
text, and Command-Left and Command-Right to the start and the end of the line.
The other keys, the pointer, and copy, cut and paste, are a text field's (a
triple-click selects the paragraph, up to and with its line feed); a pasted line
break stays one (a return, or a return and a line feed, becomes a line feed). It
scrolls itself to keep the caret in sight, and the wheel scrolls it. In the
semantic tree it is a `:text-area` with the text as its value and `:caret`,
`:selection` and `:offset` in its state; the machine interface's `type-text` and
`set-value` type a line feed as Return.

Every widget also takes `:tooltip`, words that float under it when the pointer
has rested on it for half a second, until the pointer leaves or something is
pressed or typed. A widget with a tooltip takes the pointer even if it takes no
presses (a label, a disabled button). In the semantic tree the words are the
node's `:help`, and the shown tooltip is a `:tooltip` node; VoiceOver reads them
as the widget's help.

Every widget also takes `:flex`, its share of the free space of its container. A
widget that holds something but has no words of its own (a text field, a text
area, a slider, a choice, a list, a progress bar, a canvas) takes
`:labelled-by`, the id of the label that names it, which is what an agent calls
it by: `(bolt:text-field name :id :name :labelled-by :name-label)`. The label is
the one with that id nearest the widget: in its own container first, then
outward, so two panels can each have a label `:title` and a field labelled by
it.

## Containers

`spacer` (`:flex`, 1 if not given) is empty room that takes its share of the
free space of the row or column it is in: `(bolt:row (:fill t) (bolt:label
"Title" :id :title) (bolt:spacer) (bolt:button "Done" :id :done))` puts the
button at the right edge. It holds nothing, so it takes no `:id`, and it is not
in the semantic tree.

A `list-box` scrolls to show the selected row on its first frame, and whenever
the app gives a selected row other than the one it gave last; while the
selection stays the same, the list stays where the wheel put it.

Containers take their options in a list and their children after it:
`(bolt:column (:gap 10 :cross-align :center) ...)`. They are `column` and `row`
(`:gap :main-align :cross-align :fill :flex`), `stack` (children on top of each
other), `padding` (`:all :left :top :right :bottom`), `panel` (padding with a
background: `:color :radius`, and an edge, `:border-color` and `:border-width`,
1 if not given), `center`, `grid`, `pressable`, `scroll`, `lazy-list`,
`context-menu`, `popover`, `dialog` and `design-surface`.

`pressable` (`:id :tooltip :enabled :selected :name :look :color :border-color
:border-width :radius :gap`, the padding keys and the sizes below) is a
container that is pressed as a button is: what is in it is one under another,
`:gap` apart, on a background that answers the pointer, and it returns true on
the frame it is clicked, by the pointer anywhere on it or by Space or Return
with the keyboard on it. `:look :button` draws it in the theme's button colours
and what is in it in the button's text colour; `:look :plain` (the default)
draws it on `:color` (the theme's panel colour if none), a little darker or
lighter under the pointer. Given `:selected`, it is a toggle, drawn in the
theme's selected-row colour when on. What is in it that takes presses of its own
(a button, a field, selectable text) takes them first. To a screen reader it is
one button (a `:toggle-button` with `:selected` in its state, if it is a toggle)
named by `:name`, or by the words of the labels and paragraphs in it, joined by
commas; they are not things of their own, but what can be pressed or typed into
inside it is.

```lisp
(when (bolt:pressable (:id (list :day day) :all 6 :selected (= day shown))
        (bolt:label (day-number day) :id :number :size 16)
        (bolt:label (day-reading day) :id :reading :size 12))
  (setf shown day))
```

`grid` (`:columns :gap :row-gap :column-widths :row-heights` and the sizes
below) puts its children in `:columns` columns, filling each row before the
next, and stretches each to its cell. `:column-widths` is `:equal` (the default)
or a list of numbers, each column's share of the width; where nothing limits the
width, the columns are as wide as their widest child. `:row-heights` is `:auto`
(each row as tall as its tallest child), `:uniform` (every row as tall as the
tallest child of all, as the days of a month are) or a number of points. `:gap`
is between columns and `:row-gap` (`:gap` if not given) between rows. In the
semantic tree it is a `:grid`, and each child's state has its `:row` and
`:column`, from 0.

```lisp
(bolt:grid (:id :month :columns 7 :gap 4 :row-heights :uniform)
  (dolist (day days)
    (bolt:panel (:id (day-number day) :all 6)
      (bolt:label (day-name day) :id :name))))
```

`column`, `row`, `stack`, `padding`, `panel`, `center`, `grid` and `pressable`
also take a size: `:width` and `:height`, which fix it, and `:min-width`,
`:min-height`, `:max-width` and `:max-height`, which bound it on one side. They
are in points and hold within what the container's parent allows, which wins (a
container stretched by `:cross-align :stretch` is as wide as it is stretched
to). What is in a container given a greatest size and is too large for it is cut
off at its edges: `(bolt:panel (:id :day :height 100) ...)` is 100 points tall
whatever is in it.

`scroll` (`:id :height :flex`) shows what is inside it, one under another, in a
window that the wheel or a trackpad moves up and down, with a bar at the right
edge when not all of it shows. Its `:id` is required, since the place the window
is at is kept by it. The window is `:height` points tall where nothing gives it
a height (the window is as tall as what is inside if there is no `:height`), and
`:flex` gives it a share of a column's free space instead. It is as wide as it
is allowed to be (as wide as what is inside where nothing limits it), so its bar
is beside what is inside. The wheel goes first to what is inside it (a
`list-box` scrolls its own rows), and what is left moves the window while the
pointer is over the container or anything in it. When Tab brings the keyboard to
something out of the window, the window moves as little as shows all of it and
its ring. The ring round what has the keyboard is drawn past the container's
edges, where what is inside is cut off. What is out of the window cannot be
pressed, and is `:hidden` in the semantic tree.

```lisp
(bolt:scroll (:id :log :height 300)
  (dolist (line lines)
    (bolt:label (line-text line) :id (line-id line))))
```

Moving the window lays the screen out again, so a wheel tick costs what a change
of layout does: about 0.06 ms with 50 rows of a label and a button, and 2.8 ms
with 2000 rows.

`lazy-list` (`:id :key :row-height :height :width :flex`) is a `scroll` for a
long list, in which only the rows that show are described. It is written like
`dolist`, and its body is one row, `:row-height` points tall (24 if not given):

```lisp
(bolt:lazy-list (task tasks :id :tasks :key #'task-id :row-height 28 :height 300)
  (bolt:row (:gap 8)
    (bolt:checkbox (task-text task) (task-done task) :id :done)))
```

The items are a vector (a list works, but it is walked from the start to the
first row that shows). `:key` gives an item's id, which is the id of its row:
what the ui keeps for a row (a caret, the keyboard) stays with its item when the
items are reordered or one is put in above it. Without `:key` a row is known by
its index. The window is `:height` points tall where nothing gives it a height
(400 if there is no `:height`), and `:width` wide, or as wide as it is allowed
to be. A wheel tick costs the same with a million rows as with a hundred, about
0.04 ms with rows of a label and a button. Tab reaches only the rows that are
described. In the semantic tree it is a `:scroll-area` whose state also has
`:row-count` and `:first-row`, and whose children are the rows that were
described.

`design-surface` (`:id :selected :flex`) is for a designer, which draws the view being designed in
it: what is in it is drawn as it looks, one under another, and none of it works. The pointer, a press
and the keyboard do not reach it, so a button in it is not hovered or pressed and a text field in it
takes no keys. A press on it returns the id of the innermost thing with an id that the press was on,
on the frame of the press: `(let ((pressed (bolt:design-surface (:id :canvas :selected picked) (draw
view)))) (when pressed (setf picked pressed)))`. The thing whose id is `:selected` is outlined in
the theme's ring colour, and the thing the pointer is over in its dim text colour. What is in it is
in the semantic tree as it would be anywhere, so the machine interface finds it (`:within` the
surface), and a click there, or VoiceOver's press, selects it. What floats above the screen from a
widget in it (a dropdown's menu, a tooltip, a dialog) is not covered yet, nor a `canvas` with
`:autofocus`, which takes the keyboard when the window opens.

## Themes

A theme is the colours of the widgets, their font and its size, how round the
corners of a control are, and how much room there is in and round the controls.
The window is drawn in `:dark` or `:light`, whichever the system shows (it
changes when the system's setting does), unless the app draws in a theme of its
own.

`(define-theme name (&key (base :dark)) &rest settings)` defines the theme
called `name`, a keyword, like `base` with `settings`. Defining it again changes
it, and an open window shows the change at once. `(make-theme &key name base
&rest settings)` makes one without naming it.

`(with-theme (theme) body...)` draws what `body` describes in `theme` (a theme
or its name), and a menu, popover or dialog that `body` opens. With it round the
whole app, the window and its background are in that theme. A tooltip and a
toast are in the window's theme.

The settings (`+theme-settings+`):

- The seeds, `:background`, `:text` and `:accent` (the colour of a button, a
  chosen row and the selection). A theme that gives a seed works out the colours
  that come from it again, and keeps its base's others; a colour given as well
  as its seed is what was given.
- The colours worked out from them: `:panel`, `:dim-text`, `:button-text` (the
  words on a button, and on the chosen row of a list or a menu), `:ring` (round
  what has the keyboard), `:field-border`, `:field-fill`, `:field-invalid`,
  `:field-disabled`, `:selection`, `:caret`, `:track`, `:thumb`,
  `:row-selected`, `:row-hover`, `:scrim`, `:tooltip`, `:tooltip-text`,
  `:button`, `:button-hover`, `:button-pressed` and `:button-disabled`.
- `:font-name`, `:font-size` (what a `label` or `paragraph` is with no `:size`)
  and `:radius`.
- `:spacing`, the seed of the sizes: 1 is the sizes the widgets have in `:dark`
  and `:light`, 1.25 a quarter more. A theme that gives it works out the sizes
  again, except those it gives too, each its size at 1 times the spacing, to the
  nearest point.
- The sizes, in points (`+theme-sizes+`, with each at a spacing of 1):
  `:button-padding-x` and `:button-padding-y` (round the words of a button, a
  menu button and a dropdown), `:field-padding-x` and `:field-padding-y` (round
  the text of a field, a combo box and a text area), `:row-height` (a row of a
  list box or a menu), `:menu-padding`, `:popover-padding`, `:popover-gap`,
  `:dialog-padding`, `:dialog-gap`, `:option-gap` and `:option-row-gap` (between
  the options of a choice), and `:control-height` (a checkbox, a switch, an
  option), `:slider-height` and `:progress-height`. The room in a toast and a
  tooltip follows the spacing as well.

A colour is `"#rrggbb"`, `"#rrggbbaa"`, or a packed colour (`pack-color`), and
`+theme-colors+` lists the settings that are colours. Where a widget or
container takes a `:color` (a label, a paragraph, a panel, a pressable), it may
also be a colour of the theme it is drawn in: a setting, such as `:accent` or
`:dim-text`; `(:accent :alpha 0.2)`, a colour with that much of it showing; or
`(:mix :accent :background 0.8)`, the colour that far from the one to the other,
each of which may be any of these. Such a colour follows the theme: a tint given
as `(:accent :alpha 0.2)` suits a dark window and a light one, and what
`with-theme` draws. `(theme-color designator)` is the packed colour of a
designator in the theme being described in. `(find-theme name)` is the theme,
`(theme-names)` the names there are, `(theme-setting theme setting)` one of its
values, and `(theme-form theme)` the `define-theme` form that defines it again.
The theme designer (`apps/theme-designer`,
`(almighty-bolt-theme-designer:open-designer)`) makes a theme by eye and writes
its `define-theme` form. `(contrast-ratio a b)` is the WCAG contrast of two
colours (text is easy to read at 4.5 or more), `(mix-colors a b fraction)` a
colour between two, `(parse-color designator)` a packed colour, and `(color-hex
color)` its `"#rrggbb"`.

## Time

`(bolt:now)` is the time of the frame in nanoseconds, and `(bolt:ask-for-frame)`
asks for another one: a still app draws nothing and costs no CPU, and an
animation asks for the next frame itself.

## Canvas

A panel or a pressable given `:border-color` has an edge of that colour,
`:border-width` points wide (1 if not given), drawn inside it and round its
corners: `(bolt:panel (:id :today :border-color :accent :border-width 2) ...)`
rings a day without hiding its own colour. In a canvas, `push-rounded-stroke`
draws such an edge: `(push-rounded-stroke list x y width height radius
stroke-width color)`, a stroke of up to about 60 points.

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
`:cursor` gives the pointer a shape over the canvas: `:arrow`, `:text`,
`:pointing-hand`, `:crosshair`, `:open-hand`, `:closed-hand`, `:not-allowed`,
`:resize-left-right` or `:resize-up-down`. The app can pass another each frame,
as what is under the pointer changes (a pointing hand over something that can be
pressed). The 7GUIs Cells and Circles and the stress apps' editor are canvases.

The pointer is a text cursor over a text field, a text area, a combo box and
selectable text, and while a selection is dragged from one; it is an arrow
everywhere else, unless a canvas says otherwise.

## The screen as a tree

Every widget says what it is (a `button`, a `text-field`, a `list`, a
`checkbox`...), what it is called, what it holds, its state (focused, disabled,
the caret, the selected item) and where it is, and the containers hold what is
in them. The tree is made when it is asked for, from the frame that was last
drawn, so a frame costs nothing for it. A node is a property list: `(:id :name
:role :text-field :name "Name:" :value "Ada" :state (:focused t :hovered t
:caret 3) :bounds (310.0 60.0 160.0 26.0))`.

### VoiceOver

VoiceOver reads the tree through the system's accessibility API. Each widget is
an element of the window's view, with its name as the label (a static text says
its words as its value), its value, whether it is enabled, and its place on the
screen, which is worked out when it is asked for so that a window that was moved
is where it says. The widget's `:id` is its accessibility identifier. The roles
are:

| Widget               | Role                  | Inside it                                                                                                                                   |
|----------------------|-----------------------|---------------------------------------------------------------------------------------------------------------------------------------------|
| `label`, `paragraph` | `AXStaticText`        |                                                                                                                                             |
| `button`             | `AXButton`            |                                                                                                                                             |
| `pressable`          | `AXButton`            | what can be pressed or typed into inside it; its labels are its name                                                                        |
| `grid`               | `AXGroup`             | its cells, each with its `:row` and `:column` in the semantic tree                                                                          |
| `text-field`         | `AXTextField`         |                                                                                                                                             |
| `text-area`          | `AXTextArea`          |                                                                                                                                             |
| `combo-box`          | `AXComboBox`          | while its suggestions show, an `AXMenuItem` for each                                                                                        |
| `checkbox`, `switch` | `AXCheckBox`          |                                                                                                                                             |
| `spinner`            | `AXBusyIndicator`     |                                                                                                                                             |
| `menu-button`        | `AXMenuButton`        | while its menu is open, an `AXMenuItem` for each row                                                                                        |
| `dropdown`           | `AXPopUpButton`       | its value is the option it shows; while its menu is open, an `AXMenuItem` for each row, which can be pressed                                |
| `slider`             | `AXSlider`            | its value, and its least and greatest                                                                                                       |
| `progress`           | `AXProgressIndicator` | its value, from 0 to 1                                                                                                                      |
| `choice`             | `AXRadioGroup`        | an `AXRadioButton` for each option, valued 1 if it is the chosen one, with the identifier of the choice and the option's number after a dot |
| `list-box`           | `AXList`              | an `AXRow` for each row that is showing, the picked one selected, with the identifier of the list and the row's number after a dot          |
| `canvas`, `scrim`    | `AXGroup`             |                                                                                                                                             |

Containers that only lay things out (and `scroll`) are not elements, and what is
in them is taken up to their parent. An element stays the same object while its
widget is on the screen (it is found by the ids from the top, and by position
for a container with no id; a row by its number in the list, so that it is the
same when the list scrolls), so a screen reader's cursor stays on it.

A button, a check box, an option or a row can be pressed from VoiceOver; the
press is a move, a press and a release in the middle of the thing, delivered to
the window as a hand's would be, and a disabled one is not pressed. A slider can
be moved up or down by a twentieth of its range, by pressing where the value
then is. A text field can be given the keyboard, which a press on it does. The
view says which element has the keyboard and which is under a point. Once
something has asked what the window shows (VoiceOver, the Accessibility
Inspector), or while the system says that VoiceOver or Switch Control is
running, the system is told after each frame when a value changed, when elements
came or went, when one moved or changed size (so that the cursor of a screen
reader follows it), when the picked row of a list changed, and when the keyboard
moved; before that nothing is worked out until the system asks, and a window at
rest does no work. (The system's own flag for VoiceOver was found to say no
inside an app while VoiceOver was running, so it is not what this goes by
alone.) A list is read as the rows that are showing.

VoiceOver says a changed value only for the thing its cursor is on. A `label` or
`paragraph` given `:live t` is read out, by announcement, whenever its text
changes, wherever the cursor is: `(bolt:label (princ-to-string count) :id :count
:live t)`. One that has just appeared is not read out, only one that changes;
use it for a result, a count or a status, not for text that changes every frame.

Two other things use the tree, in different systems because they are for
different people.

## Windows

An app can open more windows. `(bolt:open-window 'make-preferences :title "Preferences" :width 400 :height 300)` opens one that shows the app `make-preferences` makes, and returns it; `(bolt:close-window window)` closes it as its close button would. It can be called in the app's function, where a button says it was pressed (the window opens after the frame), or from the REPL while a window is open, or before one is, when it starts the windows as `open-app` does without being the app's window. In the app's function, `(bolt:this-window)` is the window the app is in, the one whose frame is being built: `(bolt:close-window (bolt:this-window))` where a button says it was pressed closes the app's own window, and `(bolt:open-window 'make-details :beside (bolt:this-window))` opens one next to it. Outside a frame it is `nil`.

Each window's app is an app of its own, with its own `state-let`. What two windows share is the Lisp they share: a closure that the constructor makes over the first app's variables, or a special variable. A frame in which one window took input (a press, a key, the wheel) draws the others again, so each shows what the other changed; a frame that took none does not, so windows do not keep waking each other. While you are developing (`open-app`, or `run-app` with `:hot` or `:watch-systems`), compiling code makes every window's app again. Closing the app's window leaves the others open, and closing the last one stops the windows' loop; `close-app` closes the app's window only. `(almighty-bolt/machine/host-session:open-session window)` is a machine-interface session on a window that `open-window` returned.

Where a window opens is given to `open-app`, `open-window` and `run-app`. `:top`, `:left`, `:bottom` and `:right` are points from that edge of the screen's usable area (below the menu bar and beside the Dock), as CSS's absolute positioning has them: `(bolt:open-window 'make-palette :top 0 :right 0)` opens in the top right corner. When both `:left` and `:right` are given `:left` is taken, and `:top` before `:bottom`; along an axis that has neither, the window is in the middle. `:beside window` (one that `open-app` or `open-window` returned) opens it next to that window, on its `:side` (`:right`, the default, `:left`, `:above` or `:below`), `:gap` points away (12), with the edges lined up, or on the other side if there is no room. With none of these, the first window opens in the middle of the screen and each after it below and to the right of the last one opened. Wherever a window goes, it is kept on the screen. The inspector opens beside the app's window.

## Views

A view is a piece of a window, from all of it down to a row of a list, made in the GUI designer
(`apps/gui-designer`) and kept by its name, a symbol of the app's package, so that two apps' or
libraries' views do not take each other's names. `(bolt:define-view login form)` defines it; the
designer writes a file of that form when it saves a view, named after the file and in the package
given in its Package field (the file begins with that `in-package`). FORM is
`(kind id (option value ...) what-is-in-it ...)`, the kinds being `:label`, `:button`,
`:text-field`, `:column` and `:row` and the options the widgets' own: `(:row :actions (:gap 8)
(:button :sign-in (:text "Sign in")))`. A form that cannot be drawn is an error when the file is
compiled: a kind there is no widget of, an option the widget does not take or a value it cannot
have, an id two widgets have, or something inside a widget that holds nothing. A view is defined
when its file is compiled too, so the code that draws it, compiled after it, is checked against it:
put the view's file before that code in the system.

`(bolt:view login clause ...)` draws the view where it is called, wired to the code by its
clauses. Each clause is one widget of the view, by its kind and id, then what the code gives it:

- an option, by the widget's own name for it, worked out each frame: `(label :greeting :text
  (format nil "Hello ~a" name))`, `(button :sign-in :enabled (plusp (length name)))`;
- `:on-press` on a button: what a press does;
- `:value` on a text field: a place (a variable of `state-let`, say) that the field shows and
  that typing sets;
- `:contents` on a column or row: what is drawn in it, after what the view has there.

What is not wired is as the view has it. `(bolt:view (message-row :id (message-id m)) ...)` gives
the view's outermost widget that id in place of its own, so that one view can be in one container
more than once, each wired to its own; ids inside it are its own.

The wiring is checked three times. When the code is compiled, a kind, an option or a wire that does
not exist is an error, and so, if the view is defined then, is an id it does not have or a widget of
another kind; a view not defined then is a style warning, and the check is made when it is drawn.
When a view is defined again (the designer saves it), the code that draws it, as loaded, is checked
against it: what no longer fits is a style warning, which the designer shows. (The wiring of code
loaded and then compiled again without a clause is still noted until the Lisp is started again, so
this can name a clause that no code has now.) And when it is drawn, a
wiring that does not fit is an error, which a window shows over its last good frame. A view defined
again is drawn in every open window at once, with the code as it is.

A view drawn this way allocates nothing beyond what the same widgets written by hand do.

## When a frame fails

If the app's function, or the layout or painting of what it described, signals an error while a
window is open, the window shows the last frame that did not fail, with the error along its bottom,
and says the error once on the error output. The window goes on: what was shown is where it was, so
a press on it reaches the app, and a frame that lays out again is shown as any other. A layout that
fails because of what the app holds (an option it was given that the layout does not take) can be
put right that way, by a button that changes it back or an Undo. While frames fail, the machine
interface and VoiceOver see what the failing frame described, which has no places yet. In a test,
and when an app's frame is made from the REPL, the error is the caller's, as before.

## The inspector

While an app is developed (opened with `open-app`, or with `run-app` and `:hot` or `:watch-systems`), Command-Option-I in its window opens the inspector, in a window of its own beside it that closes with it; the keys again, in the app's window, close it. In an app that is delivered (`run-app` alone) the keys are the app's. Its Pick button makes the next press in the app's window pick the widget under the pointer instead of pressing it (Escape, in either window, stops), and an outline follows the pointer while it picks and stays round what it picked. It says what the picked widget is: its kind and role, its id, its name and value, where it is and how big, the theme it is drawn in, whether it is enabled, and what the tree says of its state (a check box's `:checked`, a list's items). It says it as of the app's last frame, and the pick is kept while the widget is described in the same place, through hot reloads.

It changes the picked widget as a person would, with the machine interface's actions, so the app stays in charge of what it holds: Press for a button, a menu button, a check box, a switch or a dropdown; a field and Set for a text field, a combo box, a text area or a slider (a number); and a button for each option of a choice or a dropdown and each item of a list. What it asks for is done after its frame, on a thread of its own, through a session on the app's window, and if it cannot be (the widget is disabled) the inspector says why.

It changes the theme the picked widget is drawn in: type a setting (`accent`, `row-height`, `font-size`) and what to make it, and Change the theme defines that theme again as it was given, on its own base, with that setting (`(change-theme-setting name setting value)` in `almighty-bolt/ui/theme` does the same), so everything drawn in it changes. Under it is the theme's `define-theme` form, which can be selected and copied into the app's code. A theme with no name (one `make-theme` made in the app's function) cannot be defined again, and the inspector says so. The inspector is drawn in the theme its window had when it opened, so changing `:dark` or `:light` changes the app and not the inspector. Its outlines are not in the app's semantic tree: an agent, the machine interface and VoiceOver see the app alone. `(almighty-bolt/machine:perform-actions session actions)` does what it asks for, through any session.

## The machine interface

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
things is an error that says so (`ambiguous-node`). Each call also takes
`:within`, the id of a container the thing is in, for an id that is used in more
than one container: `(m:click session :id :show :within :gregorian-search)`. The
calls are `see`, `find-node`, `click`, `double-click`, `secondary-click`,
`type-text`, `press-key`, `set-value`, `select-item`, `scroll`, `wait` and
`screenshot`. The tests of the 7GUIs drive their apps through it. A thing
scrolled out of the window of a `scroll` is `:hidden` in its state, with bounds
of no size where it went out, and `click`, `double-click` and `set-value` refuse
it with a message that says to scroll first; `scroll` on the container (its role
is `:scroll-area`, and its state has `:offset` and `:content-height`) brings it
into sight.

## The MCP server

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

The agent gets ten tools: `snapshot` (the screen as text, one line for each
thing with its role, name, id, value, state and place), `click`, `double_click`,
`secondary_click` (which opens a context menu), `type_text`, `press_key`,
`set_value`, `scroll`, `wait_idle` and `screenshot` (an image). Every action
answers with the screen as it is afterwards, so an agent needs one call to act
and see. What cannot be done (a thing that is not there, a disabled button, two
things of the same name) comes back as an error result that says why, and the
agent goes on. This is what the agent reads of the counter:

```
window @0,0,222,80
  static-text "34" id=count @24,24,36,32
  button "Count" id=count-button @130,26,68,29
```

`(almighty-bolt/mcp:stop-mcp-server)` closes it and every connection. Anything
on this machine that can connect to the port can work the app, which is why it
is local and has to be started by a call: leave it off in an app whose buttons
you would not let any program press.
