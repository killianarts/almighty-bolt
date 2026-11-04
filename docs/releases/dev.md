# almighty-bolt 0.3 release notes

*Under development*

Welcome to almighty-bolt X.Y! One paragraph on the theme of the release, then
links to the major sections: [what's new](#whats-new-in-xy),
[backwards incompatible changes](#backwards-incompatible-changes-in-xy),
[deprecations](#features-deprecated-in-xy).

See [Upgrading to X.Y](../upgrading.md) if you are moving from X.(Y-1).

<!-- highlights:start -->
## Highlights

- Area: one line a user would care about.
- Area: one line a user would care about.
<!-- highlights:end -->

## Platform and compatibility

- Supported OSes and GPU APIs.
- Supported SBCL (and Coalton) versions.

## What's new in X.Y

### The inspector

While you develop an app (`open-app`, or `run-app` with `:hot`), Command-Option-I in its window opens the inspector in a window of its own beside it. Press Pick, then a widget in the app's window, and the press picks it instead of pressing it; the inspector says what it is, and an outline in the app's window shows where.

- Its kind and role, its id, its name and value, where it is and how big, and the theme it is drawn in.
- Whether it is enabled, and what the semantic tree says of its state: whether a check box is checked, the items of a list, the options of a choice.
- While it picks, the outline follows the pointer; Escape, in either window, stops.
- It changes the picked widget's value as a hand would: Press for a button or a check box, a field and Set for a text field or a slider, a button for each option of a choice, a dropdown or a list. The app stays in charge of what it holds, and the inspector says why when something cannot be done (the widget is disabled).
- It changes a setting of the theme the picked widget is drawn in (`accent`, `row-height`, `font-size`), which defines that theme again, and shows the theme's `define-theme` form to copy into the app's code.
- It is there only while the app is developed: in an app that is delivered, Command-Option-I is the app's. It closes with the app's window, and the app's semantic tree has nothing of it.

### The GUI designer, begun

`apps/gui-designer` is the start of the GUI designer on the [roadmap](../roadmap.md): `(almighty-bolt-gui-designer:open-designer)` after loading the system `almighty-bolt-gui-designer`. It shows the widgets it knows on the left (a label, a button, a text field, a column and a row), a view made of them in the middle, and what is picked on the right.

- The view is drawn with the real widgets, in the theme, as the app will draw it, but none of them works: a press picks the widget it was on (the innermost: a press between two widgets picks the column they are in), and typing goes nowhere.
- The picked widget is outlined, and so is the one the pointer is over. The right shows its kind, its id and the value of each of its options, given or not.
- The view is data: a tree of widgets, each with its kind, its id and its options by the widget's own names, written as a form such as `(:button :sign-in (:text "Sign in"))`. For now it is one written in the designer; placing widgets from the list, changing their options and saving the view come next.

It is built on a new container, `design-surface`, which any designer can use: what is in it is drawn but does not work, the pointer and the keyboard do not reach it, and a press on it returns the id of the thing it was on. `(bolt:design-surface (:id :canvas :selected picked) ...)` outlines the thing whose id is `picked`.

### More than one window

An app can open more windows: `(bolt:open-window 'make-preferences :title "Preferences" :width 400 :height 300)`, called where a button says it was pressed (the window opens after the frame) or from the REPL, opens a window that shows the app `make-preferences` makes, and returns it for `(bolt:close-window window)`.

- Each window's app is an app of its own, with its own `state-let`. Two windows share what Lisp shares: a closure over the first app's variables, or a special variable.
- A frame in which one window took input (a press, a key, the wheel) draws the others, so a preferences window and the window it sets show the same thing. A frame that took none wakes nobody, so windows at rest stay at rest.
- While you develop (`open-app`, or `run-app` with `:hot`), compiling code makes every window's app again, not only the first.
- Closing the app's window leaves the others open; closing the last stops the windows' loop and leaves the Lisp running. `close-app` closes the app's window only, and Quit (Command-Q) closes them all: before, it closed only the window in front.
- `open-session` takes a window, for the machine interface on a window other than the app's.
- Where a window opens can be given, to `open-app`, `open-window` and `run-app`: `:top`, `:left`, `:bottom` and `:right`, points from those edges of the screen's usable area as in CSS's absolute positioning (`:top 0 :right 0` is the top right corner), or `:beside window`, with `:side` (`:right`, `:left`, `:above`, `:below`) and `:gap`. A window given neither opens below and to the right of the last one, and the first in the middle of the screen; before, every window opened in the middle, on top of the others. The inspector opens beside the app's window.
- `(bolt:this-window)`, in an app's function, is the window the app is in, as `open-window` returns it: a button can close its own window with `(bolt:close-window (bolt:this-window))`, or open another next to it with `:beside (bolt:this-window)`. Outside a frame it is `nil`.

### Spacing in a theme

A theme now says how much room there is in and round the controls, as it says their colours, font and corners. It is worked out from one number, `:spacing`, as the colours are worked out from three seeds, and any of the sizes can be given on its own as well.

```lisp
(define-theme :roomy (:base :light)
  :spacing 1.25)        ; everything a quarter roomier

(define-theme :dense (:base :dark)
  :spacing 0.8
  :row-height 20)       ; and the rows of lists and menus exactly 20 points
```

At a spacing of 1 every widget is the size it was before. Each size is its size at 1 times the spacing, to the nearest point. The sizes, in points:

- `:button-padding-x` and `:button-padding-y` (14 and 7): round the words of a button, a menu button and a dropdown.
- `:field-padding-x` and `:field-padding-y` (8 and 6): round the text of a text field, a combo box and a text area.
- `:row-height` (24): a row of a list box or a menu. `:menu-padding` (4): round the rows of a menu.
- `:popover-padding` and `:popover-gap` (12 and 8), and `:dialog-padding` and `:dialog-gap` (20 and 12): inside a popover and a dialog, and between what is in them.
- `:option-gap` and `:option-row-gap` (20 and 6): between the options of a choice, side by side and one under another.
- `:control-height` (22): a checkbox, a switch and an option of a choice. `:slider-height` (24) and `:progress-height` (14).

The room in a toast and a tooltip follows the spacing too. What a mark is (a checkbox's box, a radio dot, a slider's thumb, a switch's track) stays its own size. The gaps between the parts of the app, which it gives with `:gap` and `padding`, are the app's.

A theme that gives the spacing works out every size again, except those it gives as well; a theme that does not give it keeps its base's. `theme-form` writes the spacing and the sizes down as numbers, and `+theme-sizes+` lists the sizes with what each is at a spacing of 1. The theme designer has a Spacing slider, from 50% to 200%.

### Minor features

#### Windowing

- [`symbol`](../reference/page.md#symbol) now does X.

#### Rendering

- A `panel` or a `pressable` can have an edge: `:border-color` (any colour, a theme's too) and `:border-width`, drawn inside it, round its corners, antialiased as its fill is. It shows over a translucent fill, as a larger rectangle behind a smaller one would not: a calendar's today can be ringed in the accent colour and keep its own tint. The renderer draws it in the same pass as rectangles and glyphs; `push-rounded-stroke` puts one in a canvas.

- A `:color` can be a colour of the theme that the thing is drawn in, so that an app's tints suit a dark window and a light one without a colour for each: a setting (`:accent`, `:dim-text`, `:panel` ...), `(:accent :alpha 0.2)`, or `(:mix :accent :background 0.8)`. Labels, paragraphs, panels and pressables take them, and `"#rrggbb"` strings as well as packed colours. `(theme-color designator)` gives the packed colour.

#### Text

- `label` and `paragraph` take `:weight` (`:regular` or `:bold`) and `:font`, a font family: a time or a heading can be bold at the size of what is round it, and a number can be in `"Menlo"`. Bold is the family's bold face, made by Core Text; a family that has none is drawn regular. A paragraph is broken into lines again when its weight or font changes.

#### Layout

- `pressable` is a container that is pressed as a button is, so that a card, a calendar day or a row of two lines is one thing to press and not a button among labels. It returns true on the frame it is clicked, by the pointer anywhere on it or by Space or Return. It looks like a button (`:look :button`, with what is in it in the button's text colour) or like a panel that answers the pointer (`:look :plain`), and with `:selected` it is a toggle. A screen reader reads it as one button named by the words in it; a button or a field inside it is still pressed on its own.

- `grid` lays its children out in `:columns` columns, a row at a time, each stretched to its cell. The columns are equal or take shares of the width (`:column-widths '(1 2 1)`); the rows are each as tall as their tallest child (`:auto`), all as tall as the tallest child of all (`:uniform`, for a month of days whose cells should be one size), or a given height. In the semantic tree it is a `:grid`, and each cell says its `:row` and `:column`.

- A container can be given a size. `column`, `row`, `stack`, `padding`, `panel` and `center` take `:width` and `:height`, which fix it, and `:min-width`, `:min-height`, `:max-width` and `:max-height`, which bound one side. A paragraph in a container with a greatest width is broken to that width, and what is too large for a container with a greatest size is cut off at its edges, as in a scroll container. The limits hold within what the parent allows: a parent's limits win. Before, only a `scroll` took a height.

- `(spacer)` is empty room that takes the free space of the row or column it is in, so that what comes after it goes to the far edge: `(row (:fill t) (label "Title" :id :title) (spacer) (button "Done" :id :done))`. `:flex` gives it more than one share. It needs no id, since it holds nothing, and it is not in the semantic tree. It replaces an empty `(row (:id :gap :flex 1))`.

#### Input

- `choice` takes `:style :segmented`, which draws its options as one control of joined segments with the chosen one filled, as a Mac segmented control is: the usual look for switching between two or three views, which radio buttons are not. It is chosen as any choice is, and is a radio group in the semantic tree and to VoiceOver. (Tabs are still on the roadmap.)

- A menu longer than the window shows as many rows as fit, with a bar that says where they are among them all, and scrolls: by the wheel, and by the arrow keys, which keep the highlighted row in view. `dropdown` and `menu-button` take `:max-rows`. Letters typed while a menu is open move the highlight to the first row they begin, as in a Mac pop-up button. Before, a dropdown of sixty countries opened a menu that ran past the bottom of the window. A row is highlighted by the pointer only when the pointer moves onto it, so a still pointer no longer takes the highlight back from the keys.

- A `list-box` whose selected row is out of its window shows it: on its first frame, and when the app selects a row other than the one it selected before (say, from a search field beside it). While the selection stays the same, the list stays where the wheel put it. Before, a list opened at its top whatever was selected.

- `(shortcut #\s :command)` is true in the frame its keys are pressed, for the app's own keyboard shortcuts: `(when (bolt:shortcut #\s :command) (save))`. It takes the key, and the modifiers held must be the ones given.
- `canvas` takes `:cursor`, the shape the pointer has over it: a crosshair to draw with, a pointing hand over something that can be pressed, a hand to drag with, arrows to resize. The app can give another each frame.

#### Machine interface

- `make-session` takes `:app`, a function that returns the app instance the window runs, where it took `:variant`, and `session-app` is what `session-variant` was. The library's internal names follow: what a window runs is an app instance (`almighty-bolt/app-instance`, `immediate-app`, `host-app`), not a variant.
- Every call that names a thing (`find-node`, `click`, `set-value` and the rest) takes `:within`, the id of a container it is inside. Ids need be unique only within a container, so `(m:click session :id :show :within :gregorian-search)` presses the Show button of one panel of two that each have one. A name that fits more than one thing says to use it. The MCP server's tools take `within` as well.

#### Examples

- The 7GUIs tasks and the stress apps no longer carry the name of the immediate "variant", from when the project compared three kinds of ui. The make targets take `TASK=` alone (`make 7guis TASK=counter`, `make 7guis-smoke TASK=counter`, `make snapshots`, `make replay SCRIPT=…`), `make image` writes `build/apps`, each task's app is in `examples/7guis/<task>/app.lisp` (and `bench/stress/<app>/app.lisp`), and the systems are `almighty-bolt-7guis/apps` and `almighty-bolt-stress/apps`. Replay scripts no longer write `:variant :immediate` in their header; old scripts still play. Benchmark records are version 3, without `:variant`.
- The examples that draw in a canvas (the cells' grid, the circles and the stress apps' rows, editor and chart) import only from the public `almighty-bolt` package, as a program that uses the library does; they took the same functions from `almighty-bolt/internals` before. What they draw with is all public: `push-rect`, `context-push-text`, the key constants and the rest.
- The gallery shows how each widget, container and function in it is called, under it: its arguments and options with their defaults, as its lambda list has them, in a fixed-width face that can be selected and copied. It also shows the containers it did not have before, `grid`, `pressable` and `spacer`, and a segmented `choice`.

## Backwards incompatible changes in X.Y

### `context-font` takes a weight

`context-font` is now `(context-font context name size &optional weight)`, where the weight is `:regular` (or NIL) or `:bold`. A method of it for a context of your own (a test double, say) must take the optional argument, or SBCL says its lambda list does not agree with the generic function's:

```lisp
(defmethod context-font ((context my-context) name size &optional weight)
  (my-font name size (or weight :regular)))
```

### Area

Before:

```lisp
(old-code)
```

After:

```lisp
(new-code)
```

What to do, and why the change was made.

## Features deprecated in X.Y

- `old-symbol` is deprecated. Use [`new-symbol`](../reference/page.md#new-symbol)
  instead. It will be removed in X.(Y+2).

## Features removed in X.Y

These features have reached the end of their deprecation cycle. See the
[X.(Y-2) release notes](X.(Y-2).md) for how to migrate.

## Bug fixes

- Text fields, combo boxes and text areas could not undo. Now Command-Z undoes an edit and Command-Shift-Z does it again. A run of typing is undone as one, and so is a run of deleting; a paste, a cut or a Return is one of its own, and a press or a key that moves the caret ends a run. When the app gives the field text of its own, what was done before can no longer be undone into it.
- An app whose whole interface was a dialog (or anything else that floats), with nothing described under it, showed an empty window. It now shows the dialog over an empty screen.
- The pointer was an arrow everywhere. It is now a text cursor over text fields, text areas, combo boxes and selectable text, as in other Mac apps.
- The menu bar had the application's menu with Quit only. It now has the menus every Mac app has: Hide, Hide Others and Show All beside Quit; Edit, with Undo, Redo, Cut, Copy, Paste and Select All, which do what their keys do in whatever has the keyboard; and Window, with Minimize, Zoom and Close.
- A key pressed with Command and Option was what Option makes of the key (Option-I is the dead key of the circumflex, and types nothing), so a `shortcut` with `:option` never came. With Command held, the key is now the key itself.
- A screenshot taken through a session opened as soon as a window was there (in `run-app`'s `:driver`, say) was a PNG one pixel square, since the window had not yet made a frame or been given its size. `open-session` now returns once it has.
- `:labelled-by` named a field after the first widget in the window with that id, though ids need be unique only within a container: of two panels that each had a label `:title` and a field labelled by it, both fields were called by the first panel's label. A field is now named by the label nearest it: among what is in its own container first, then outward.
- A panel given a transparent `:color` (`"#00000000"`, or `(pack-color 0 0 0 0)`) was drawn in the theme's panel colour, since 0 stood for "the theme's". It now draws nothing, and a panel with no `:color` is the theme's. The same holds for a label's or a paragraph's `:color`.
- A slider whose value the app keeps rounded (a whole number, say) was moved again in every build of a frame while the pointer held it, so the frame did not settle and a warning was printed, every frame of a drag. It now slides only when the pointer has moved, and shows the value the app keeps. The theme designer's Size, Corners and Spacing sliders did this.
- The wheel over a list, a lazy list, a scroll container or a text area inside a scroll container went to it even when it could move no further that way, so a page scrolled up to a list at its top stopped there. Now what cannot move the way the wheel goes leaves it to the container it is in: the page goes on up past the list, and over a text area whose lines all show the page scrolls. A menu or a popover still keeps the wheel from what is under it.
