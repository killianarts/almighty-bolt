# almighty-bolt 0.2 release notes

*Under development*

Welcome to almighty-bolt 0.2, in development. A window of almighty-bolt can be read by VoiceOver; it is dark or light as the system is, or in a theme of your own; it scrolls, in a container or a list that builds only the rows that show; text areas and copy and paste are there for everyday text; a switch, radio buttons and a spinner join the widgets; and dropdowns, menus, context menus, tooltips, popovers, dialogs, toasts, a combo box and the system's file panels float above the screen. See [what's new](#whats-new-in-02).

<!-- highlights:start -->
## Highlights

- Accessibility: VoiceOver reads every widget by what it is, what it is called and what it holds, can press buttons, check boxes, options and rows, move a slider and give a field the keyboard, is told when things change, and reads out a `:live` label or paragraph whenever its text changes.
- Overlays: a layer above the screen, with dropdowns, menu buttons, context menus, tooltips on every widget, popovers, modal dialogs, toasts and a combo box on it, and the system's Open and Save panels.
- Themes: dark and light, following the system, and themes of your own, made from three colours, for a window or a part of one, and a theme designer to make them by eye.
- Scrolling: `scroll` for anything, and `lazy-list`, which costs the same with a million rows as with a hundred.
- Everyday text: `text-area`, and copy, cut and paste in text fields.
- More widgets: `switch`, radio buttons (`choice :direction :vertical`) and `spinner`.
<!-- highlights:end -->

## Platform and compatibility

- macOS on Apple silicon, with Metal. SBCL 2.6. One window, one fixed dark theme.
- Assistive tools are told what changes once they have asked what the window shows (or the system says VoiceOver or Switch Control is running).

## What's new in 0.2

### VoiceOver reads the screen

A window of almighty-bolt draws all of itself in one view, so to the system it was one blank rectangle. It now hands out what the [semantic tree](../../README.md#working-an-app-from-a-program-or-an-agent) says: each widget is an element with a role, a name, a value, an enabled state and a place on the screen, found again as the same element from frame to frame (so that VoiceOver's cursor stays where it was). The containers that only lay things out are left out.

| Widget | Is read as |
|---|---|
| label, paragraph | static text, saying its words |
| button | button |
| text field | text field, holding its text |
| text area | text area, holding its text |
| check box, switch | check box, checked or not |
| spinner | busy indicator |
| menu button | menu button; while it is open, a menu item for each row |
| combo box | combo box, holding its text; while its suggestions show, a menu item for each |
| dropdown | pop-up button, saying the option it shows; while it is open, a menu item for each row |
| slider | slider, with its value and its ends |
| progress bar | progress indicator, from 0 to 1 |
| choice | radio group, with a radio button for each option and the chosen one marked |
| list | list, with a row for each row that is showing and the picked one marked |
| canvas, scrim | group |

What VoiceOver can do with them:

- Read each one by its role, its name and its value: a button called Count, a text field holding Ada, a check box called Read the README that is checked.
- Press a button, a check box, an option of a choice or a row of a list (VO-Space). The press arrives as the events a hand makes (a move, a press and a release in the middle of the thing), so the app answers as it does to a click. A disabled one refuses.
- Move a slider up or down a step, the step of an arrow key (a twentieth of its range), by pressing where the value then is.
- Ask for the keyboard in a text field. Typing then goes to it.
- Find what is under the pointer, and know which thing has the keyboard.
- Be told, as they happen, when a value changes, when things come or go (a list that scrolls shows other rows), when a thing moves or changes size (VoiceOver draws its cursor where it was last told a thing is, so a button pushed along by a count that grew a digit keeps its cursor with it), when the picked row of a list changes, and when the keyboard moves. VoiceOver says a changed value only for the thing its cursor is on.
- Hear a `:live` label or paragraph whenever its text changes, wherever the cursor is.

Nothing is worked out for this until something asks what the window shows (VoiceOver, the Accessibility Inspector) or the system says that VoiceOver or Switch Control is running; from the first question on, what changes is told as it happens. A window that nothing has asked about costs what it did, and one at rest does no work.

A count that changes when a button elsewhere is pressed is not said by VoiceOver, since its cursor is on the button. Say that the text matters with `:live`: `(bolt:label (princ-to-string count) :id :count :live t)` is read out as the new count after each press. It is for a result, a count or a status that a person should hear without going to look for it, not for text that changes every frame. A live text that has just appeared is not read out, only one that changes, so a status that comes and goes is better as a label that is always there and sometimes empty.

A list is read as the rows that are showing, so that scrolling it changes which ones there are; a screen reader is not told of the rows out of sight. What a canvas draws is not read: it is a group, called by its label if it has one.

### Themes

```lisp
(bolt:define-theme :sea () :accent "#20a090")

(bolt:with-theme (:sea)
  (bolt:button "Dive" :id :dive))
```

- A window is dark or light as the system is, and changes when the system's setting does.
- `define-theme` makes a theme from a base (`:dark` if not given) and what is different. Give a seed (`:background`, `:text` or `:accent`) and the colours that come from it are worked out again: hover, pressed, the ring, the selection and the rest. Any of those can be given too.
- `with-theme` draws a part of the screen in a theme, and the menus, popovers and dialogs it opens.
- A theme sets the font and its size, which labels and paragraphs take when they are given no `:size`, and how round the corners of a control are.
- Compile a `define-theme` again and the open window shows it at once.
- `theme-form` writes a theme down as a `define-theme` form, and `contrast-ratio` says how readable text in one colour is on another.
- The words of a disabled button are dimmed, as those of a disabled dropdown and menu button were.

A theme designer, in [`apps/theme-designer/`](../../apps/theme-designer), makes one by eye: a theme starts as the dark or the light one, and its seeds are picked by hue, saturation and lightness or typed in. Any worked-out colour can be given instead, and the font, its size and the corners set. Some of every kind of widget is shown in the theme as it changes, how readable each kind of text is is worked out (WCAG's contrast), and the theme is written down as a `define-theme` form, to copy or to save to a file. `(almighty-bolt-theme-designer:open-designer)` opens it.

See [Themes](../reference.md#themes).

### A scroll container

`scroll` shows what is inside it, one under another, in a window that the wheel or a trackpad moves up and down:

```lisp
(bolt:scroll (:id :log :height 300)
  (dolist (line lines)
    (bolt:label (line-text line) :id (line-id line))))
```

- The window is `:height` points tall, or takes a share of a column's free space with `:flex`. It is as wide as it is allowed to be, with its bar at the right edge. The `:id` is required: the place the window is at is kept by it, across frames and across a reload.
- The wheel goes first to what is inside (a `list-box` in a `scroll` scrolls its own rows), and what is left of it moves the window while the pointer is over the container or anything in it.
- Tab brings the keyboard to something out of the window, and the window moves to show it, with room for the ring round it. What is inside goes right up to the edges, and the ring round what has the keyboard shows past them (the same in a `lazy-list`).
- What is out of the window cannot be pressed. In the semantic tree it is `:hidden`, and the machine interface refuses to click it until it has been scrolled into sight.

Moving the window lays the screen out again: a wheel tick costs about 0.06 ms with 50 rows of a label and a button, and 2.8 ms with 2000. For long lists there is `lazy-list`, below.

VoiceOver does not yet scroll a `scroll` to the thing its cursor is on. (What it makes of the things out of the window has not been tried.)

### A list that builds only the rows that show

`lazy-list` is a scrolling list whose rows are any widgets, written like `dolist`. Its rows are all one height, so it knows which of them show before it builds any, and it describes only those: a wheel tick costs about 0.04 ms whether the list has a hundred rows or a million.

```lisp
(bolt:lazy-list (task tasks :id :tasks :key #'task-id :row-height 28 :height 300)
  (bolt:row (:gap 8)
    (bolt:checkbox (task-text task) (task-done task) :id :done)))
```

- `:key` gives each item's id. What the ui keeps for a row (the caret of a field in it, the keyboard) stays with its item when the items are sorted again or one is put in above it. Without `:key`, a row is known by its index.
- The items are best a vector: a row is found by its index.
- The wheel, the bar at the edge, and what cannot be pressed out of the window are as for `scroll`. Tab reaches only the rows that are described.

### The system's file panels

```lisp
(let ((path (bolt:open-file-dialog :id :open :types '("txt" "md")
                                   :show (bolt:button "Open..." :id :open-button))))
  (when path (setf document (load-document path))))
```

- `open-file-dialog` and `save-file-dialog` show macOS's own Open and Save panels. They are called every frame as widgets are, and on the frame `:show` is true the panel is shown.
- The path chosen is returned once, on the frame it comes back; after Cancel, nothing is.
- `:types` limits the extensions; `:name` is what a Save panel offers.
- While a panel is open, what the REPL and an agent give the window still runs.

### A combo box

```lisp
(multiple-value-bind (text changed taken)
    (bolt:combo-box city (cities-that-fit city) :id :city)
  (when changed (setf city text)))
```

- A text field with a menu of suggestions under it, which the app gives for the text each frame. Typing opens them.
- Down and Up move into them, Return or a press takes one, and Escape closes them. The space bar types.
- It returns the text, whether it changed, and whether it changed because a suggestion was taken.

### Toasts

```lisp
(when (bolt:button "Save" :id :save)
  (save-document)
  (bolt:toast "Saved"))
```

- `toast` shows words at the bottom of the window, in the middle, for three seconds (`:seconds`). Several stack, the newer under the older, and a press puts one away.
- It can be called anywhere in the app's function. While a toast shows, the frames ask for the next, so that it goes on time.
- VoiceOver reads a toast out as it comes, wherever its cursor is.

### Popovers and dialogs

```lisp
(when (bolt:button "Details" :id :details) (setf details t))
(when (bolt:popover (:id :more :open details :anchor :details)
        (bolt:label "More about it" :id :about))
  (setf details nil))

(when (bolt:dialog (:id :confirm :open asking :title "Delete the file?")
        (when (bolt:button "Delete" :id :delete) (delete-it) (setf asking nil)))
  (setf asking nil))
```

- Both are containers that show while the app says they are `:open`, and return true on the frame the person dismisses them.
- A `popover` floats below (or `:placement :right`, beside) the widget named by `:anchor`, an id described before it in the same container. A press outside it or Escape dismisses it, and the press goes nowhere else. Several can be open at once.
- A `dialog` is modal: in the middle of the window, over a scrim that keeps the pointer from what is under it. The keyboard goes into it and Tab stays inside it. Escape dismisses it.

### Tooltips

Every widget takes `:tooltip`:

```lisp
(bolt:button "Save" :id :save :tooltip "Save the document (Command-S)")
```

- The words float under the widget when the pointer has rested on it for half a second, and go when the pointer leaves or something is pressed or typed.
- A label, a disabled button, anything with a tooltip takes the pointer, so that it can be rested on.
- While a tooltip waits to come, the frames ask for the next one, so that it comes on time.
- The words are the node's `:help` in the semantic tree, and the widget's help to VoiceOver.

### A menu button and context menus

```lisp
(let ((command (bolt:menu-button "Actions" '("Rename" "Delete") :id :actions)))
  (when (equal command "Delete") (delete-it)))

(let ((command (bolt:context-menu (:id :row-menu :items '("Rename" "Delete"))
                 (bolt:label (task-text task) :id :text))))
  (when (equal command "Delete") (remove-task task)))
```

- `menu-button` is a button that opens a menu of commands. It returns the command chosen, on the frame it is chosen.
- `context-menu` is a container. A press of the secondary button anywhere on what it holds opens its menu where the pointer is, and it returns the item chosen. A context menu inside another's area takes the press first.
- A press of the primary button with Control down is now a secondary press everywhere, as on a Mac: it opens a context menu, and a canvas gets it as a press of button 1.
- The machine interface has `secondary-click`, and the MCP server `secondary_click`, to open a context menu.
- VoiceOver reads a menu button as a menu button and can press it and the rows of its menu. A context menu is not yet offered to VoiceOver (as the action a screen reader shows a menu with).

### A dropdown

`dropdown` is a pop-up button: it shows the chosen option, and a press opens a menu of all of them. It returns what `choice` does.

```lisp
(multiple-value-bind (index changed) (bolt:dropdown '("Red" "Green" "Blue") colour :id :colour)
  (when changed (setf colour index)))
```

- The menu floats above everything, under the dropdown, or over it where there is no room under it. It is as wide as the dropdown at least, and marks the chosen option.
- A row chooses. With the keyboard, the space bar, Return or an arrow opens it, Up and Down move the highlight, Return chooses, and Escape closes.
- A press outside the menu closes it and goes nowhere else, as on a Mac; nothing under an open menu scrolls.
- The semantic tree has the dropdown as a `:pop-up-button` (its value the chosen option, and `:expanded` while open) and the open menu as a `:menu` of `:menu-item`s after the rest of the screen. `set-value` opens it and presses the row. VoiceOver reads it as a pop-up button and can press it and its rows.

### A switch, radio buttons and a spinner

```lisp
(multiple-value-bind (on changed) (bolt:switch "Wi-Fi" wifi :id :wifi)
  (when changed (setf wifi on)))
(bolt:choice '("Standard" "Express") delivery :id :delivery :direction :vertical)
(when loading (bolt:spinner :id :loading))
```

- `switch` is a check box that looks like a light switch, for a setting that takes effect at once. It returns what `checkbox` does.
- `choice` takes `:direction :vertical`, which puts its options one under another: radio buttons, as a Mac lays them out. Arrow keys move the choice either way.
- `spinner` is a ring of dots that goes round, for work whose end is not known. It asks for about ten frames a second while it is shown, so show it only while the work goes on.

### A text area

`text-area` is a text field of several lines. It is called as `text-field` is, and returns the text and whether the person changed it:

```lisp
(multiple-value-bind (text changed) (bolt:text-area notes :id :notes :width 320 :rows 6)
  (when changed (setf notes text)))
```

- Return puts in a line feed. A line too long for the width goes on in the next.
- Up and Down move a line, keeping to where across the first of them started. Command-Up and Command-Down go to the start and the end of the text, Command-Left and Command-Right to the start and the end of the line. The arrows with Shift select.
- Copy, cut and paste work as in a text field, and a pasted line break stays a line break.
- It scrolls itself to keep the caret in sight, and the wheel scrolls it.
- VoiceOver reads it as a text area holding its text. The machine interface types a line feed as Return.

### Minor features

#### Text fields

- Command-C, Command-X and Command-V copy, cut and paste in a text field, through the system's clipboard. A line break that is pasted becomes a space, since the field holds one line. Copy and cut do nothing when nothing is selected.

#### Examples

- `almighty-bolt-examples/gallery:make-gallery` puts every widget and container on one page, each with what it returns shown under it, for trying them and for showing them. The tests check that it has one of each, so a new widget is added to it.

#### Machine interface

- `type-text` and `set-value` type a line feed in the text as Return, the key a person presses for it. In a text area that starts a new line; in a text field it submits the field (before, a line feed was dropped).
- `click`, `double-click` and `set-value` refuse a thing that is scrolled out of the window of a `scroll` or a `lazy-list` (it is `:hidden` in the tree), with a message that says to scroll first. A thing partly out of the window is pressed in the middle of the part that shows.

- A widget's `:id` is its element's accessibility identifier, so the id that an agent or a test finds a widget by is what the Accessibility Inspector shows for it.
